# 01_fetch.R — ingest How's Life? data straight from the OECD SDMX REST API.
#
# The whole point of this file: no human ever downloads a CSV by hand. Every
# snapshot is stamped with the URL it came from, when it was taken and a hash
# of the bytes, so any figure in any output can be traced back to a specific
# retrieval from the source of record.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(glue)
  library(cli)
})

SDMX_BASE <- "https://sdmx.oecd.org/public/rest/data"

# The WISE well-being dataflows. Agency + dataflow id + version, exactly as
# they appear in the OECD Data Explorer URL.
HSL_FLOWS <- list(
  cwb = list(
    id    = "OECD.WISE.WDP,DSD_HSL@DF_HSL_CWB,1.1",
    label = "How's Life? Current well-being"
  )
)

#' Build the SDMX-REST data query URL for a dataflow.
#'
#' `key = "all"` returns every series. `dimensionAtObservation=AllDimensions`
#' gives a flat observation-per-row table, which is what we want for a tidy
#' pipeline. `csvfilewithlabels` returns both the codes and their English
#' labels, so we never have to maintain our own codelists.
hsl_url <- function(flow = "cwb", key = "all", start_period = NULL) {
  f <- HSL_FLOWS[[flow]]
  if (is.null(f)) cli_abort("Unknown dataflow {.val {flow}}.")
  q <- c(
    "dimensionAtObservation=AllDimensions",
    "format=csvfilewithlabels",
    if (!is.null(start_period)) glue("startPeriod={start_period}")
  )
  glue("{SDMX_BASE}/{f$id}/{key}?{paste(q, collapse = '&')}")
}

#' Fetch a dataflow and write a vintage-stamped snapshot.
#'
#' Returns the manifest invisibly. Snapshots are named by UTC date so a daily
#' or quarterly schedule produces one file per run and never silently
#' overwrites the previous vintage — that is what makes `hsl_diff()` possible.
hsl_fetch <- function(flow = "cwb",
                      start_period = NULL,
                      dir = "data/raw",
                      vintage = format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
                      timeout = 300) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  url  <- hsl_url(flow, start_period = start_period)
  dest <- file.path(dir, glue("hsl_{flow}_{vintage}.csv"))

  cli_alert_info("Fetching {.val {flow}} from the OECD SDMX API")
  t0 <- Sys.time()
  # curl is used rather than download.file() because the OECD endpoint is slow
  # to first byte on large queries and we want an explicit, generous timeout.
  status <- utils::download.file(url, dest, quiet = TRUE, mode = "wb",
                                 method = "libcurl", cacheOK = FALSE)
  if (status != 0 || !file.exists(dest)) {
    cli_abort("Download failed for {.url {url}}")
  }

  n_rows <- length(readr::read_lines(dest)) - 1L
  manifest <- tibble::tibble(
    flow         = flow,
    label        = HSL_FLOWS[[flow]]$label,
    vintage      = vintage,
    fetched_utc  = format(as.POSIXct(t0, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    seconds      = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
    url          = url,
    path         = dest,
    bytes        = file.size(dest),
    sha256       = as.character(openssl::sha256(file(dest))),
    n_rows       = n_rows
  )

  mpath <- file.path(dir, "manifest.csv")
  readr::write_csv(manifest, mpath,
                   append = file.exists(mpath), col_names = !file.exists(mpath))

  cli_alert_success("{n_rows} observations -> {.path {dest}} ({manifest$seconds}s)")
  invisible(manifest)
}

#' Read a snapshot into a tidy, typed table.
#'
#' The published CSV carries both `CODE` and `Label` columns for every
#' dimension. We keep the codes as the data and the labels as a lookup, so
#' nothing downstream depends on an English label string that WISE is free to
#' reword between releases.
hsl_read <- function(path) {
  raw <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                         progress = FALSE)

  obs <- raw %>%
    transmute(
      ref_area      = REF_AREA,
      measure       = MEASURE,
      unit_measure  = UNIT_MEASURE,
      age           = AGE,
      sex           = SEX,
      education_lev = EDUCATION_LEV,
      domain        = DOMAIN,
      time_period   = suppressWarnings(as.integer(TIME_PERIOD)),
      obs_value     = suppressWarnings(as.numeric(OBS_VALUE)),
      obs_status    = OBS_STATUS
    )

  # One row per code, carrying its published English label.
  labels <- bind_rows(
    distinct(raw, dim = "ref_area",     code = REF_AREA,       label = `Reference area`),
    distinct(raw, dim = "measure",      code = MEASURE,        label = Measure),
    distinct(raw, dim = "unit_measure", code = UNIT_MEASURE,   label = `Unit of measure`),
    distinct(raw, dim = "domain",       code = DOMAIN,         label = Domain),
    distinct(raw, dim = "obs_status",   code = OBS_STATUS,     label = `Observation status`)
  ) %>%
    filter(!is.na(code)) %>%
    distinct(dim, code, .keep_all = TRUE)

  structure(obs, labels = labels, source = path, class = c("hsl_tbl", class(obs)))
}

#' The key that uniquely identifies an observation in this dataflow.
HSL_KEY <- c("ref_area", "measure", "unit_measure",
             "age", "sex", "education_lev", "time_period")
