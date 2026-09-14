# 04_fetch_fua.R — ingest the OECD city-level (functional urban area) dataflows.
#
# These live in a different directorate (CFE, Centre for Entrepreneurship, SMEs,
# Regions and Cities) from the How's Life? database (WISE). Same SDMX API, same
# manifest discipline: every file is stamped with its URL, retrieval time and
# SHA-256 so any number downstream traces back to a specific retrieval.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(glue)
  library(cli)
})

CFE_AGENCY <- "OECD.CFE.EDS"

# The dataflows used by the environmental well-being analysis, keyed by the
# short id used in file names.
FUA_FLOWS <- c(
  DF_HEAT_STRESS = "DSD_FUA_CLIM@DF_HEAT_STRESS",  # UTCI heat-stress days
  DF_UHI         = "DSD_FUA_ENV@DF_UHI",           # urban heat island, degC
  DF_GREEN_AREA  = "DSD_FUA_ENV@DF_GREEN_AREA",    # green area, m2/person
  DF_AGE_SEX     = "DSD_FUA_DEMO@DF_AGE_SEX",      # population by age & sex
  DF_CDD_HDD     = "DSD_FUA_ENER@DF_CDD_HDD"       # cooling/heating degree days
)

fua_url <- function(flow_id) {
  glue("{SDMX_BASE}/{CFE_AGENCY},{flow_id},/all",
       "?dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
}

#' Fetch one CFE dataflow into data/raw/fua/<short>.csv, appending a manifest row.
#'
#' Unlike the How's Life? snapshots, these are not vintage-stamped: they are
#' inputs to an analysis rather than the object being change-tracked, and some
#' are large (the degree-day file is ~200 MB). `refresh = FALSE` reuses a file
#' already on disk, which is what you want when iterating locally.
fua_fetch <- function(short, dir = "data/raw/fua", refresh = FALSE, timeout = 900) {
  flow_id <- FUA_FLOWS[[short]]
  if (is.null(flow_id)) cli_abort("Unknown FUA dataflow {.val {short}}.")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, glue("{short}.csv"))

  mpath <- file.path(dir, "manifest.csv")
  if (file.exists(dest) && !refresh) {
    cli_alert_info("{short}: using existing {.path {dest}}")
    # A file that arrived outside this function still gets a provenance row,
    # so nothing downstream is built on an unrecorded input.
    known <- if (file.exists(mpath)) read_csv(mpath, show_col_types = FALSE)$flow else character(0)
    if (!short %in% known) {
      row <- tibble::tibble(
        flow = short, dataflow = glue("{CFE_AGENCY},{flow_id}"),
        fetched_utc = format(as.POSIXct(file.info(dest)$mtime, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
        seconds = NA_real_, url = fua_url(flow_id), path = dest,
        bytes = file.size(dest), sha256 = as.character(openssl::sha256(file(dest)))
      )
      write_csv(row, mpath, append = file.exists(mpath), col_names = !file.exists(mpath))
    }
    return(invisible(dest))
  }

  url <- fua_url(flow_id)
  cli_alert_info("Fetching {short} from {CFE_AGENCY}")
  t0 <- Sys.time()
  old <- options(timeout = timeout); on.exit(options(old), add = TRUE)
  status <- utils::download.file(url, dest, quiet = TRUE, mode = "wb",
                                 method = "libcurl", cacheOK = FALSE)
  if (status != 0 || !file.exists(dest)) cli_abort("Download failed: {.url {url}}")

  manifest <- tibble::tibble(
    flow        = short,
    dataflow    = glue("{CFE_AGENCY},{flow_id}"),
    fetched_utc = format(as.POSIXct(t0, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    seconds     = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
    url         = url,
    path        = dest,
    bytes       = file.size(dest),
    sha256      = as.character(openssl::sha256(file(dest)))
  )
  write_csv(manifest, mpath, append = file.exists(mpath), col_names = !file.exists(mpath))
  cli_alert_success("{short}: {round(manifest$bytes/1e6, 1)} MB in {manifest$seconds}s")
  invisible(dest)
}

#' Fetch every dataflow the analysis needs.
fua_fetch_all <- function(dir = "data/raw/fua", refresh = FALSE,
                          flows = names(FUA_FLOWS)) {
  for (s in flows) fua_fetch(s, dir = dir, refresh = refresh)
  invisible(file.path(dir, paste0(flows, ".csv")))
}

#' Read a CFE csvfilewithlabels file as character columns (typed later, by
#' the function that knows what each column means).
fua_read <- function(short, dir = "data/raw/fua") {
  read_csv(file.path(dir, glue("{short}.csv")),
           col_types = cols(.default = "c"), progress = FALSE,
           name_repair = "minimal")
}
