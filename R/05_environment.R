# 05_environment.R — exposure is not risk.
#
# The How's Life? environmental dimension measures exposure only: how hot it
# gets, how polluted the air is, how little green there is. It says nothing
# about who is sensitive to that exposure or what protects them. This module
# builds the missing layers from the OECD's own city-level data and asks, with a
# number rather than an argument, whether it changes the answer.
#
# Every layer is tagged with its IPCC risk component:
#   hazard            heat-stress days (UTCI >= 32 C), population-weighted, FUA
#   hazard modifier   summer night-time urban heat island (degC)
#   exposure          resident population, by age band
#   vulnerability     share aged 65+ (sensitivity)
#   adaptive capacity green area per person; cooling degree days (energy burden)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(glue)
  library(cli)
})

# ── Geography ────────────────────────────────────────────────────────────────

# FUA codes look like "USA207F" (ISO3 prefix) or "FR001F" (Eurostat ISO2 prefix
# plus a digit). Map both to ISO3 so outputs join to the How's Life? REF_AREA.
ISO2_TO_ISO3 <- c(
  AT = "AUT", BE = "BEL", BG = "BGR", HR = "HRV", CY = "CYP", CZ = "CZE",
  DK = "DNK", EE = "EST", FI = "FIN", FR = "FRA", DE = "DEU", EL = "GRC",
  GR = "GRC", HU = "HUN", IE = "IRL", IT = "ITA", LV = "LVA", LT = "LTU",
  LU = "LUX", MT = "MLT", NL = "NLD", PL = "POL", PT = "PRT", RO = "ROU",
  SK = "SVK", SI = "SVN", ES = "ESP", SE = "SWE", CH = "CHE", NO = "NOR",
  IS = "ISL", UK = "GBR", GB = "GBR", TR = "TUR", CL = "CHL", CO = "COL",
  KR = "KOR", MX = "MEX", US = "USA", CA = "CAN", AU = "AUS", NZ = "NZL",
  IL = "ISR", RS = "SRB", MK = "MKD", AL = "ALB", ME = "MNE", BA = "BIH",
  XK = "XKX", MD = "MDA", UA = "UKR", LI = "LIE", JP = "JPN", CR = "CRI",
  PE = "PER", AR = "ARG", BR = "BRA", ZA = "ZAF", IN = "IND", ID = "IDN"
)

env_iso3 <- function(fua) {
  p3 <- substr(fua, 1, 3)
  p2 <- substr(fua, 1, 2)
  two_letter <- grepl("^[A-Z]{2}[0-9]", fua)
  out <- ifelse(two_letter, unname(ISO2_TO_ISO3[p2]), p3)
  ifelse(is.na(out), p3, out)
}

# ── Layers ───────────────────────────────────────────────────────────────────

# Filter a CFE table down to functional-urban-area rows when that dimension
# exists; some flows are FUA-only and lack the column.
fua_only <- function(d) {
  if ("TERRITORIAL_LEVEL" %in% names(d)) filter(d, TERRITORIAL_LEVEL == "FUA") else d
}

#' Build the city-level layer table for one reference year.
#'
#' Returns one row per functional urban area with every IPCC component as its
#' own column. `min_pop` drops small FUAs whose single-cell reanalysis values
#' are least reliable.
env_layers <- function(year = 2020, dir = "data/raw/fua", min_pop = 1e5,
                       heat_threshold = "GE32", include_cdd = TRUE) {
  yr <- as.character(year)

  heat <- fua_read("DF_HEAT_STRESS", dir) %>%
    filter(MEASURE == "UTCI_POP_EXP", HEAT_STRESS == heat_threshold,
           TIME_PERIOD == yr) %>%
    fua_only() %>%
    transmute(fua = REF_AREA, name = `Reference area`,
              heat_days = as.numeric(OBS_VALUE))

  uhi <- fua_read("DF_UHI", dir) %>%
    filter(TIME_SEASON == "NIGHT_SUMMER", TIME_PERIOD == yr) %>%
    fua_only() %>%
    transmute(fua = REF_AREA, uhi_night = as.numeric(OBS_VALUE))

  # Green area is a single 2021 snapshot; take whatever year is present.
  green <- fua_read("DF_GREEN_AREA", dir) %>%
    filter(UNIT_MEASURE == "M2_PS") %>%
    fua_only() %>%
    group_by(fua = REF_AREA) %>%
    summarise(green_m2 = as.numeric(first(OBS_VALUE)), .groups = "drop")

  demo <- fua_read("DF_AGE_SEX", dir) %>%
    filter(MEASURE == "POP", SEX == "_T", TIME_PERIOD == yr,
           AGE %in% c("_T", "Y_GE65")) %>%
    fua_only() %>%
    transmute(fua = REF_AREA, age = AGE, pop = as.numeric(OBS_VALUE)) %>%
    filter(!is.na(pop)) %>%
    pivot_wider(names_from = age, values_from = pop, values_fn = first) %>%
    transmute(fua, pop = `_T`, pop65 = Y_GE65, pct65 = 100 * Y_GE65 / `_T`)

  city <- heat %>%
    inner_join(uhi,   by = "fua") %>%
    inner_join(green, by = "fua") %>%
    inner_join(demo,  by = "fua")

  if (include_cdd && file.exists(file.path(dir, "DF_CDD_HDD.csv"))) {
    cdd <- fua_read("DF_CDD_HDD", dir) %>%
      filter(MEASURE == "CDD", TIME_PERIOD == yr) %>%
      fua_only() %>%
      group_by(fua = REF_AREA) %>%
      summarise(cdd = as.numeric(first(OBS_VALUE)), .groups = "drop")
    city <- left_join(city, cdd, by = "fua")
  } else {
    city$cdd <- NA_real_
  }

  city %>%
    filter(!is.na(pop), !is.na(pop65), pop >= min_pop,
           !is.na(heat_days), !is.na(uhi_night), !is.na(green_m2)) %>%
    mutate(iso3 = env_iso3(fua), year = as.integer(year)) %>%
    select(fua, iso3, name, year, heat_days, uhi_night, pop, pop65, pct65,
           green_m2, cdd)
}

# ── Scoring ──────────────────────────────────────────────────────────────────

# Percentile rank, NA-safe; higher = worse for every component after signing.
pct_rank <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  r / sum(!is.na(x))
}

#' Attach component ranks and two scores to the city table.
#'
#' `risk_exposure_only` is the hazard rank alone - the analogue of the existing
#' indicator 9_3. `risk_full` averages every available component. Equal weights
#' are a choice, not a finding; `env_weight_sensitivity()` shows the effect.
env_score <- function(city, weights = NULL) {
  comps <- c(hazard = "heat_days", modifier = "uhi_night",
             vulnerability = "pct65", capacity = "green_m2", energy = "cdd")
  sign  <- c(hazard = 1, modifier = 1, vulnerability = 1, capacity = -1, energy = 1)
  avail <- comps[map_lgl(comps, ~ any(!is.na(city[[.x]])))]

  ranks <- imap_dfc(avail, function(col, nm) {
    tibble(!!paste0("r_", nm) := pct_rank(sign[[nm]] * city[[col]]))
  })
  city <- bind_cols(city, ranks)

  rcols <- paste0("r_", names(avail))
  w <- if (is.null(weights)) setNames(rep(1, length(rcols)), rcols) else weights[rcols]
  w <- w / sum(w, na.rm = TRUE)

  R <- as.matrix(city[rcols])
  # Row-wise weighted mean over the components present for that city.
  present <- !is.na(R)
  Wm <- matrix(w, nrow(R), ncol(R), byrow = TRUE) * present
  city$risk_full <- round(100 * rowSums(R * Wm, na.rm = TRUE) / rowSums(Wm), 1)
  city$risk_exposure_only <- round(100 * city$r_hazard, 1)
  city$top_quintile_risk  <- city$risk_full >= quantile(city$risk_full, 0.8, na.rm = TRUE)
  city
}

# ── Country aggregation ──────────────────────────────────────────────────────

#' One row per country: the exposure-only reading and the risk-based reading.
#'
#' exposure_only  : population-weighted heat-stress days (what 9_3 measures)
#' risk_share65   : share of the country's urban 65+ living in top-quintile-risk cities
#' risk_n65       : the count behind that share
#' risk_wmean65   : 65+-weighted mean risk score (continuous alternative)
env_country <- function(scored, min_cities = 3) {
  scored %>%
    group_by(iso3, year) %>%
    summarise(
      n_cities      = n(),
      pop_urban     = sum(pop),
      pop65_urban   = sum(pop65),
      exposure_only = weighted.mean(heat_days, pop),
      risk_wmean65  = weighted.mean(risk_full, pop65),
      risk_n65      = sum(pop65[top_quintile_risk]),
      risk_share65  = 100 * risk_n65 / pop65_urban,
      .groups = "drop"
    ) %>%
    filter(n_cities >= min_cities) %>%
    # Many countries have no city in the global top risk quintile and tie at
    # zero on the share; the 65+-weighted mean score breaks those ties so the
    # ranking stays informative all the way down.
    arrange(desc(exposure_only)) %>% mutate(rank_exposure = row_number()) %>%
    arrange(desc(risk_share65), desc(risk_wmean65)) %>% mutate(rank_risk = row_number()) %>%
    mutate(rank_shift = rank_exposure - rank_risk)
}

#' Does measuring risk instead of exposure change which countries you worry about?
env_rank_test <- function(country) {
  rho <- suppressWarnings(cor(country$exposure_only, country$risk_share65,
                              method = "spearman"))
  n <- nrow(country)
  top_q <- ceiling(n / 5)
  in_top_exposure <- country$rank_exposure <= top_q
  misplaced <- sum(country$risk_n65[!in_top_exposure]) / sum(country$risk_n65)

  movers <- country %>%
    mutate(abs_shift = abs(rank_shift)) %>%
    arrange(desc(abs_shift)) %>%
    select(iso3, n_cities, exposure_only, risk_share65, risk_n65,
           rank_exposure, rank_risk, rank_shift) %>%
    head(10)

  list(
    n_countries         = n,
    spearman_rho        = round(rho, 3),
    top_quintile_size   = top_q,
    misplaced_share65   = round(100 * misplaced, 1),
    movers              = movers
  )
}

#' Add components one at a time and watch the country ranking move.
env_layer_sensitivity <- function(city) {
  steps <- list(
    "hazard only"                 = c("r_hazard"),
    "+ night heat island"         = c("r_hazard", "r_modifier"),
    "+ share aged 65+"            = c("r_hazard", "r_modifier", "r_vulnerability"),
    "+ green space"               = c("r_hazard", "r_modifier", "r_vulnerability", "r_capacity")
  )
  if ("r_energy" %in% names(city) && any(!is.na(city$r_energy))) {
    steps[["+ cooling degree days"]] <- c(steps[[4]], "r_energy")
  }
  final_cols <- steps[[length(steps)]]

  rank_for <- function(cols) {
    s <- rowMeans(as.matrix(city[cols]), na.rm = TRUE)
    city %>% mutate(s = s) %>% group_by(iso3) %>%
      summarise(score = weighted.mean(s, pop65), .groups = "drop")
  }
  final <- rank_for(final_cols)
  prev  <- NULL
  imap_dfr(steps, function(cols, nm) {
    cur <- rank_for(cols)
    j   <- inner_join(cur, final, by = "iso3", suffix = c("", "_final"))
    rho_final <- suppressWarnings(cor(j$score, j$score_final, method = "spearman"))
    rho_prev  <- if (is.null(prev)) NA_real_ else {
      jp <- inner_join(cur, prev, by = "iso3", suffix = c("", "_prev"))
      suppressWarnings(cor(jp$score, jp$score_prev, method = "spearman"))
    }
    prev <<- cur
    tibble(step = nm, components = length(cols),
           rho_vs_final = round(rho_final, 3),
           rho_vs_previous = round(rho_prev, 3))
  })
}

#' How much do the rankings move under different weightings?
env_weight_sensitivity <- function(city) {
  base <- env_country(env_score(city))
  schemes <- list(
    equal            = NULL,
    hazard_heavy     = c(r_hazard = 2, r_modifier = 1, r_vulnerability = 1, r_capacity = 1, r_energy = 1),
    vulnerability_heavy = c(r_hazard = 1, r_modifier = 1, r_vulnerability = 2, r_capacity = 2, r_energy = 1)
  )
  imap_dfr(schemes, function(w, nm) {
    alt <- env_country(env_score(city, weights = w))
    j <- inner_join(base, alt, by = "iso3", suffix = c("_base", "_alt"))
    tibble(weighting = nm,
           rho_vs_equal = round(suppressWarnings(cor(j$risk_share65_base, j$risk_share65_alt,
                                                     method = "spearman")), 3))
  })
}

# ── Output in the framework's own shape ──────────────────────────────────────

#' Country rows in How's Life? column grammar. OBS_STATUS "E" (estimated) and a
#' methods note: this is evidence and a method, not a published statistic.
env_hsl_rows <- function(country) {
  bind_rows(
    country %>% transmute(REF_AREA = iso3, MEASURE = "ENV_HEATRISK_65_SH",
                          UNIT_MEASURE = "PT_POP_Y_GE65_URB", AGE = "Y_GE65", SEX = "_T",
                          EDUCATION_LEV = "_T", DOMAIN = "HSL_9",
                          TIME_PERIOD = year, OBS_VALUE = round(risk_share65, 2),
                          OBS_STATUS = "E"),
    country %>% transmute(REF_AREA = iso3, MEASURE = "ENV_HEATRISK_65_N",
                          UNIT_MEASURE = "PS", AGE = "Y_GE65", SEX = "_T",
                          EDUCATION_LEV = "_T", DOMAIN = "HSL_9",
                          TIME_PERIOD = year, OBS_VALUE = round(risk_n65),
                          OBS_STATUS = "E"),
    country %>% transmute(REF_AREA = iso3, MEASURE = "ENV_HEATEXP_URB",
                          UNIT_MEASURE = "D_Y", AGE = "_T", SEX = "_T",
                          EDUCATION_LEV = "_T", DOMAIN = "HSL_9",
                          TIME_PERIOD = year, OBS_VALUE = round(exposure_only, 2),
                          OBS_STATUS = "E")
  ) %>% arrange(MEASURE, REF_AREA)
}

# ── Decomposition: climate vs ageing ─────────────────────────────────────────

#' Why did the number of older people in hot cities change between two years?
#'
#' f(P, H) = sum over cities of pop65 x [hot]. Two orderings of the
#' counterfactual are reported, plus their Shapley average, so the split does
#' not depend on which factor you happen to change first.
#' Heat exposure averaged over a window of years centred on `year`.
#'
#' Single-year heat is noisy: 2010 was a hot year across the OECD (53 pop-
#' weighted days against 45-47 in 2008-09), which biased a 2010-vs-2020 split
#' towards demography. A centred five-year mean (half_window = 2) is the
#' default baseline; half_window = 0 reproduces single-year values.
env_heat_window <- function(dir, year, half_window = 2, heat_threshold = "GE32") {
  yrs <- as.character((year - half_window):(year + half_window))
  fua_read("DF_HEAT_STRESS", dir) %>%
    filter(MEASURE == "UTCI_POP_EXP", HEAT_STRESS == heat_threshold,
           TIME_PERIOD %in% yrs) %>%
    fua_only() %>%
    transmute(fua = REF_AREA, days = as.numeric(OBS_VALUE)) %>%
    filter(!is.na(days)) %>%
    group_by(fua) %>%
    summarise(days = mean(days), n_years = n(), .groups = "drop") %>%
    filter(n_years == length(yrs)) %>%
    select(fua, days)
}

#' The balanced panel behind the decomposition: pop65 and windowed heat at both years.
env_decomp_panel <- function(dir = "data/raw/fua", y0 = 2010, y1 = 2020,
                             half_window = 2, heat_threshold = "GE32") {
  a <- fua_read("DF_AGE_SEX", dir) %>%
    filter(MEASURE == "POP", SEX == "_T", AGE == "Y_GE65",
           TIME_PERIOD %in% c(y0, y1)) %>%
    fua_only() %>%
    transmute(fua = REF_AREA, year = as.integer(TIME_PERIOD),
              pop65 = as.numeric(OBS_VALUE)) %>%
    filter(!is.na(pop65)) %>%
    pivot_wider(names_from = year, values_from = pop65, names_prefix = "pop65_", values_fn = first)
  h0 <- env_heat_window(dir, y0, half_window, heat_threshold) %>% rename(days_0 = days)
  h1 <- env_heat_window(dir, y1, half_window, heat_threshold) %>% rename(days_1 = days)
  a %>% inner_join(h0, by = "fua") %>% inner_join(h1, by = "fua") %>% drop_na()
}

# Two-factor Shapley split of f(P, H) = sum(P * H) between P (people) and H (heat).
shapley2 <- function(P0, P1, H0, H1) {
  f <- function(P, H) sum(P * H)
  base <- f(P0, H0); final <- f(P1, H1)
  clim_first <- c(climate = f(P0, H1) - base, ageing = final - f(P0, H1))
  age_first  <- c(ageing  = f(P1, H0) - base, climate = final - f(P1, H0))
  list(
    base = base, final = final, clim_first = clim_first, age_first = age_first,
    shapley = c(climate = unname(mean(c(clim_first["climate"], age_first["climate"]))),
                ageing  = unname(mean(c(clim_first["ageing"],  age_first["ageing"])))),
    interaction = unname(final - f(P0, H1) - f(P1, H0) + base)
  )
}

#' Why did the number of older people in hot cities change between two years?
#'
#' f(P, H) = sum over cities of pop65 x [hot]. Two orderings of the
#' counterfactual are reported, plus their Shapley average, so the split does
#' not depend on which factor you happen to change first. Heat is a centred
#' five-year mean by default (see env_heat_window).
env_decompose <- function(dir = "data/raw/fua", y0 = 2010, y1 = 2020,
                          hot_days = 30, heat_threshold = "GE32", half_window = 2) {
  w <- env_decomp_panel(dir, y0, y1, half_window, heat_threshold)
  P0 <- w[[glue("pop65_{y0}")]]; P1 <- w[[glue("pop65_{y1}")]]
  s <- shapley2(P0, P1, as.numeric(w$days_0 > hot_days), as.numeric(w$days_1 > hot_days))
  tibble(
    term  = c("start", "climate (climate-first order)", "ageing (climate-first order)",
              "ageing (ageing-first order)", "climate (ageing-first order)",
              "climate (Shapley)", "ageing (Shapley)", "interaction", "end"),
    value = c(s$base, s$clim_first["climate"], s$clim_first["ageing"],
              s$age_first["ageing"], s$age_first["climate"],
              s$shapley["climate"], s$shapley["ageing"], s$interaction, s$final),
    n_cities = nrow(w), y0 = y0, y1 = y1, hot_days = hot_days, half_window = half_window
  )
}

#' Does the climate/ageing split survive other reasonable choices?
#'
#' A binary threshold only registers cities that cross it - a city already hot
#' in y0 that got hotter adds nothing to the climate term. The person-days
#' metric (pop65 x days) captures that intensification. Both are reported, at
#' several thresholds, with single-year and windowed heat, so the reader sees
#' the range rather than one number.
env_decompose_sensitivity <- function(dir = "data/raw/fua", y0 = 2010, y1 = 2020,
                                      thresholds = c(10, 20, 30, 45, 60),
                                      heat_threshold = "GE32") {
  rows <- list()
  for (hw in c(0, 2)) {
    w <- env_decomp_panel(dir, y0, y1, hw, heat_threshold)
    P0 <- w[[glue("pop65_{y0}")]]; P1 <- w[[glue("pop65_{y1}")]]
    lab <- if (hw == 0) "single year" else glue("{2*hw+1}-year mean")
    for (t in thresholds) {
      s <- shapley2(P0, P1, as.numeric(w$days_0 > t), as.numeric(w$days_1 > t))
      rows[[length(rows) + 1]] <- tibble(
        metric = glue("people 65+ in cities above {t} heat days"), heat_basis = lab,
        threshold = t, start = s$base, end = s$final,
        climate = unname(s$shapley["climate"]), ageing = unname(s$shapley["ageing"]),
        climate_share = unname(s$shapley["climate"]) / (s$final - s$base), n_cities = nrow(w))
    }
    s <- shapley2(P0, P1, w$days_0, w$days_1)
    rows[[length(rows) + 1]] <- tibble(
      metric = "person-days of heat stress, residents 65+", heat_basis = lab,
      threshold = NA_real_, start = s$base, end = s$final,
      climate = unname(s$shapley["climate"]), ageing = unname(s$shapley["ageing"]),
      climate_share = unname(s$shapley["climate"]) / (s$final - s$base), n_cities = nrow(w))
    rows[[length(rows) + 1]] <- tibble(
      metric = "context: total residents 65+ / pop65-weighted heat days", heat_basis = lab,
      threshold = NA_real_, start = sum(P0), end = sum(P1),
      climate = weighted.mean(w$days_0, P0), ageing = weighted.mean(w$days_1, P1),
      climate_share = NA_real_, n_cities = nrow(w))
  }
  bind_rows(rows) %>% mutate(across(c(climate_share), ~ round(.x, 3)))
}

# ── Orchestrator ─────────────────────────────────────────────────────────────

env_run <- function(year = 2020, dir = "data/raw/fua", out = "out/env",
                    min_pop = 1e5) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  cli_h2("Environmental well-being: exposure vs risk ({year})")

  city    <- env_layers(year, dir, min_pop = min_pop)
  scored  <- env_score(city)
  country <- env_country(scored)
  test    <- env_rank_test(country)
  layers  <- env_layer_sensitivity(scored)
  weights <- env_weight_sensitivity(city)
  hsl     <- env_hsl_rows(country)
  decomp  <- env_decompose(dir)
  dsens   <- env_decompose_sensitivity(dir)

  cli_alert_info("{nrow(scored)} cities, {n_distinct(scored$iso3)} countries with all layers")
  cli_alert_info("Spearman rho (exposure-only vs risk-based country ranking): {test$spearman_rho}")
  cli_alert_info("{test$misplaced_share65}% of at-risk urban 65+ live in countries outside the exposure-only top quintile")

  write_csv(scored,  file.path(out, "city_layers_scored.csv"))
  write_csv(country, file.path(out, "country_indicator.csv"))
  write_csv(test$movers, file.path(out, "rank_test_movers.csv"))
  write_csv(tibble(metric = c("n_countries", "spearman_rho", "top_quintile_size", "misplaced_share65_pct"),
                   value  = c(test$n_countries, test$spearman_rho, test$top_quintile_size, test$misplaced_share65)),
            file.path(out, "rank_test_summary.csv"))
  write_csv(layers,  file.path(out, "layer_sensitivity.csv"))
  write_csv(weights, file.path(out, "weight_sensitivity.csv"))
  write_csv(hsl,     file.path(out, "hsl_schema_rows.csv"))
  write_csv(decomp,  file.path(out, "decomposition.csv"))
  write_csv(dsens,   file.path(out, "decomposition_sensitivity.csv"))
  cs <- dsens %>% filter(!grepl("^context", metric)) %>% pull(climate_share)
  cli_alert_info("Climate share of the change: {round(100*min(cs))}-{round(100*max(cs))}% across {length(cs)} specifications")
  cli_alert_success("Wrote {.path {out}}")

  invisible(list(city = scored, country = country, test = test, layers = layers,
                 weights = weights, hsl = hsl, decomp = decomp, dsens = dsens))
}
