# 02_validate.R — quality assurance rules for a How's Life? snapshot.
#
# Every rule returns rows that FAIL it, tagged with a severity. The output is
# one tidy issues table, which is what makes this reportable, diffable and
# testable. Nothing here stops the pipeline: the job of QA is to tell you what
# changed and what looks wrong, not to hide it by refusing to run.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# SDMX cross-domain observation status codelist (CL_OBS_STATUS). Anything
# outside this set means the publisher has started using a code we do not
# understand, which is worth knowing about immediately.
OBS_STATUS_KNOWN <- c("A", "B", "D", "E", "F", "G", "H", "I", "J",
                      "K", "L", "M", "O", "P", "Q", "S", "U", "V", "W")

# Plausible ranges implied by the unit of measure. Unlike a hardcoded
# per-indicator table, this is derived from a dimension the publisher already
# maintains, so new indicators are covered the day they appear.
#
# The distinction between the two percentage cases is not cosmetic. A share of
# a population cannot be negative. A percentage expressed *relative to another
# group* can be: the gender wage gap (2_2, unit PT_WG_SAL_M_D) is negative in
# Luxembourg in 2018 and 2022, in Bulgaria in 2006 and in Croatia in 2014,
# because women in those deciles out-earned men. A blanket "percentages are
# 0-100" rule flags all four as errors. They are correct values, and the rule
# was wrong - which is the argument for calibrating validation against the
# full live series rather than against a sample.
unit_range <- function(unit) {
  case_when(
    grepl("^PT_POP", unit)          ~ list(c(0, 100)),    # shares of a population
    grepl("^PT_",    unit)          ~ list(c(-100, 100)), # signed: gaps, ratios
    unit %in% c("Y")                ~ list(c(0, 120)),    # years
    grepl("SCORE|_0_10$", unit)     ~ list(c(0, 10)),     # 0-10 scales
    grepl("^H_",     unit)          ~ list(c(0, 168)),    # hours
    TRUE                            ~ list(c(-Inf, Inf))
  )
}

issue <- function(data, rule, severity, detail) {
  if (nrow(data) == 0) {
    return(tibble::tibble(rule = character(), severity = character(),
                          detail = character(), ref_area = character(),
                          measure = character(), time_period = integer()))
  }
  data %>%
    mutate(rule = rule, severity = severity, detail = detail) %>%
    select(rule, severity, detail, any_of(c("ref_area", "measure", "time_period")))
}

#' Run every rule over a snapshot. Returns a tidy issues table.
hsl_validate <- function(obs) {
  stopifnot(all(HSL_KEY %in% names(obs)))

  bind_rows(
    # --- structural -------------------------------------------------------
    obs %>%
      filter(is.na(time_period)) %>%
      issue("time_period_unparseable", "error",
            "TIME_PERIOD did not parse as an integer year"),

    obs %>%
      group_by(across(all_of(HSL_KEY))) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      distinct(across(all_of(HSL_KEY))) %>%
      issue("duplicate_key", "error",
            "More than one observation shares the same full dimension key"),

    # --- value integrity --------------------------------------------------
    # A normal value that is missing is a contradiction: status A asserts the
    # figure is real and final.
    obs %>%
      filter(obs_status == "A", is.na(obs_value)) %>%
      issue("normal_value_missing", "error",
            "OBS_STATUS is 'A' (normal value) but OBS_VALUE is empty"),

    obs %>%
      filter(!is.na(obs_status), !obs_status %in% OBS_STATUS_KNOWN) %>%
      issue("obs_status_unknown", "warning",
            "OBS_STATUS is not in the SDMX CL_OBS_STATUS codelist"),

    obs %>%
      mutate(rng = unit_range(unit_measure)) %>%
      filter(!is.na(obs_value),
             obs_value < map_dbl(rng, 1) | obs_value > map_dbl(rng, 2)) %>%
      issue("value_out_of_range", "error",
            "OBS_VALUE falls outside the range implied by its unit of measure"),

    obs %>%
      filter(!is.na(time_period), time_period < 1990 |
               time_period > as.integer(format(Sys.Date(), "%Y"))) %>%
      issue("time_period_implausible", "warning",
            "Reference year is before 1990 or in the future"),

    # --- comparability ----------------------------------------------------
    # These are not errors. They are the flags an analyst must see before
    # putting two countries on the same chart, and they are routinely lost
    # when data is passed around as a spreadsheet of values.
    obs %>%
      filter(obs_status == "B") %>%
      issue("series_break", "info",
            "Break in time series - not comparable with earlier years"),

    obs %>%
      filter(obs_status == "D") %>%
      issue("definition_differs", "info",
            "Definition differs - limited cross-country comparability"),

    obs %>%
      filter(obs_status %in% c("E", "P")) %>%
      issue("provisional_or_estimated", "info",
            "Estimated or provisional value - liable to revision")
  )
}

#' Coverage of the country-average series: how many countries report each
#' indicator, and how stale is the most recent figure?
#'
#' This is the table that answers the question the data collection round
#' actually exists to answer - which cells do we need to chase.
hsl_coverage <- function(obs, as_of = as.integer(format(Sys.Date(), "%Y"))) {
  obs %>%
    filter(sex == "_T", age == "_T", education_lev == "_T",
           !is.na(obs_value), !is.na(time_period)) %>%
    group_by(domain, measure) %>%
    summarise(
      n_countries    = n_distinct(ref_area),
      latest_year    = max(time_period),
      lag_years      = as_of - max(time_period),
      n_obs          = n(),
      pct_flagged    = round(100 * mean(obs_status %in% c("B", "D", "E", "P")), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(lag_years), n_countries)
}

#' Which country x indicator cells are missing entirely, or have gone stale?
#' Feeds the "who do we write to" list for the next CSSP data request.
hsl_gaps <- function(obs, stale_after = 3L,
                     as_of = as.integer(format(Sys.Date(), "%Y"))) {
  cavg <- obs %>%
    filter(sex == "_T", age == "_T", education_lev == "_T", !is.na(obs_value))

  latest <- cavg %>%
    group_by(ref_area, measure) %>%
    summarise(latest_year = max(time_period), .groups = "drop")

  expand_grid(
    ref_area = sort(unique(cavg$ref_area)),
    measure  = sort(unique(cavg$measure))
  ) %>%
    left_join(latest, by = c("ref_area", "measure")) %>%
    mutate(
      status = case_when(
        is.na(latest_year)                     ~ "missing",
        as_of - latest_year > stale_after      ~ "stale",
        TRUE                                   ~ "current"
      ),
      lag_years = as_of - latest_year
    ) %>%
    filter(status != "current") %>%
    arrange(status, desc(lag_years), ref_area)
}
