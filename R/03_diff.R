# 03_diff.R — what changed between two vintages of the database?
#
# This is the module that makes a quarterly update round manageable. When the
# database refreshes, someone has to be able to answer: what is new, what was
# revised, what was withdrawn, and did anything move enough to change a
# published finding. Doing that by eye across ~60k observations is not a task,
# it is a hope.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Compare two snapshots and classify every key.
#'
#' Returns a tibble with one row per observation key and a `change` column:
#'   added    - key present in `new` only
#'   removed  - key present in `old` only (a withdrawal; easy to miss)
#'   revised  - same key, value moved by more than `tol`
#'   reflagged- same key and value, but OBS_STATUS changed (e.g. P -> A)
#'   stable   - no change
hsl_diff <- function(old, new, tol = 1e-9) {
  key <- HSL_KEY

  o <- old %>% select(all_of(key), old_value = obs_value, old_status = obs_status)
  n <- new %>% select(all_of(key), new_value = obs_value, new_status = obs_status)

  # Presence markers survive the join, so a key that exists at one vintage
  # with a legitimately empty value is not mistaken for an absent key.
  o$in_old <- TRUE
  n$in_new <- TRUE

  full_join(o, n, by = key) %>%
    mutate(
      in_old     = coalesce(in_old, FALSE),
      in_new     = coalesce(in_new, FALSE),
      abs_change = abs(new_value - old_value),
      pct_change = if_else(!is.na(old_value) & old_value != 0,
                           100 * (new_value - old_value) / abs(old_value),
                           NA_real_),
      change = case_when(
        !in_old &  in_new                          ~ "added",
         in_old & !in_new                          ~ "removed",
        !is.na(abs_change) & abs_change > tol      ~ "revised",
        !identical_status(old_status, new_status)  ~ "reflagged",
        TRUE                                       ~ "stable"
      )
    ) %>%
    select(all_of(key), change, old_value, new_value, abs_change, pct_change,
           old_status, new_status)
}

# NA-safe status comparison: two missing statuses count as identical.
identical_status <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

#' One-line-per-category summary of a diff, for the top of the QA report.
hsl_diff_summary <- function(d) {
  d %>%
    count(change, name = "n") %>%
    mutate(pct = round(100 * n / sum(n), 2)) %>%
    arrange(desc(n))
}

#' Revisions worth a human look.
#'
#' A revision matters when it is large in relative terms, or when it lands on
#' the most recent year (which is what headline figures and country profiles
#' are drawn from). Everything else is noise from rounding and reprocessing.
hsl_material_revisions <- function(d, pct_threshold = 5, recent_years = 2) {
  latest <- max(d$time_period, na.rm = TRUE)
  d %>%
    filter(change == "revised") %>%
    mutate(
      material = abs(pct_change) >= pct_threshold |
                 time_period >= latest - recent_years + 1
    ) %>%
    filter(material) %>%
    arrange(desc(abs(pct_change)))
}

#' Reconcile a local extract against the live source.
#'
#' Answers "is the copy on my laptop still what the database says?" — the
#' check that catches an analysis quietly running on a superseded download.
#' Compares only the keys present in `local`, so a filtered extract does not
#' report the entire rest of the database as missing.
hsl_reconcile <- function(local, live, tol = 1e-9) {
  d <- hsl_diff(local, live, tol = tol)
  keys <- local %>% select(all_of(HSL_KEY))
  d %>%
    semi_join(keys, by = HSL_KEY) %>%
    mutate(change = recode(change, added = "not_in_local", removed = "withdrawn_at_source"))
}
