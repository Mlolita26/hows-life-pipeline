library(testthat)

source("../../R/01_fetch.R")
source("../../R/02_validate.R")
source("../../R/03_diff.R")

# A minimal snapshot in the shape hsl_read() produces.
mk <- function(...) {
  base <- tibble::tibble(
    ref_area = "FRA", measure = "5_1", unit_measure = "Y",
    age = "_T", sex = "_T", education_lev = "_T", domain = "HSL_5",
    time_period = 2022L, obs_value = 82.5, obs_status = "A"
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

test_that("a clean observation raises nothing", {
  expect_equal(nrow(hsl_validate(mk())), 0)
})

test_that("a normal value may not be empty", {
  i <- hsl_validate(mk(obs_value = NA_real_, obs_status = "A"))
  expect_true("normal_value_missing" %in% i$rule)
  expect_equal(i$severity[i$rule == "normal_value_missing"], "error")
})

test_that("duplicate dimension keys are an error", {
  i <- hsl_validate(bind_rows(mk(), mk(obs_value = 83.0)))
  expect_true("duplicate_key" %in% i$rule)
})

test_that("population shares are bounded at zero but gaps are not", {
  # A share of a population cannot be negative...
  share <- mk(unit_measure = "PT_POP", obs_value = -1)
  expect_true("value_out_of_range" %in% hsl_validate(share)$rule)

  # ...but a gender wage gap legitimately can be. This is the real case:
  # Luxembourg 2018, measure 2_2, unit PT_WG_SAL_M_D, value -3.13.
  gap <- mk(measure = "2_2", unit_measure = "PT_WG_SAL_M_D", obs_value = -3.13)
  expect_false("value_out_of_range" %in% hsl_validate(gap)$rule)
})

test_that("unrecognised observation statuses are surfaced", {
  i <- hsl_validate(mk(obs_status = "ZZ"))
  expect_true("obs_status_unknown" %in% i$rule)
})

test_that("comparability flags are reported as info, not failure", {
  i <- hsl_validate(mk(obs_status = "B"))
  expect_true(all(i$severity == "info"))
  expect_true("series_break" %in% i$rule)
})

# ── diff ─────────────────────────────────────────────────────────────────────

test_that("diff classifies every kind of change", {
  old <- bind_rows(
    mk(),                                              # unchanged
    mk(time_period = 2021L, obs_value = 82.0),         # will be revised
    mk(time_period = 2020L, obs_value = 81.5),         # will be withdrawn
    mk(time_period = 2019L, obs_value = 81.0, obs_status = "P")  # will be reflagged
  )
  new <- bind_rows(
    mk(),
    mk(time_period = 2021L, obs_value = 82.4),
    mk(time_period = 2023L, obs_value = 82.9),         # newly added
    mk(time_period = 2019L, obs_value = 81.0, obs_status = "A")
  )

  d <- hsl_diff(old, new)
  got <- setNames(d$change, d$time_period)

  expect_equal(unname(got["2022"]), "stable")
  expect_equal(unname(got["2021"]), "revised")
  expect_equal(unname(got["2020"]), "removed")
  expect_equal(unname(got["2023"]), "added")
  expect_equal(unname(got["2019"]), "reflagged")
})

test_that("a withdrawal is not silently read as a revision to NA", {
  d <- hsl_diff(mk(), mk()[0, ])
  expect_equal(d$change, "removed")
})

test_that("material revisions filter on size and recency", {
  old <- bind_rows(mk(time_period = 2010L, obs_value = 100),
                   mk(time_period = 2011L, obs_value = 100))
  new <- bind_rows(mk(time_period = 2010L, obs_value = 100.01),  # trivial, old
                   mk(time_period = 2011L, obs_value = 130))     # large
  m <- hsl_material_revisions(hsl_diff(old, new), pct_threshold = 5,
                              recent_years = 1)
  expect_equal(nrow(m), 1)
  expect_equal(m$time_period, 2011L)
})

test_that("coverage reports lag and country counts per indicator", {
  obs <- bind_rows(
    mk(ref_area = "FRA", time_period = 2020L),
    mk(ref_area = "DEU", time_period = 2022L)
  )
  cv <- hsl_coverage(obs, as_of = 2026)
  expect_equal(cv$n_countries, 2)
  expect_equal(cv$latest_year, 2022)
  expect_equal(cv$lag_years, 4)
})

test_that("gaps separate missing cells from stale ones", {
  obs <- bind_rows(
    mk(ref_area = "FRA", measure = "5_1", time_period = 2025L),
    mk(ref_area = "FRA", measure = "5_2", time_period = 2015L),
    mk(ref_area = "DEU", measure = "5_1", time_period = 2025L)
  )
  g <- hsl_gaps(obs, stale_after = 3, as_of = 2026)
  expect_setequal(g$status, c("stale", "missing"))
  expect_equal(g$measure[g$status == "missing"], "5_2")
  expect_equal(g$ref_area[g$status == "missing"], "DEU")
})

test_that("the SDMX query URL is well formed", {
  u <- hsl_url("cwb", start_period = 2015)
  expect_match(u, "^https://sdmx\\.oecd\\.org/public/rest/data/")
  expect_match(u, "DSD_HSL@DF_HSL_CWB,1\\.1", fixed = FALSE)
  expect_match(u, "startPeriod=2015")
  expect_error(hsl_url("nope"))
})
