library(testthat)

source("../../R/01_fetch.R")
source("../../R/04_fetch_fua.R")
source("../../R/05_environment.R")

# A small synthetic city table in the shape env_layers() produces.
mk_city <- function() {
  tibble::tibble(
    fua   = c("AAA01F", "AAA02F", "AAA03F", "BBB01F", "BBB02F", "BBB03F",
              "CC001F", "CC002F", "CC003F"),
    iso3  = c(rep("AAA", 3), rep("BBB", 3), rep("CCC", 3)),
    name  = paste("City", 1:9),
    year  = 2020L,
    # AAA: very hot but young and green.  BBB: moderate heat, old, dense, no green.
    # CCC: cool.
    heat_days = c(120, 110, 100,   60, 55, 50,   10, 8, 5),
    uhi_night = c(0.2, 0.3, 0.1,   3.0, 2.8, 2.5, 1.0, 0.9, 1.1),
    pop       = c(5e5, 4e5, 3e5,   6e5, 5e5, 4e5, 3e5, 2e5, 2e5),
    pop65     = c(5e4, 4e4, 3e4,   1.8e5, 1.5e5, 1.2e5, 4.5e4, 3e4, 3e4),
    pct65     = c(10, 10, 10,      30, 30, 30,   15, 15, 15),
    green_m2  = c(600, 550, 500,   30, 25, 20,   150, 140, 160),
    cdd       = NA_real_
  )
}

test_that("FUA prefixes map to ISO3 for both code styles", {
  expect_equal(env_iso3(c("USA207F", "FR001F", "EL001F", "UK001F", "JPN01C")),
               c("USA", "FRA", "GRC", "GBR", "JPN"))
})

test_that("scores exist, are bounded, and exposure-only equals the hazard rank", {
  s <- env_score(mk_city())
  expect_true(all(s$risk_full >= 0 & s$risk_full <= 100))
  expect_equal(s$risk_exposure_only, round(100 * s$r_hazard, 1))
  # The hottest city has the top exposure-only score
  expect_equal(s$fua[which.max(s$risk_exposure_only)], "AAA01F")
})

test_that("a young, green, very hot country ranks above an old, dense, warm one on exposure but below it on risk", {
  cty <- env_country(env_score(mk_city()), min_cities = 3)
  a <- cty[cty$iso3 == "AAA", ]; b <- cty[cty$iso3 == "BBB", ]
  expect_lt(a$rank_exposure, b$rank_exposure)   # AAA hotter
  expect_gt(a$rank_risk,     b$rank_risk)       # but BBB at greater risk
})

test_that("the rank test reports a correlation and a misplaced share", {
  t <- env_rank_test(env_country(env_score(mk_city()), min_cities = 3))
  expect_true(is.numeric(t$spearman_rho))
  expect_true(t$misplaced_share65 >= 0 && t$misplaced_share65 <= 100)
  expect_equal(t$n_countries, 3)
})

test_that("layer sensitivity ends with rho = 1 against itself", {
  ls <- env_layer_sensitivity(env_score(mk_city()))
  expect_equal(tail(ls$rho_vs_final, 1), 1)
  expect_true(is.na(ls$rho_vs_previous[1]))
})

test_that("HSL-schema rows carry the framework's columns and an E status", {
  h <- env_hsl_rows(env_country(env_score(mk_city()), min_cities = 3))
  expect_setequal(names(h), c("REF_AREA", "MEASURE", "UNIT_MEASURE", "AGE", "SEX",
                              "EDUCATION_LEV", "DOMAIN", "TIME_PERIOD", "OBS_VALUE",
                              "OBS_STATUS"))
  expect_true(all(h$OBS_STATUS == "E"))
  expect_true(all(h$DOMAIN == "HSL_9"))
  expect_equal(nrow(h), 3 * 3)
})

test_that("the decomposition identity holds: start + climate + ageing + interaction = end", {
  # Build a tiny two-year FUA directory on the fly.
  d <- tempfile(); dir.create(d)
  heat <- tibble::tibble(
    STRUCTURE = "x", REF_AREA = rep(c("AAA01F", "AAA02F"), each = 2),
    `Reference area` = "c", MEASURE = "UTCI_POP_EXP", HEAT_STRESS = "GE32",
    TERRITORIAL_LEVEL = "FUA", TIME_PERIOD = rep(c("2010", "2020"), 2),
    OBS_VALUE = c("20", "40", "50", "60")
  )
  demo <- tibble::tibble(
    STRUCTURE = "x", REF_AREA = rep(c("AAA01F", "AAA02F"), each = 2),
    `Reference area` = "c", MEASURE = "POP", SEX = "_T", AGE = "Y_GE65",
    TIME_PERIOD = rep(c("2010", "2020"), 2),
    OBS_VALUE = c("1000", "1500", "2000", "2600")
  )
  readr::write_csv(heat, file.path(d, "DF_HEAT_STRESS.csv"))
  readr::write_csv(demo, file.path(d, "DF_AGE_SEX.csv"))

  dc <- env_decompose(dir = d, y0 = 2010, y1 = 2020, hot_days = 30, half_window = 0)
  v  <- setNames(dc$value, dc$term)
  expect_equal(unname(v["start"] + v["climate (Shapley)"] + v["ageing (Shapley)"]),
               unname(v["end"]))
  # City 1 crosses the threshold (20 -> 40): climate effect is positive.
  expect_gt(unname(v["climate (Shapley)"]), 0)

  # Sensitivity table: the two metrics answer different questions and give
  # different splits. Hand-computed for this fixture:
  #   threshold  f = pop65 x [days > 30]: base 2000, end 4100,
  #              Shapley climate mean(1000, 1500) = 1250 -> share 1250/2100 = 0.595
  #   person-days f = pop65 x days: base 120000, end 216000,
  #              Shapley climate mean(40000, 56000) = 48000 -> share 0.5
  s <- env_decompose_sensitivity(dir = d, y0 = 2010, y1 = 2020, thresholds = 30)
  thr <- s[s$metric == "people 65+ in cities above 30 heat days" & s$heat_basis == "single year", ]
  pd  <- s[grepl("^person-days", s$metric) & s$heat_basis == "single year", ]
  expect_equal(thr$climate_share, 0.595)
  expect_equal(pd$climate_share, 0.5)
  expect_equal(pd$start, 120000); expect_equal(pd$end, 216000)
  # Both bases (single year and windowed) are present, plus a context row each.
  expect_setequal(unique(s$heat_basis), c("single year", "5-year mean"))
  expect_equal(sum(grepl("^context", s$metric)), 2)
})

test_that("a centred heat window averages the requested years only", {
  d <- tempfile(); dir.create(d)
  heat <- tibble::tibble(
    STRUCTURE = "x", REF_AREA = "AAA01F", `Reference area` = "c", MEASURE = "UTCI_POP_EXP",
    HEAT_STRESS = "GE32", TERRITORIAL_LEVEL = "FUA",
    TIME_PERIOD = as.character(2008:2012), OBS_VALUE = c("10", "20", "60", "20", "10")
  )
  readr::write_csv(heat, file.path(d, "DF_HEAT_STRESS.csv"))
  expect_equal(env_heat_window(d, 2010, half_window = 2)$days, 24)
  expect_equal(env_heat_window(d, 2010, half_window = 0)$days, 60)
  # An incomplete window is dropped rather than averaged over fewer years.
  expect_equal(nrow(env_heat_window(d, 2011, half_window = 2)), 0)
})
