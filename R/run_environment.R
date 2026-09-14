# run_environment.R — the environmental well-being analysis, figures and site.
#
#   Rscript R/run_environment.R
#
# Assumes R/run_pipeline.R has produced a How's Life? snapshot and QA outputs.
# Fetches the city-level CFE dataflows if they are not already on disk.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(cli)
})

source("R/01_fetch.R")
source("R/02_validate.R")
source("R/03_diff.R")
source("R/04_fetch_fua.R")
source("R/05_environment.R")
source("R/06_figures.R")
source("R/07_site.R")

year <- as.integer(Sys.getenv("HSL_ENV_YEAR", "2020"))

cli_h1("Environmental well-being")
fua_fetch_all(refresh = identical(Sys.getenv("HSL_REFRESH_FUA"), "1"))
env_run(year = year)

cli_h2("Figures")
snap <- read_csv("data/raw/manifest.csv", show_col_types = FALSE) %>% slice_tail(n = 1) %>% pull(path)
fig_all(snap)

cli_h2("Site")
build_site()
