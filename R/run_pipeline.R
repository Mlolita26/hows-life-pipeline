# run_pipeline.R — the whole refresh, end to end, in one command.
#
#   Rscript R/run_pipeline.R
#
# Fetches the current vintage, validates it, diffs it against the previous
# vintage if there is one, and writes a QA report. This is what the scheduled
# GitHub Action runs; a human runs the identical command locally.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(glue)
  library(cli)
})

source("R/01_fetch.R")
source("R/02_validate.R")
source("R/03_diff.R")

# Minimal markdown table writer - keeps the report dependency-free.
md_table <- function(df) {
  if (nrow(df) == 0) return("_none_")
  df <- mutate(df, across(everything(), as.character))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

dir.create("out", showWarnings = FALSE)

# ── 1. Fetch ─────────────────────────────────────────────────────────────────
mf  <- hsl_fetch("cwb")
new <- hsl_read(mf$path)

# ── 2. Validate ──────────────────────────────────────────────────────────────
cli_h2("Validating")
issues   <- hsl_validate(new)
coverage <- hsl_coverage(new)
gaps     <- hsl_gaps(new)

sev <- count(issues, severity)
cli_alert_info("{nrow(issues)} issues raised across {n_distinct(issues$rule)} rules")
print(sev)

write_csv(issues,   "out/issues.csv")
write_csv(coverage, "out/coverage.csv")
write_csv(gaps,     "out/gaps.csv")

# ── 3. Diff against the previous vintage ─────────────────────────────────────
prev <- sort(list.files("data/raw", "^hsl_cwb_.*\\.csv$", full.names = TRUE))
prev <- setdiff(prev, mf$path)

if (length(prev) > 0) {
  cli_h2("Diffing against {.path {basename(tail(prev, 1))}}")
  old <- hsl_read(tail(prev, 1))
  d   <- hsl_diff(old, new)
  write_csv(d, "out/diff.csv")
  write_csv(hsl_material_revisions(d), "out/material_revisions.csv")
  print(hsl_diff_summary(d))
} else {
  cli_alert_info("No earlier vintage on disk - nothing to diff. ",
                 "The next scheduled run will produce a change report.")
  d <- NULL
}

# ── 4. Report ────────────────────────────────────────────────────────────────
report <- c(
  glue("# How's Life? refresh - {mf$vintage}"),
  "",
  glue("Source: `{mf$url}`"),
  glue("Retrieved: {mf$fetched_utc} ({mf$seconds}s, {format(mf$bytes/1e6, digits=3)} MB)"),
  glue("SHA-256: `{substr(mf$sha256, 1, 16)}...`"),
  glue("Observations: **{format(nrow(new), big.mark = ',')}** across ",
       "{n_distinct(new$ref_area)} reference areas and {n_distinct(new$measure)} indicators"),
  "",
  "## Validation",
  "",
  md_table(sev),
  "",
  "### Issues by rule",
  "",
  md_table(count(issues, rule, severity, sort = TRUE)),
  "",
  "## Coverage: indicators with the longest reporting lag",
  "",
  md_table(head(coverage, 12)),
  "",
  "## Collection priorities",
  "",
  glue("{sum(gaps$status == 'missing')} country x indicator cells have no data at all; ",
       "{sum(gaps$status == 'stale')} are more than 3 years old. See `out/gaps.csv`."),
  "",
  if (!is.null(d)) c("## Changes since the previous vintage", "",
                     md_table(hsl_diff_summary(d))) else
    c("## Changes", "", "_First vintage on disk; no comparison available._")
)

writeLines(report, "out/report.md")
cli_alert_success("Wrote {.path out/report.md}")
