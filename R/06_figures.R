# 06_figures.R — the three figures, each written once as PNG (print) and SVG (web)
# from the same specification. Colour is viridis throughout (colour-blind safe)
# and never the only carrier of meaning: every mark is also labelled.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(glue)
  library(cli)
})

fig_theme <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = base + 3),
      plot.subtitle = element_text(colour = "grey35", size = base),
      plot.caption = element_text(colour = "grey45", size = base - 2, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(colour = "grey30")
    )
}

save_both <- function(p, name, dir = "docs/figures", w = 9, h = 6) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ragg::agg_png(file.path(dir, paste0(name, ".png")), width = w, height = h,
                units = "in", res = 160, background = "white")
  print(p); dev.off()
  svglite::svglite(file.path(dir, paste0(name, ".svg")), width = w, height = h)
  print(p); dev.off()
  cli_alert_success("figure {.file {name}} (png + svg)")
}

# ── Figure 1: where the data gaps are ────────────────────────────────────────

fig_coverage <- function(snapshot_path, out_dir = "docs/figures") {
  obs <- hsl_read(snapshot_path)
  lab <- attr(obs, "labels")
  dom <- lab %>% filter(dim == "domain") %>% select(domain = code, domain_lab = label)

  cov <- obs %>%
    filter(sex == "_T", age == "_T", education_lev == "_T",
           !is.na(obs_value), !is.na(time_period), time_period >= 2005,
           !grepl("_VER$|_DEP$|_GAP$", measure)) %>%
    count(domain, measure, time_period, name = "n_countries") %>%
    left_join(dom, by = "domain") %>%
    mutate(domain_lab = coalesce(domain_lab, domain))

  order_m <- cov %>% group_by(domain_lab, measure) %>%
    summarise(latest = max(time_period), .groups = "drop") %>%
    arrange(domain_lab, latest) %>% pull(measure)
  cov$measure <- factor(cov$measure, levels = unique(order_m))

  p <- ggplot(cov, aes(x = time_period, y = measure, fill = n_countries)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(n_countries >= 40, n_countries, "")),
              size = 2.2, colour = "white") +
    scale_fill_viridis_c(name = "Countries reporting", option = "viridis",
                         direction = 1, limits = c(0, 47)) +
    scale_x_continuous(breaks = seq(2005, 2025, 5), expand = c(0, 0)) +
    facet_grid(domain_lab ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(
      title = "Where the How's Life? database has data - and where it does not",
      subtitle = "Country-average series by indicator and year. Blank = no country reported.",
      x = NULL, y = NULL,
      caption = "Source: OECD How's Life? database (DSD_HSL@DF_HSL_CWB) via the SDMX API. Inequality variants (_VER/_DEP) omitted."
    ) +
    fig_theme(10) +
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 8),
          strip.placement = "outside", axis.text.y = element_text(size = 7),
          panel.spacing.y = unit(2, "pt"))
  save_both(p, "fig1_coverage", out_dir, w = 10, h = 11)
}

# ── Figure 2: exposure-only vs risk-based country ranking ────────────────────

fig_rank_test <- function(country_path = "out/env/country_indicator.csv",
                          summary_path = "out/env/rank_test_summary.csv",
                          out_dir = "docs/figures") {
  cty <- read_csv(country_path, show_col_types = FALSE)
  sm  <- read_csv(summary_path, show_col_types = FALSE)
  rho <- sm$value[sm$metric == "spearman_rho"]
  n   <- nrow(cty)

  p <- ggplot(cty, aes(x = rank_exposure, y = rank_risk)) +
    annotate("rect", xmin = 0.5, xmax = ceiling(n / 5) + 0.5, ymin = ceiling(n / 5) + 0.5,
             ymax = n + 0.5, fill = "grey92", colour = NA) +
    # Axes are reversed (rank 1 at top-right), so anchor the label at the
    # rectangle's left edge and let it run towards the origin.
    annotate("text", x = ceiling(n / 5) + 0.3, y = n - 0.5, hjust = 0, vjust = 0,
             size = 3, colour = "grey35", lineheight = 0.9,
             label = "Top quintile on heat exposure alone,\nbut not on risk") +
    geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
    geom_point(aes(size = risk_n65 / 1e6, colour = risk_share65), alpha = 0.9) +
    ggrepel_or_text(cty) +
    scale_x_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30), expand = expansion(add = 1)) +
    scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30), expand = expansion(add = 1)) +
    scale_colour_viridis_c(name = "Urban 65+ in top-risk cities (%)",
                           option = "plasma", end = 0.9) +
    scale_size_area(name = "Urban 65+ at risk (m)", max_size = 11,
                    breaks = c(1, 5, 10, 20)) +
    labs(
      title = "Exposure is not risk: the same countries, ranked two ways",
      subtitle = glue("Rank by heat exposure alone (what indicator 9_3 measures) vs rank by risk to older residents. ",
                      "Spearman rho = {rho}, {n} countries, 2020."),
      x = "Rank on heat exposure alone (1 = most exposed)",
      y = "Rank on risk to residents aged 65+ (1 = highest)",
      caption = "Risk = equal-weight rank score of heat-stress days, summer night urban heat island, share aged 65+, green space per person (inverse).\nSource: OECD CFE functional urban area dataflows via the SDMX API. Cities over 100,000 residents with all layers present."
    ) +
    fig_theme() +
    guides(colour = guide_colourbar(barwidth = 12, order = 1), size = guide_legend(order = 2))
  save_both(p, "fig2_exposure_vs_risk", out_dir, w = 9.5, h = 7.5)
}

# Label points without a hard dependency on ggrepel.
ggrepel_or_text <- function(cty) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(aes(label = iso3), size = 3, colour = "grey20",
                             max.overlaps = 40, seed = 1)
  } else {
    geom_text(aes(label = iso3), size = 2.8, colour = "grey20", vjust = -0.9)
  }
}

# ── Figure 3: why the number changed ─────────────────────────────────────────

fig_decomposition <- function(decomp_path = "out/env/decomposition.csv",
                              out_dir = "docs/figures") {
  d <- read_csv(decomp_path, show_col_types = FALSE)
  v <- setNames(d$value, d$term)
  y0 <- d$y0[1]; y1 <- d$y1[1]; hot <- d$hot_days[1]

  steps <- tibble(
    label = c(glue("{y0}"), "Cities got hotter\n(climate, Shapley)",
              "Populations aged\n(demography, Shapley)", glue("{y1}")),
    value = c(v["start"], v["climate (Shapley)"], v["ageing (Shapley)"], v["end"]) / 1e6,
    kind  = c("level", "change", "change", "level")
  ) %>%
    mutate(
      idx = row_number(),
      end   = cumsum(ifelse(kind == "level" & idx > 1, 0, value)),
      end   = ifelse(idx == nrow(.), value, end),
      start = ifelse(kind == "level", 0, lag(end)),
      mid   = (start + end) / 2,
      txt   = ifelse(kind == "level", sprintf("%.1f m", value),
                     sprintf("%+.1f m", value))
    )

  p <- ggplot(steps) +
    geom_rect(aes(xmin = idx - 0.4, xmax = idx + 0.4, ymin = start, ymax = end, fill = kind)) +
    geom_text(aes(x = idx, y = pmax(start, end), label = txt), vjust = -0.5, size = 3.6,
              fontface = "bold") +
    scale_x_continuous(breaks = steps$idx, labels = steps$label) +
    scale_fill_manual(values = c(level = "#3B528B", change = "#5DC863"), guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = glue("Older people living in hot cities: {y0} to {y1}"),
      subtitle = glue("Residents aged 65+ in OECD functional urban areas with more than {hot} days a year of strong heat stress (UTCI >= 32 C).\n",
                      "Shapley split of the change into climate and demography; the interaction term is shared between them."),
      x = NULL, y = "Millions of people aged 65+",
      caption = glue("Balanced panel of {d$n_cities[1]} cities present in both years. Source: OECD CFE DF_HEAT_STRESS and DF_AGE_SEX via the SDMX API.")
    ) +
    fig_theme()
  save_both(p, "fig3_decomposition", out_dir, w = 9, h = 6)
}

fig_all <- function(snapshot_path) {
  fig_coverage(snapshot_path)
  fig_rank_test()
  fig_decomposition()
}
