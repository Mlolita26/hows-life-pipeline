# 06_figures.R — three figures in an OECD-like visual register.
#
# Forms follow the FT Visual Vocabulary: a slope chart for how ranks change
# between two measures (Ranking), a waterfall for a change split into signed
# components (Part-to-whole), a heatmap for a category x time grid (Change over
# time). Colour follows the job, not taste, and every categorical colour used
# here passed scripts/validate_palette.js (dataviz skill) on a white surface:
#   primary  #245c99  (OECD-style mid navy; L in band, >= 3:1 on white)
#   accent   #5ba3de  (light blue; 2.7:1 -> relief by direct labels, always)
#   context  #9aa5b4  (de-emphasis grey: never carries identity alone)
# The deep OECD navy #0d2240 fails the mark-lightness band, so it is chrome only:
# the footer band, and the title ink. Sequential magnitude uses one blue ramp,
# light -> dark. Text never wears a series colour.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(grid)
  library(glue)
  library(cli)
})

OECD_NAVY    <- "#0d2240"
OECD_PRIMARY <- "#245c99"
OECD_ACCENT  <- "#5ba3de"
OECD_GREY    <- "#9aa5b4"
OECD_INK     <- "#141a24"
OECD_INK2    <- "#4a5568"
OECD_GRID    <- "#e6e9ee"
OECD_SURFACE <- "#ffffff"
# Sequential blue ramp (light -> dark, single hue; monotone L and 13 deg hue
# spread verified). The lightest step means "nearly none" and may recede.
OECD_RAMP <- c("#c5dff5", "#9fc7ea", "#6fa9dc", "#4688c9", "#2f6fb0", "#245c99",
               "#1a4172", "#0d2240")

oecd_theme <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = OECD_SURFACE, colour = NA),
      panel.background = element_rect(fill = OECD_SURFACE, colour = NA),
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", size = base + 5, colour = OECD_INK,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = base, colour = OECD_INK2, lineheight = 1.05,
                                   margin = margin(b = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = OECD_GRID, linewidth = 0.4),
      axis.title = element_text(colour = OECD_INK2, size = base - 1),
      axis.text  = element_text(colour = OECD_INK2),
      legend.position = "bottom",
      legend.title = element_text(colour = OECD_INK2, size = base - 1),
      legend.text  = element_text(colour = OECD_INK2, size = base - 1),
      plot.margin = margin(14, 18, 8, 18)
    )
}

#' Render a plot with the OECD-style footer band: deep navy strip, white text,
#' data source on the right. Written once as PNG (print) and SVG (web).
save_oecd <- function(p, name, source_text, dir = "docs/figures", w = 9, h = 6,
                      band_in = 0.5) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  draw <- function() {
    grid.newpage()
    grid.rect(gp = gpar(fill = OECD_SURFACE, col = NA))
    lay <- grid.layout(2, 1, heights = unit(c(h - band_in, band_in), "in"))
    pushViewport(viewport(layout = lay))
    pushViewport(viewport(layout.pos.row = 1)); print(p, newpage = FALSE); upViewport()
    pushViewport(viewport(layout.pos.row = 2))
    grid.rect(gp = gpar(fill = OECD_NAVY, col = NA))
    # Two lines, both left-aligned: the brand line never collides with a long
    # source string, whatever the canvas width.
    grid.text("How's Life? refresh & QA pipeline", x = unit(0.18, "in"), y = unit(0.66, "npc"),
              just = "left", gp = gpar(col = "white", fontsize = 9, fontface = "bold"))
    grid.text(source_text, x = unit(0.18, "in"), y = unit(0.28, "npc"), just = "left",
              gp = gpar(col = "#c9d6e6", fontsize = 7.6))
    upViewport(2)
  }
  ragg::agg_png(file.path(dir, paste0(name, ".png")), width = w, height = h,
                units = "in", res = 160, background = OECD_SURFACE)
  draw(); dev.off()
  svglite::svglite(file.path(dir, paste0(name, ".svg")), width = w, height = h)
  draw(); dev.off()
  cli_alert_success("figure {.file {name}} (png + svg)")
}

# ── Figure 1: where the data gaps are (heatmap, sequential) ─────────────────

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

  # Dimensions ordered by mean reporting lag of their indicators, largest first,
  # so the least current dimension - Environmental quality - leads the eye.
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  dom_order <- cov %>% group_by(domain_lab, measure) %>%
    summarise(lag = this_year - max(time_period), .groups = "drop") %>%
    group_by(domain_lab) %>% summarise(mean_lag = mean(lag), .groups = "drop") %>%
    arrange(desc(mean_lag), domain_lab) %>% pull(domain_lab)
  cov$domain_lab <- factor(cov$domain_lab, levels = dom_order)
  order_m <- cov %>% group_by(domain_lab, measure) %>%
    summarise(latest = max(time_period), .groups = "drop") %>%
    arrange(domain_lab, latest) %>% pull(measure)
  cov$measure <- factor(cov$measure, levels = unique(order_m))

  p <- ggplot(cov, aes(x = time_period, y = measure, fill = n_countries)) +
    geom_tile(colour = OECD_SURFACE, linewidth = 0.6) +
    geom_text(aes(label = ifelse(n_countries >= 44, n_countries, "")),
              size = 2.1, colour = "white") +
    scale_fill_gradientn(name = "Countries reporting", colours = OECD_RAMP,
                         limits = c(0, 47), breaks = c(1, 10, 20, 30, 40, 47)) +
    scale_x_continuous(breaks = seq(2005, 2025, 5), expand = c(0, 0)) +
    facet_grid(domain_lab ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(
      title = "Environmental quality is the least current part of the How's Life? database",
      subtitle = "Number of countries with a country-average value, by indicator and year. Blank = no country reported.\nInequality variants (_VER, _DEP) omitted. Green space (9_1) was last published in 2018, for 29 of 47 countries.",
      x = NULL, y = NULL
    ) +
    oecd_theme(10) +
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 8,
                                           colour = OECD_INK),
          strip.placement = "outside", axis.text.y = element_text(size = 7),
          panel.spacing.y = unit(3, "pt"), panel.grid.major = element_blank(),
          legend.key.width = unit(28, "pt"), legend.key.height = unit(8, "pt"))
  save_oecd(p, "fig1_coverage",
            "Source: OECD How's Life? database (OECD.WISE.WDP, DSD_HSL@DF_HSL_CWB), retrieved via the SDMX API",
            out_dir, w = 10, h = 11.5)
}

# ── Figure 2: exposure vs risk — a slope chart (Ranking) ────────────────────

fig_rank_test <- function(country_path = "out/env/country_indicator.csv",
                          summary_path = "out/env/rank_test_summary.csv",
                          out_dir = "docs/figures", n_highlight = 6) {
  cty <- read_csv(country_path, show_col_types = FALSE)
  sm  <- read_csv(summary_path, show_col_types = FALSE)
  rho <- sm$value[sm$metric == "spearman_rho"]
  n   <- nrow(cty)

  # Emphasis: the biggest movers in the primary blue, everyone else in grey.
  # Direction is carried by the slope itself and by the labels - never by hue.
  movers <- cty %>% arrange(desc(abs(rank_shift))) %>% slice_head(n = n_highlight) %>% pull(iso3)
  cty <- cty %>% mutate(
    emph  = iso3 %in% movers,
    left  = rank_exposure, right = rank_risk,
    lab_l = glue("{iso3}  {left}"),
    lab_r = glue("{right}  {iso3}"),
    lab_r = ifelse(emph, glue("{lab_r}   ({sprintf('%+d', -rank_shift)})"), lab_r)
  )
  long <- cty %>% select(iso3, emph, left, right) %>%
    pivot_longer(c(left, right), names_to = "side", values_to = "rank") %>%
    mutate(x = ifelse(side == "left", 0, 1))

  p <- ggplot() +
    geom_segment(data = filter(cty, !emph),
                 aes(x = 0, xend = 1, y = left, yend = right),
                 colour = OECD_GREY, linewidth = 0.7, alpha = 0.8) +
    geom_segment(data = filter(cty, emph),
                 aes(x = 0, xend = 1, y = left, yend = right),
                 colour = OECD_PRIMARY, linewidth = 2, lineend = "round") +
    geom_point(data = filter(long, !emph), aes(x, rank), colour = OECD_GREY, size = 2.2,
               stroke = 0.8, fill = OECD_SURFACE, shape = 21) +
    geom_point(data = filter(long, emph), aes(x, rank), colour = OECD_PRIMARY, size = 3.6,
               stroke = 1, fill = OECD_SURFACE, shape = 21) +
    geom_text(data = cty, aes(x = -0.03, y = left, label = lab_l,
                              fontface = ifelse(emph, "bold", "plain")),
              hjust = 1, size = 2.9, colour = ifelse(cty$emph, OECD_INK, OECD_INK2)) +
    geom_text(data = cty, aes(x = 1.03, y = right, label = lab_r,
                              fontface = ifelse(emph, "bold", "plain")),
              hjust = 0, size = 2.9, colour = ifelse(cty$emph, OECD_INK, OECD_INK2)) +
    annotate("text", x = 0, y = 0, label = "Rank on heat exposure alone\n(what indicator 9_3 measures)",
             hjust = 0.5, vjust = 0, size = 3.2, colour = OECD_INK, fontface = "bold", lineheight = 0.95) +
    annotate("text", x = 1, y = 0, label = "Rank on risk to residents aged 65+\n(hazard × heat island × vulnerability × green space)",
             hjust = 0.5, vjust = 0, size = 3.2, colour = OECD_INK, fontface = "bold", lineheight = 0.95) +
    scale_y_reverse(breaks = NULL, expand = expansion(add = c(0.6, 1.6))) +
    scale_x_continuous(limits = c(-0.42, 1.55), breaks = NULL) +
    labs(
      title = "Exposure is not risk: the same countries, ranked two ways",
      subtitle = glue("Rank 1 = most exposed / most at risk. Highlighted: the {n_highlight} countries whose rank moves most.\n",
                      "Spearman ρ = {rho} across {n} OECD countries with at least three qualifying cities, 2020."),
      x = NULL, y = NULL
    ) +
    oecd_theme() +
    theme(panel.grid.major = element_blank(), axis.text = element_blank())
  save_oecd(p, "fig2_exposure_vs_risk",
            "Source: OECD CFE functional urban area dataflows (heat stress, urban heat island, green area, population by age), SDMX API. Cities > 100,000 residents.",
            out_dir, w = 9.5, h = 10)
}

# ── Figure 3: why the number changed — waterfall (Part-to-whole) ────────────

fig_decomposition <- function(decomp_path = "out/env/decomposition.csv",
                              out_dir = "docs/figures") {
  d <- read_csv(decomp_path, show_col_types = FALSE)
  v <- setNames(d$value, d$term)
  y0 <- d$y0[1]; y1 <- d$y1[1]; hot <- d$hot_days[1]

  steps <- tibble(
    label = c(as.character(y0), "Cities got hotter\n(climate)", "Populations aged\n(demography)", as.character(y1)),
    value = c(v["start"], v["climate (Shapley)"], v["ageing (Shapley)"], v["end"]) / 1e6,
    kind  = c("Level", "Change", "Change", "Level")
  ) %>%
    mutate(idx = row_number(),
           end = c(value[1], value[1] + value[2], value[1] + value[2] + value[3], value[4]),
           start = c(0, value[1], value[1] + value[2], 0),
           txt = ifelse(kind == "Level", sprintf("%.1f m", value), sprintf("%+.1f m", value)))

  bw <- 0.34   # thin bars: air in the slot, per the mark spec
  p <- ggplot(steps) +
    geom_segment(data = steps[1:3, ], aes(x = idx + bw, xend = idx + 1 - bw, y = end, yend = end),
                 colour = OECD_GREY, linewidth = 0.5) +
    geom_rect(aes(xmin = idx - bw, xmax = idx + bw, ymin = start, ymax = end, fill = kind)) +
    geom_text(aes(x = idx, y = pmax(start, end), label = txt), vjust = -0.6, size = 3.6,
              fontface = "bold", colour = OECD_INK) +
    scale_x_continuous(breaks = steps$idx, labels = steps$label) +
    scale_fill_manual(values = c(Level = OECD_PRIMARY, Change = OECD_ACCENT), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12)), breaks = seq(0, 100, 25)) +
    labs(
      title = glue("Older people in hot OECD cities, {y0} to {y1}:\nmost of the rise is ageing, not warming"),
      subtitle = glue("Residents aged 65+ in functional urban areas with more than {hot} days a year of strong heat stress (UTCI ≥ 32 °C), millions.\n",
                      "Shapley split of the change into climate and demography (interaction shared). Balanced panel of {d$n_cities[1]} cities."),
      x = NULL, y = "Millions of people aged 65+"
    ) +
    oecd_theme() +
    theme(panel.grid.major.x = element_blank(), legend.position = "top",
          legend.justification = "left", legend.margin = margin(0, 0, 0, 0),
          axis.text.x = element_text(colour = OECD_INK, size = 10))
  save_oecd(p, "fig3_decomposition",
            "Source: OECD CFE DSD_FUA_CLIM@DF_HEAT_STRESS and DSD_FUA_DEMO@DF_AGE_SEX, retrieved via the SDMX API",
            out_dir, w = 9, h = 6.2)
}

fig_all <- function(snapshot_path) {
  fig_coverage(snapshot_path)
  fig_rank_test()
  fig_decomposition()
}
