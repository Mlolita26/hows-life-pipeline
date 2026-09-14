# 07_site.R — build the GitHub Pages site (docs/index.html) from pipeline outputs.
#
# Static HTML, no framework, no build step, no external scripts: the kind of
# page an institutional CMS can embed and a communications team can maintain.
# Accessibility is a requirement, not a finish: semantic headings, a skip link,
# scoped table headers, alt text on every figure, colour never the sole carrier
# of meaning, and a language toggle (EN / FR) driven by one content object.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(glue)
  library(cli)
})

# ── helpers ──────────────────────────────────────────────────────────────────

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
fmt  <- function(x, d = 0) format(round(x, d), big.mark = ",", nsmall = d, trim = TRUE)

# Bilingual span: both languages are in the DOM; CSS shows the active one.
bi <- function(en, fr) glue('<span lang="en">{en}</span><span lang="fr">{fr}</span>')

html_table <- function(df, caption_en, caption_fr, digits = 1, id = NULL) {
  df <- as.data.frame(df)
  num <- vapply(df, is.numeric, logical(1))
  for (c in names(df)[num]) df[[c]] <- ifelse(is.na(df[[c]]), "", fmt(df[[c]], digits))
  head <- paste0("<th scope=\"col\">", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) paste0("<td>", esc(as.character(r)), "</td>", collapse = ""))
  glue('<div class="tablewrap"><table{if (is.null(id)) "" else glue(" id=\\"{id}\\"")}>',
       '<caption>{bi(caption_en, caption_fr)}</caption>',
       '<thead><tr>{head}</tr></thead><tbody>',
       '{paste0("<tr>", rows, "</tr>", collapse = "")}</tbody></table></div>')
}

figure <- function(name, alt_en, alt_fr, cap_en, cap_fr) {
  glue('<figure><picture>',
       '<source srcset="figures/{name}.svg" type="image/svg+xml">',
       '<img src="figures/{name}.png" alt="{esc(alt_en)}" loading="lazy" width="1600">',
       '</picture><figcaption>{bi(cap_en, cap_fr)} ',
       '<a href="figures/{name}.png">PNG</a> · <a href="figures/{name}.svg">SVG</a></figcaption></figure>')
}

stat <- function(value, label_en, label_fr) {
  glue('<div class="stat"><div class="v">{value}</div><div class="l">{bi(label_en, label_fr)}</div></div>')
}

# ── build ────────────────────────────────────────────────────────────────────

build_site <- function(out_dir = "docs", data_dir = "out", raw_dir = "data/raw",
                       repo_url = "https://github.com/Mlolita26/hows-life-pipeline") {
  dir.create(file.path(out_dir, "data"), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(out_dir, ".nojekyll"))

  # copy downloadable outputs
  for (f in c(list.files(data_dir, "\\.csv$|\\.md$", full.names = TRUE),
              list.files(file.path(data_dir, "env"), "\\.csv$", full.names = TRUE),
              file.path(raw_dir, "manifest.csv"))) {
    if (file.exists(f)) file.copy(f, file.path(out_dir, "data", basename(f)), overwrite = TRUE)
  }
  fua_m <- file.path(raw_dir, "fua", "manifest.csv")
  if (file.exists(fua_m)) file.copy(fua_m, file.path(out_dir, "data", "manifest_fua.csv"), overwrite = TRUE)

  # inputs
  man   <- read_csv(file.path(raw_dir, "manifest.csv"), show_col_types = FALSE) %>% slice_tail(n = 1)
  snap  <- hsl_read(man$path)
  iss   <- read_csv(file.path(data_dir, "issues.csv"), show_col_types = FALSE)
  cov   <- read_csv(file.path(data_dir, "coverage.csv"), show_col_types = FALSE)
  gaps  <- read_csv(file.path(data_dir, "gaps.csv"), show_col_types = FALSE)
  cty   <- read_csv(file.path(data_dir, "env", "country_indicator.csv"), show_col_types = FALSE)
  sm    <- read_csv(file.path(data_dir, "env", "rank_test_summary.csv"), show_col_types = FALSE)
  mov   <- read_csv(file.path(data_dir, "env", "rank_test_movers.csv"), show_col_types = FALSE)
  lay   <- read_csv(file.path(data_dir, "env", "layer_sensitivity.csv"), show_col_types = FALSE)
  wts   <- read_csv(file.path(data_dir, "env", "weight_sensitivity.csv"), show_col_types = FALSE)
  dec   <- read_csv(file.path(data_dir, "env", "decomposition.csv"), show_col_types = FALSE)
  dsens <- read_csv(file.path(data_dir, "env", "decomposition_sensitivity.csv"), show_col_types = FALSE)
  cs    <- dsens %>% filter(!grepl("^context", metric))
  cs_lo <- round(100 * min(cs$climate_share)); cs_hi <- round(100 * max(cs$climate_share))
  ctx   <- dsens %>% filter(grepl("^context", metric), grepl("mean", heat_basis))
  pop_growth <- round(100 * (ctx$end / ctx$start - 1)); heat_growth <- round(100 * (ctx$ageing / ctx$climate - 1))
  city  <- read_csv(file.path(data_dir, "env", "city_layers_scored.csv"), show_col_types = FALSE)
  hsl   <- read_csv(file.path(data_dir, "env", "hsl_schema_rows.csv"), show_col_types = FALSE)

  v <- setNames(dec$value, dec$term)
  smv <- setNames(sm$value, sm$metric)
  dom_lag <- cov %>% group_by(domain) %>% summarise(mean_lag = mean(lag_years), .groups = "drop") %>%
    arrange(desc(mean_lag))
  env_lag <- dom_lag$mean_lag[dom_lag$domain == "HSL_9"]
  n_dist <- snap %>% mutate(b = sex != "_T" | age != "_T" | education_lev != "_T") %>%
    group_by(domain) %>% summarise(p = 100 * mean(b), .groups = "drop")
  env_dist <- n_dist$p[n_dist$domain == "HSL_9"]

  top_cities <- city %>% arrange(desc(risk_full)) %>% head(12) %>%
    transmute(City = sub(" \\(.*\\)$", "", name), Country = iso3,
              `Heat days` = round(heat_days), `Night UHI (°C)` = round(uhi_night, 1),
              `Green m²/person` = round(green_m2), `% 65+` = round(pct65, 1),
              `65+ (thousands)` = round(pop65 / 1e3), `Risk score` = risk_full)

  country_tbl <- cty %>%
    transmute(Country = iso3, Cities = n_cities,
              `Heat days (pop-weighted)` = exposure_only,
              `Urban 65+ in top-risk cities (%)` = risk_share65,
              `Urban 65+ at risk (thousands)` = risk_n65 / 1e3,
              `Rank: exposure` = rank_exposure, `Rank: risk` = rank_risk, `Shift` = rank_shift) %>%
    arrange(`Rank: risk`)

  issues_tbl <- iss %>% count(rule, severity, name = "n") %>% arrange(desc(n))
  lag_tbl <- cov %>% arrange(desc(lag_years)) %>% head(10) %>%
    transmute(Domain = domain, Indicator = measure, Countries = n_countries,
              `Latest year` = latest_year, `Lag (years)` = lag_years)
  decomp_tbl <- tibble(
    Term = c(glue("{dec$y0[1]} (start)"), "Cities got hotter (Shapley)", "Populations aged (Shapley)",
             "Interaction (shared in Shapley terms)", glue("{dec$y1[1]} (end)")),
    `Millions aged 65+` = c(v["start"], v["climate (Shapley)"], v["ageing (Shapley)"], v["interaction"], v["end"]) / 1e6
  )

  css <- '
  :root{--ink:#141a24;--navy:#0d2240;--muted:#4a5568;--bg:#f7f9fb;--card:#fff;--line:#dfe5ec;--acc:#245c99;--acc2:#5ba3de}
  *{box-sizing:border-box}html{scroll-behavior:smooth}
  body{margin:0;font:16px/1.55 system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:var(--ink);background:var(--bg)}
  a{color:var(--acc)}a:focus,button:focus{outline:3px solid #f2c14e;outline-offset:2px}
  .skip{position:absolute;left:-999px;top:0;background:#fff;padding:.5rem 1rem;z-index:10}.skip:focus{left:1rem}
  header{background:var(--navy);color:#fff;padding:1.4rem 1rem}
  .wrap{max-width:1080px;margin:0 auto;padding:0 1rem}
  header h1{margin:.2rem 0 .3rem;font-size:1.65rem;line-height:1.25}header p{margin:0;color:#c9d6e6;max-width:70ch}
  .toggle{display:flex;gap:.4rem;justify-content:flex-end;margin-bottom:.6rem}
  .toggle button{background:transparent;color:#fff;border:1px solid #5ba3de;border-radius:999px;padding:.25rem .8rem;cursor:pointer;font:inherit}
  .toggle button[aria-pressed="true"]{background:#fff;color:var(--ink);border-color:#fff}
  main{padding:1.5rem 0 3rem}section{margin:2.2rem 0}h2{font-size:1.35rem;border-bottom:3px solid var(--acc2);padding-bottom:.3rem;margin:0 0 .8rem;color:var(--navy)}
  h3{font-size:1.05rem;margin:1.2rem 0 .4rem}p{max-width:75ch}.lead{font-size:1.1rem}
  .stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:.8rem;margin:1rem 0}
  .stat{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:.9rem 1rem}
  .stat .v{font-size:1.7rem;font-weight:700;color:var(--acc)}.stat .l{color:var(--muted);font-size:.92rem}
  figure{margin:1.2rem 0;background:var(--card);border:1px solid var(--line);border-radius:8px;padding:.8rem}
  figure img{max-width:100%;height:auto;display:block}figcaption{color:var(--muted);font-size:.9rem;margin-top:.5rem}
  .tablewrap{overflow-x:auto;margin:.8rem 0}table{border-collapse:collapse;width:100%;font-size:.92rem;background:var(--card)}
  caption{text-align:left;font-weight:600;padding:.4rem 0;color:var(--ink)}th,td{border:1px solid var(--line);padding:.4rem .6rem;text-align:left}
  th{background:#e9f1f9;color:var(--navy)}td:nth-child(n+3){text-align:right;font-variant-numeric:tabular-nums}
  .note{background:#eef5fc;border-left:4px solid var(--acc);padding:.7rem 1rem;border-radius:4px}
  .box{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:1rem 1.2rem}
  code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:.9em}pre{background:#eef2f6;padding:.8rem;border-radius:6px;overflow-x:auto}
  footer{color:#c9d6e6;background:var(--navy);font-size:.9rem;padding:1.2rem 0;margin-top:2rem}footer a{color:#fff}
  html[data-lang="en"] [lang="fr"]{display:none}html[data-lang="fr"] [lang="en"]{display:none}
  @media (prefers-reduced-motion:reduce){html{scroll-behavior:auto}}
  '

  js <- '
  (function(){
    var root=document.documentElement;
    function setLang(l){root.setAttribute("data-lang",l);root.setAttribute("lang",l);
      document.querySelectorAll(".toggle button").forEach(function(b){b.setAttribute("aria-pressed",String(b.dataset.lang===l));});
      try{localStorage.setItem("hl-lang",l);}catch(e){}}
    var saved=null;try{saved=localStorage.getItem("hl-lang");}catch(e){}
    setLang(saved==="fr"?"fr":"en");
    document.querySelectorAll(".toggle button").forEach(function(b){b.addEventListener("click",function(){setLang(b.dataset.lang);});});
  })();'

  html <- paste0(
'<!doctype html><html lang="en" data-lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>How\'s Life? — automated refresh, QA and environmental well-being</title>
<meta name="description" content="Automated refresh and quality assurance for the OECD How\'s Life? database, and a risk-based reading of its environmental dimension built from OECD city data.">
<style>', css, '</style></head><body>
<a class="skip" href="#main">', bi("Skip to content", "Aller au contenu"), '</a>
<header><div class="wrap">
  <div class="toggle" role="group" aria-label="Language">
    <button type="button" data-lang="en" aria-pressed="true">English</button>
    <button type="button" data-lang="fr" aria-pressed="false">Français</button>
  </div>
  <h1>', bi("How\'s Life? — automated refresh, quality assurance, and a risk-based reading of environmental well-being",
            "How\'s Life? — actualisation automatisée, assurance qualité, et une lecture du bien-être environnemental fondée sur le risque"), '</h1>
  <p>', bi("Built entirely on OECD public data through the SDMX API. Every number on this page traces to a recorded retrieval. No model touches a published statistic.",
           "Construit entièrement à partir des données publiques de l\'OCDE via l\'API SDMX. Chaque chiffre de cette page renvoie à un téléchargement documenté. Aucun modèle ne touche une statistique publiée."), '</p>
  <p style="margin-top:.6rem"><a href="', repo_url, '" style="color:#fff">', bi("Code and data on GitHub", "Code et données sur GitHub"), ' →</a></p>
</div></header>
<main id="main" class="wrap">

<section aria-labelledby="s0"><h2 id="s0">', bi("At a glance", "En bref"), '</h2>
<div class="stats">',
  stat(fmt(nrow(snap)), "observations fetched from the How\'s Life? database", "observations extraites de la base How\'s Life?"),
  stat(glue("{n_distinct(snap$ref_area)} / {n_distinct(snap$measure)}"), "reference areas / indicators", "zones de référence / indicateurs"),
  stat(glue("{man$seconds}s"), "to retrieve the full database from the API", "pour extraire toute la base via l\'API"),
  stat(fmt(sum(gaps$status == "missing")), "country × indicator cells with no data", "cellules pays × indicateur sans donnée"),
  stat(fmt(sum(gaps$status == "stale")), "cells more than three years old", "cellules datant de plus de trois ans"),
  stat(glue("{round(env_lag, 1)} y"), "mean reporting lag, Environmental quality — the worst dimension", "retard moyen, Qualité de l\'environnement — la pire dimension"),
  stat(glue("{round(env_dist)}%"), "of environmental observations carry any population breakdown", "des observations environnementales avec une ventilation par population"),
  stat(glue("{n_distinct(city$fua)} / {n_distinct(city$iso3)}"), "cities / countries with every risk layer", "villes / pays avec toutes les couches de risque"),
  stat(glue("ρ = {smv['spearman_rho']}"), glue("rank correlation, exposure alone vs risk, {smv['n_countries']} countries with ≥3 cities"), glue("corrélation de rang, exposition seule vs risque, {smv['n_countries']} pays avec ≥3 villes")),
  stat(glue("{fmt(v['start']/1e6,1)} → {fmt(v['end']/1e6,1)} m"), glue("urban residents 65+ in hot cities, {dec$y0[1]}–{dec$y1[1]}"), glue("citadins de 65 ans et plus dans des villes chaudes, {dec$y0[1]}–{dec$y1[1]}")),
'</div></section>

<section aria-labelledby="s1"><h2 id="s1">', bi("1. Keeping the database fresh — automatically", "1. Maintenir la base à jour — automatiquement"), '</h2>
<p class="lead">', bi("The How\'s Life? database is fully available over the OECD SDMX REST API. This pipeline treats the API as the source of record: it fetches every observation, records the URL, timestamp and SHA-256 of what it received, validates it against a rule set, and — when run again — reports what was added, revised, reflagged or withdrawn.",
                      "La base How\'s Life? est entièrement accessible via l\'API SDMX REST de l\'OCDE. Ce pipeline traite l\'API comme la source de référence : il extrait chaque observation, enregistre l\'URL, l\'horodatage et le SHA-256 de ce qu\'il a reçu, le valide selon un jeu de règles et — relancé — signale ce qui a été ajouté, révisé, re-signalé ou retiré."), '</p>',
  figure("fig1_coverage",
         "Heatmap of the How's Life? database: indicators on the vertical axis grouped by dimension, years 2005 to 2025 on the horizontal axis, tiles shaded by the number of countries reporting. Environmental indicators show few recent years.",
         "Carte thermique de la base How's Life? : indicateurs regroupés par dimension, années 2005 à 2025, teinte selon le nombre de pays déclarants.",
         "Figure 1. Where the data is and where it is not. Environmental quality has the longest reporting lag of any dimension; access to green space (9_1) was last published in 2018 for 29 of 47 countries.",
         "Figure 1. Où sont les données et où elles manquent. La qualité de l\'environnement a le plus long retard de déclaration ; l\'accès aux espaces verts (9_1) date de 2018 pour 29 pays sur 47."),
  html_table(issues_tbl, "Validation issues in the latest snapshot, by rule. Comparability flags are reported as information, not failures.",
             "Anomalies de validation du dernier instantané, par règle. Les indicateurs de comparabilité sont de l\'information, pas des échecs.", 0),
  html_table(lag_tbl, "The ten indicators with the longest reporting lag.", "Les dix indicateurs au plus long retard de déclaration.", 0),
'<p>', bi(glue("The gaps file — {fmt(sum(gaps$status=='missing'))} empty and {fmt(sum(gaps$status=='stale'))} stale country × indicator cells — is the first draft of the data request the OECD sends to national statistical offices each year."),
           glue("Le fichier des lacunes — {fmt(sum(gaps$status=='missing'))} cellules vides et {fmt(sum(gaps$status=='stale'))} obsolètes — est le premier brouillon de la demande de données envoyée chaque année aux offices statistiques nationaux.")), '</p>
<div class="note">', bi("<strong>A rule that was wrong.</strong> The first version treated every percentage unit as 0–100 and flagged four errors on indicator 2_2, the gender wage gap. The values were negative — and correct: women in those deciles out-earned men in Luxembourg, Bulgaria and Croatia. A share of a population cannot be negative; a percentage relative to another group can. The rule now separates the two, a regression test pins the case, and the story stays in the README because it is the argument for calibrating rules against the full live series.",
                       "<strong>Une règle erronée.</strong> La première version traitait tout pourcentage comme borné à 0–100 et signalait quatre erreurs sur l\'indicateur 2_2, l\'écart salarial femmes-hommes. Les valeurs étaient négatives — et justes. La règle distingue désormais part d\'une population et pourcentage relatif ; un test de régression fixe le cas."), '</div>
</section>

<section aria-labelledby="s2"><h2 id="s2">', bi("2. Exposure is not risk", "2. L\'exposition n\'est pas le risque"), '</h2>
<p class="lead">', bi("The environmental dimension of the framework has three indicators — green space, air pollution, extreme temperature — and all three measure what people are exposed to. None measures who is sensitive to it or what protects them. It is also the only dimension with no distributional data at all: no breakdown by sex, age or education, and no inequality variant.",
                      "La dimension environnementale du cadre compte trois indicateurs — espaces verts, pollution de l\'air, températures extrêmes — qui mesurent tous l\'exposition. Aucun ne mesure qui y est sensible ni ce qui protège. C\'est aussi la seule dimension sans aucune donnée de répartition."), '</p>
<p>', bi("The OECD already publishes the missing layers, at city level, in another directorate. This section joins them and assigns each to an IPCC risk component:",
         "L\'OCDE publie déjà les couches manquantes, au niveau des villes, dans une autre direction. Cette section les assemble et attribue à chacune une composante du risque au sens du GIEC :"), '</p>
<div class="tablewrap"><table><caption>', bi("Layers and their risk component", "Couches et composante du risque"), '</caption>
<thead><tr><th scope="col">', bi("Component", "Composante"), '</th><th scope="col">', bi("Layer", "Couche"), '</th><th scope="col">', bi("OECD dataflow", "Flux de données OCDE"), '</th></tr></thead><tbody>
<tr><td>', bi("Hazard", "Aléa"), '</td><td>', bi("Days per year of strong heat stress (UTCI ≥ 32 °C), population-weighted", "Jours par an de fort stress thermique (UTCI ≥ 32 °C), pondérés par la population"), '</td><td><code>DSD_FUA_CLIM@DF_HEAT_STRESS</code></td></tr>
<tr><td>', bi("Hazard modifier", "Modulateur de l\'aléa"), '</td><td>', bi("Summer night-time urban heat island (°C)", "Îlot de chaleur urbain, nuits d\'été (°C)"), '</td><td><code>DSD_FUA_ENV@DF_UHI</code></td></tr>
<tr><td>', bi("Exposure", "Exposition"), '</td><td>', bi("Resident population, total and by age band", "Population résidente, totale et par tranche d\'âge"), '</td><td><code>DSD_FUA_DEMO@DF_AGE_SEX</code></td></tr>
<tr><td>', bi("Vulnerability (sensitivity)", "Vulnérabilité (sensibilité)"), '</td><td>', bi("Share of residents aged 65+", "Part des résidents de 65 ans et plus"), '</td><td><code>DSD_FUA_DEMO@DF_AGE_SEX</code></td></tr>
<tr><td>', bi("Adaptive capacity", "Capacité d\'adaptation"), '</td><td>', bi("Green area per person; cooling degree days (energy burden)", "Espaces verts par habitant ; degrés-jours de refroidissement"), '</td><td><code>DSD_FUA_ENV@DF_GREEN_AREA</code>, <code>DSD_FUA_ENER@DF_CDD_HDD</code></td></tr>
</tbody></table></div>',
  figure("fig2_exposure_vs_risk",
         "Slope chart: each country is a line from its rank on heat exposure alone (left) to its rank on risk to residents aged 65 and over (right). The six countries whose rank moves most are highlighted and labelled with the size of the move; the rest are grey.",
         "Graphique de pentes : chaque pays est une ligne de son rang sur l’exposition seule (gauche) à son rang sur le risque pour les 65 ans et plus (droite). Les six pays dont le rang bouge le plus sont mis en évidence.",
         glue("Figure 2. The same countries ranked by exposure alone (what indicator 9_3 measures) and by risk to older residents. Spearman ρ = {smv['spearman_rho']} across {smv['n_countries']} countries. {smv['misplaced_share65_pct']}% of at-risk urban residents aged 65+ live in countries the exposure-only ranking places outside its top quintile."),
         glue("Figure 2. Les mêmes pays classés par exposition seule et par risque pour les aînés. ρ de Spearman = {smv['spearman_rho']} sur {smv['n_countries']} pays.")),
'<h3>', bi("What the test says — honestly", "Ce que dit le test — honnêtement"), '</h3>
<p>', bi(glue("The two rankings are correlated (ρ = {smv['spearman_rho']}) but far from identical: about {round(100*(1-as.numeric(smv['spearman_rho'])^2))}% of the rank variance differs. The countries with the largest older populations at risk — Japan, Italy, Spain — are hot on any measure, so the share of at-risk people <em>misplaced</em> by exposure alone is small ({smv['misplaced_share65_pct']}%). What moves is <em>which</em> hot countries matter: the United States and Mexico fall sharply once vulnerability and green space are counted; Japan rises to first."),
         glue("Les deux classements sont corrélés (ρ = {smv['spearman_rho']}) mais loin d\'être identiques. Les pays aux plus grandes populations âgées à risque — Japon, Italie, Espagne — sont chauds quelle que soit la mesure ; la part des personnes à risque <em>mal classées</em> par l\'exposition seule est donc faible ({smv['misplaced_share65_pct']} %). Ce qui bouge, c\'est <em>quels</em> pays chauds comptent : les États-Unis et le Mexique reculent nettement ; le Japon passe premier.")), '</p>',
  html_table(mov %>% transmute(Country = iso3, Cities = n_cities, `Heat days` = exposure_only,
                               `Urban 65+ in top-risk cities (%)` = risk_share65,
                               `Urban 65+ at risk (thousands)` = risk_n65 / 1e3,
                               `Rank: exposure` = rank_exposure, `Rank: risk` = rank_risk, Shift = rank_shift),
             "Countries whose rank changes most between the two readings.", "Pays dont le rang change le plus entre les deux lectures.", 1),
  html_table(lay %>% transmute(Step = step, Components = components, `ρ vs final ranking` = rho_vs_final, `ρ vs previous step` = rho_vs_previous),
             "Adding one layer at a time: how the country ranking moves at each step (65+-weighted mean score). The share aged 65+ moves it most.",
             "Ajout d\'une couche à la fois : déplacement du classement des pays à chaque étape.", 3),
  html_table(wts %>% transmute(Weighting = weighting, `ρ vs equal weights` = rho_vs_equal),
             "Sensitivity to the weighting scheme. Equal weights are a choice, not a finding.",
             "Sensibilité au schéma de pondération. Les poids égaux sont un choix, pas un résultat.", 3),
  html_table(top_cities, "The twelve highest-risk cities for older residents, 2020, with every component shown.",
             "Les douze villes au risque le plus élevé pour les aînés, 2020, avec chaque composante.", 1),
'<h3>', bi("The country-level indicator, in the framework\'s own shape", "L\'indicateur pays, dans le format du cadre"), '</h3>
<p>', bi("The Well-being Data Monitor is a country-level tool, so the headline is a country number: the share of a country\'s urban residents aged 65+ living in cities in the global top quintile of risk. It is written in How\'s Life? column grammar with <code>OBS_STATUS = E</code> (estimated). This is evidence and a method, not a proposed official statistic; adopting any indicator goes through the Committee on Statistics and Statistical Policy and national statistical offices.",
         "Le Moniteur des données sur le bien-être est un outil au niveau des pays ; le chiffre principal est donc national : la part des citadins de 65 ans et plus vivant dans des villes du quintile supérieur de risque. Il est écrit dans la grammaire de How\'s Life? avec <code>OBS_STATUS = E</code>. C\'est une preuve et une méthode, pas une statistique officielle proposée."), '</p>',
  html_table(country_tbl, "Country indicator, 2020. Countries with at least three qualifying cities.", "Indicateur pays, 2020. Pays comptant au moins trois villes qualifiées.", 1),
'<details><summary>', bi("Sample of the HSL-schema rows", "Extrait des lignes au format HSL"), '</summary>',
  html_table(head(hsl, 12), "First rows of out/env/hsl_schema_rows.csv", "Premières lignes de out/env/hsl_schema_rows.csv", 2),
'</details>
</section>

<section aria-labelledby="s3"><h2 id="s3">', bi("3. Why it changed: climate or demography?", "3. Pourquoi cela a changé : climat ou démographie ?"), '</h2>',
  figure("fig3_decomposition",
         "Waterfall chart: older residents in hot cities in 2010, the increase attributed to cities getting hotter, the increase attributed to populations ageing, and the 2020 total.",
         "Graphique en cascade : aînés dans des villes chaudes en 2010, hausse due au réchauffement des villes, hausse due au vieillissement, total 2020.",
         glue("Figure 3. Residents aged 65+ in OECD cities with more than {dec$hot_days[1]} days a year of strong heat stress rose from {fmt(v['start']/1e6,1)} to {fmt(v['end']/1e6,1)} million between {dec$y0[1]} and {dec$y1[1]} (+{round(100*(v['end']-v['start'])/v['start'])}%). Shapley decomposition: ageing +{fmt(v['ageing (Shapley)']/1e6,1)} m, warming +{fmt(v['climate (Shapley)']/1e6,1)} m. Roughly a quarter climate, three-quarters demography; heat averaged over five years around each date. Two of the four transitions named in WISE\'s mission, pulling the same way."),
         glue("Figure 3. Les 65 ans et plus dans les villes chaudes de l\'OCDE sont passés de {fmt(v['start']/1e6,1)} à {fmt(v['end']/1e6,1)} millions entre {dec$y0[1]} et {dec$y1[1]}. Décomposition de Shapley : vieillissement +{fmt(v['ageing (Shapley)']/1e6,1)} M, réchauffement +{fmt(v['climate (Shapley)']/1e6,1)} M.")),
  html_table(decomp_tbl, "Both counterfactual orderings are in the data file; the Shapley values are their average and sum exactly to the change.",
             "Les deux ordres contrefactuels sont dans le fichier ; les valeurs de Shapley en sont la moyenne et somment exactement au changement.", 2),

'<h3>', bi("Does the split survive other choices? Challenged - and largely yes", "Le partage resiste-t-il a d\'autres choix ? Mis a l\'epreuve - et largement oui"), '</h3>
<p>', bi(glue("A binary threshold only counts cities that <em>cross</em> it: a city already hot in {dec$y0[1]} that got hotter adds nothing to the climate term. And a single-year baseline is noisy - {dec$y0[1]} was a hot year. So the split was re-run across thresholds from 10 to 60 days, with single-year and five-year-mean heat, and on a continuous measure - person-days of heat for the 65+ - which does see intensification. <strong>Climate\'s share ranges from {cs_lo}% to {cs_hi}%; demography is the larger driver in every specification.</strong> Over the decade the urban population aged 65+ grew {pop_growth}% while population-weighted heat-stress days grew {heat_growth}%. This is a statement about {dec$y0[1]}-{dec$y1[1]}, the decade the baby-boom cohort crossed 65; as ageing slows and warming accelerates, the balance should shift. Demography here also includes older people moving to hot places."),
         glue("Un seuil binaire ne compte que les villes qui le <em>franchissent</em>. Le partage a donc ete recalcule pour des seuils de 10 a 60 jours, avec une chaleur annuelle ou moyennee sur cinq ans, et sur une mesure continue (personnes-jours de chaleur des 65 ans et plus). <strong>La part du climat va de {cs_lo} % a {cs_hi} % ; la demographie domine dans toutes les specifications.</strong> Sur la decennie, la population urbaine de 65 ans et plus a cru de {pop_growth} % et les jours de stress thermique de {heat_growth} %.")), '</p>',
  html_table(cs %>% transmute(Metric = metric, `Heat basis` = heat_basis, `Start` = start / 1e6, `End` = end / 1e6,
                              `Climate` = climate / 1e6, `Ageing` = ageing / 1e6, `Climate share (%)` = 100 * climate_share) %>%
               mutate(across(where(is.numeric), ~ round(.x, 1))),
             "Sensitivity of the climate / ageing split. Headcount rows in millions of people; person-days rows in billions of person-days.",
             "Sensibilite du partage climat / vieillissement. Effectifs en millions ; personnes-jours en milliards.", 1),
'</section>

<section aria-labelledby="s4"><h2 id="s4">', bi("4. Method, provenance and limits", "4. Méthode, provenance et limites"), '</h2>
<div class="box"><h3 style="margin-top:0">', bi("Where the heat numbers come from", "D\'où viennent les chiffres de chaleur"), '</h3>
<p>', bi("From the OECD\'s own metadata: the Universal Thermal Climate Index (air temperature, wind, radiation, humidity) from the Copernicus Climate Data Store thermal-comfort grids (Di Napoli et al., <code>10.24381/cds.553b7518</code>) at 0.25°; days above each threshold counted per cell; population-weighted with the Global Human Settlement Layer (GHSL 2023, <code>10.2760/098587</code>); aggregated to OECD/EU functional urban areas. The OECD\'s own caveat applies: reanalysis estimates may differ from in-situ subnational statistics.",
         "D\'après les métadonnées de l\'OCDE : indice UTCI issu des grilles de confort thermique du Copernicus Climate Data Store (Di Napoli et al.) à 0,25°, jours au-dessus de chaque seuil comptés par cellule, pondération par la population GHSL 2023, agrégation aux aires urbaines fonctionnelles OCDE/UE."), '</p>
<p>', bi("<strong>Consequence for interpretation.</strong> A city\'s published value is already a population-weighted average. Weighting across cities by the 65+ population therefore measures <em>how many older people live in hot cities</em>, not how much heat an older person personally experiences. At 0.25° a whole urban area can sit in one or two grid cells, so within-city heat islands are invisible in the hazard layer — which is exactly why the separate UHI layer matters, and why the age <em>gap</em> in exposure is near zero within countries.",
         "<strong>Conséquence pour l\'interprétation.</strong> La valeur publiée d\'une ville est déjà une moyenne pondérée par la population. Pondérer entre villes par la population de 65 ans et plus mesure donc <em>combien d\'aînés vivent dans des villes chaudes</em>, non la chaleur qu\'un aîné subit personnellement."), '</p></div>
<h3>', bi("Limits, stated first", "Limites, énoncées d\'abord"), '</h3>
<ul>
<li>', bi("Cities only. Rural populations are excluded; their vulnerability is different, not absent.", "Villes uniquement. Les populations rurales sont exclues ; leur vulnérabilité est différente, pas absente."), '</li>
<li>', bi(glue("{n_distinct(city$fua)} cities over 100,000 residents with every layer present; the heat dataset alone covers 2,554."), glue("{n_distinct(city$fua)} villes de plus de 100 000 habitants avec toutes les couches ; le seul jeu de chaleur en couvre 2 554.")), '</li>
<li>', bi("Green space is a single 2021 snapshot. Demographic coverage thins sharply after 2020, so the joint series cannot run further than the decomposition shown.", "Les espaces verts sont un instantané 2021. La couverture démographique s\'amincit fortement après 2020."), '</li>
<li>', bi("Equal weights are a choice; the weighting table shows the effect of alternatives. Components are always published alongside the score.", "Les poids égaux sont un choix ; le tableau de pondération montre l\'effet des alternatives. Les composantes sont toujours publiées avec le score."), '</li>
<li>', bi("Another OECD directorate (CFE) publishes cities-and-climate analysis. The point here is horizontality: the well-being framework uses none of it.", "Une autre direction de l\'OCDE (CFE) publie des analyses villes-climat. Le propos ici est l\'horizontalité : le cadre du bien-être n\'en utilise rien."), '</li>
</ul>
<h3>', bi("Provenance", "Provenance"), '</h3>',
  html_table(man %>% transmute(Flow = flow, Vintage = vintage, `Fetched (UTC)` = fetched_utc, Seconds = seconds, MB = round(bytes / 1e6, 1), Rows = n_rows, `SHA-256 (prefix)` = substr(sha256, 1, 16)),
             "Latest How\'s Life? snapshot. Full manifests: data/manifest.csv and data/manifest_fua.csv.", "Dernier instantané How\'s Life?. Manifestes complets : data/manifest.csv et data/manifest_fua.csv.", 1),
'</section>

<section aria-labelledby="s5"><h2 id="s5">', bi("5. Data and reproduction", "5. Données et reproduction"), '</h2>
<ul>
<li><a href="data/report.md">report.md</a> — ', bi("the QA report from the latest refresh", "le rapport QA de la dernière actualisation"), '</li>
<li><a href="data/issues.csv">issues.csv</a>, <a href="data/coverage.csv">coverage.csv</a>, <a href="data/gaps.csv">gaps.csv</a></li>
<li><a href="data/city_layers_scored.csv">city_layers_scored.csv</a>, <a href="data/country_indicator.csv">country_indicator.csv</a>, <a href="data/hsl_schema_rows.csv">hsl_schema_rows.csv</a></li>
<li><a href="data/rank_test_summary.csv">rank_test_summary.csv</a>, <a href="data/rank_test_movers.csv">rank_test_movers.csv</a>, <a href="data/layer_sensitivity.csv">layer_sensitivity.csv</a>, <a href="data/weight_sensitivity.csv">weight_sensitivity.csv</a>, <a href="data/decomposition.csv">decomposition.csv</a></li>
<li><a href="data/manifest.csv">manifest.csv</a>, <a href="data/manifest_fua.csv">manifest_fua.csv</a></li>
</ul>
<pre><code>git clone ', repo_url, '
Rscript R/run_pipeline.R      # fetch, validate, diff, report
Rscript R/run_environment.R   # city layers, country indicator, decomposition, figures, site</code></pre>
</section>
</main>
<footer><div class="wrap">', bi(glue("Built {format(Sys.Date(), '%d %B %Y')} from OECD public data. Method borrowed from the Africa Agriculture Adaptation Atlas (hazard → threshold → exposure → aggregation) and from dataset-assessment work for the Global Goal on Adaptation; data entirely OECD. Licence: MIT (code), OECD terms (data)."),
                                   glue("Construit le {format(Sys.Date(), '%d %B %Y')} à partir des données publiques de l\'OCDE. Méthode empruntée à l\'Africa Agriculture Adaptation Atlas et aux travaux d\'évaluation des données pour l\'Objectif mondial d\'adaptation ; données entièrement OCDE.")), '</div></footer>
<script>', js, '</script>
</body></html>')

  writeLines(html, file.path(out_dir, "index.html"), useBytes = TRUE)
  cli_alert_success("Wrote {.path {file.path(out_dir, 'index.html')}} ({round(nchar(html)/1e3)} kB)")
  invisible(file.path(out_dir, "index.html"))
}
