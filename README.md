# How's Life? — automated refresh, quality assurance, and a risk-based reading of environmental well-being

**Site (figures, tables, downloads, EN/FR):** https://mlolita26.github.io/hows-life-pipeline/

A reproducible R pipeline for the OECD **How's Life?** well-being database, and an
analysis of its weakest dimension built from data the OECD already publishes.
Everything comes from the OECD SDMX API. Every number traces to a recorded
retrieval. **No model touches a published statistic.**

```
Rscript R/run_pipeline.R       # fetch, validate, diff against the previous vintage, report
Rscript R/run_environment.R    # city layers, country indicator, decomposition, figures, site
```

---

## 1. Keep the database fresh — automatically

The How's Life? database (`OECD.WISE.WDP, DSD_HSL@DF_HSL_CWB`) is fully available
over the SDMX REST API, yet it is usually moved around as a hand-downloaded CSV
with no provenance. This pipeline treats the API as the source of record.

| Stage | What it does |
|---|---|
| **Fetch** | Whole database in one call — **111,266 observations, 47 reference areas, 68 indicators, under 5 seconds** — stamped with URL, UTC time, SHA-256 and row count |
| **Validate** | 9 rules over units of measure, the SDMX `CL_OBS_STATUS` codelist, duplicate keys and value ranges. Comparability flags (7,904 *definition differs*, 1,749 *series break*) travel with the data as `info` rather than being dropped |
| **Coverage & gaps** | Reporting lag per indicator; **657 empty and 1,127 stale** country × indicator cells — the first draft of the annual data request to national statistical offices |
| **Diff** | Between vintages: `added` / `removed` (withdrawn — the one people miss) / `revised` / `reflagged` / `stable`, plus a material-revision filter |
| **CI** | Quarterly GitHub Actions run: tests → fetch → validate → **fail on any error** → analyse → publish the site → commit the vintage |

### A rule that was wrong

The first version said "percentages are 0–100" and raised four errors on `2_2`,
the **gender wage gap**, unit *percentage of wages of men in the same decile*.
The values were negative in Luxembourg, Bulgaria and Croatia — and correct. A
share of a population cannot be negative; a percentage relative to another group
can. The rule now separates `PT_POP*` from signed `PT_*`, a regression test pins
the case, and the story stays here because it is the argument for calibrating
rules against the full live series rather than a sample.

---

## 2. Exposure is not risk

The framework's environmental dimension has three indicators — green space, air
pollution, extreme temperature — and all three measure what people are *exposed*
to. None measures who is sensitive or what protects them. Two things fall out of
the pipeline's own diagnostics:

- **Environmental quality is the least current dimension** (mean reporting lag
  5.3 years; `9_1` green space last published 2018 for 29 of 47 countries).
- **It is the only dimension with no distributional data at all** — 0% of its
  observations carry a sex/age/education breakdown, and it has no `_VER` / `_DEP`
  inequality variant. Ten of eleven dimensions do.

The OECD already publishes the missing layers at city level, in another
directorate (`OECD.CFE.EDS`). This module joins them and tags each with its IPCC
risk component:

| Component | Layer | Dataflow |
|---|---|---|
| Hazard | Days/year of strong heat stress (UTCI ≥ 32 °C), population-weighted | `DSD_FUA_CLIM@DF_HEAT_STRESS` |
| Hazard modifier | Summer night-time urban heat island (°C) | `DSD_FUA_ENV@DF_UHI` |
| Exposure | Population by age band | `DSD_FUA_DEMO@DF_AGE_SEX` |
| Vulnerability | Share aged 65+ | `DSD_FUA_DEMO@DF_AGE_SEX` |
| Adaptive capacity | Green m²/person; cooling degree days | `DSD_FUA_ENV@DF_GREEN_AREA`, `DSD_FUA_ENER@DF_CDD_HDD` |

**955 cities over 100,000 residents in 34 countries** have every layer for 2020.

### The test, with a number

Rank countries two ways from the same cities: by heat exposure alone (what `9_3`
measures) and by risk to residents aged 65+. **Spearman ρ = 0.775** across 30
countries — correlated, but about 40% of the rank variance differs. Japan rises to
first; the United States and Mexico fall sharply once vulnerability and green
space count. Because the largest at-risk older populations (Japan, Italy, Spain)
are hot on any measure, only **3.1%** of at-risk urban 65+ live in countries the
exposure-only ranking places outside its top quintile. Both facts are reported;
neither is hidden. Layer-by-layer and weighting sensitivities are in
`out/env/`.

The headline is written **in the framework's own grammar** — country level,
How's Life? column names, `OBS_STATUS = E`: `ENV_HEATRISK_65_SH`, the share of a
country's urban 65+ living in global top-quintile-risk cities. It is evidence and
a method, not a proposed official statistic.

### Why it changed

Residents aged 65+ in OECD cities with more than 30 days/year of strong heat
stress: **60.3 → 86.4 million, 2010 to 2020 (+43%)**, on a balanced panel of
1,022 cities. Shapley decomposition: **ageing +20.3 m, warming +5.8 m**, both
orderings reported. Two of the four transitions in WISE's mission, pulling the
same way.

---

## Where the heat numbers come from

From the OECD's own metadata: UTCI from the Copernicus Climate Data Store
thermal-comfort grids (Di Napoli et al., `10.24381/cds.553b7518`) at 0.25°,
days above threshold counted per cell, population-weighted with GHSL 2023
(`10.2760/098587`), aggregated to OECD/EU functional urban areas. Consequence: a
city value is already a population-weighted mean, so weighting across cities by
the 65+ population measures **how many older people live in hot cities**, not the
heat an older person personally experiences. At 0.25° within-city heat islands
are invisible in the hazard layer — which is why the separate UHI layer exists.

## Limits, stated first

Cities only. 955 of 2,554 heat-covered cities carry every layer. Green space is a
single 2021 snapshot; demographic coverage thins after 2020. Equal weights are a
choice (sensitivity reported). Another OECD directorate publishes cities-and-
climate work — the point here is horizontality: the well-being framework uses
none of it. Adopting any indicator runs through the Committee on Statistics and
Statistical Policy and national statistical offices; this repository proposes
evidence, not adoption.

## Method borrowed, data not

The hazard → threshold → exposure → aggregation structure is the one used in the
[Africa Agriculture Adaptation Atlas](https://github.com/AdaptationAtlas)
(`hazards_prototype`, `atlas_exposure`). The dataset-assessment lens — can the
data that exists carry this indicator framework? — comes from work on the Global
Goal on Adaptation indicators. Neither supplies a number here; the OECD supplies
all of them.

## Layout

```
R/01_fetch.R          SDMX ingestion, vintage stamping, manifest (How's Life?)
R/02_validate.R       validation rules, coverage, collection gaps
R/03_diff.R           vintage comparison and reconciliation
R/04_fetch_fua.R      CFE city-level dataflows, with manifest
R/05_environment.R    layers, scoring, exposure-vs-risk test, HSL rows, decomposition
R/06_figures.R        three figures, PNG + SVG, viridis
R/07_site.R           GitHub Pages site (docs/), accessible, EN/FR
R/run_pipeline.R      end-to-end refresh
R/run_environment.R   end-to-end analysis + site
tests/testthat/       44 tests
.github/workflows/    quarterly refresh, fail on error, publish, commit vintage
data/raw/             How's Life? snapshots (committed) + fua/manifest.csv
out/, out/env/        issues, coverage, gaps, diff, indicator, decomposition
docs/                 the published site
```

Tests: `Rscript -e 'testthat::test_dir("tests/testthat")'`. Dependencies in
`DESCRIPTION`. Code MIT; data under OECD terms.
