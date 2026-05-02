# Daily nature contact and mental health across 55 Chinese cities
## Replication code

---

## Overview

This repository contains the replication code for the paper:

> **Daily nature contact and mental health across 55 Chinese cities**

The study used a three-week ecological momentary assessment (EMA) design in which 988 urban residents across 59 Chinese cities were recruited (23,246 assessments). The 811 participants in 55 cities who met the ≥ 25% response rate criterion reported momentary nature contact (trees, wildlife, water) and mental well-being approximately three times per day via smartphone. Linear mixed-effects models and Mundlak within/between decompositions were used to estimate associations between nature contact and momentary well-being.

---

## Files

| File | Description |
|------|-------------|
| `main_analysis.R` | Main analysis script. Applies the ≥ 11 valid assessment inclusion criterion and reproduces Tables 1–2 and Figure 1. |
| `heterogeneity_figures.R` | Heterogeneity analysis script. Produces Figures 2–4 (nine forest plots of subgroup effects). |
| `prepare_opendata.R` | Data preparation script used to generate the dataset from the raw (non-public) data file. Provided for transparency; cannot be re-run without the raw data. |
| `sample_map.R` | Generates the sample map (bubble map of participants across the 55 study cities, Albers projection). Requires the raw data file with city names and the China administrative-boundary shapefiles; cannot be re-run without them. |

---

## Dataset description

The dataset is not included in this repository. It is available from the corresponding author upon reasonable request (see **Data availability** below). The column reference below is provided to assist with interpreting the analysis code and any data received upon request.

### Inclusion criterion

Of the **988 participants** recruited across 59 cities (23,246 assessments), **811** in 55 cities met the ≥ 25% response rate criterion (≥ 11 valid assessments) and were included in the analysis (22,515 assessments).

### Column reference

#### Identifiers

| Column | Description |
|--------|-------------|
| `participant_id` | Anonymous participant identifier (integer) |
| `n_assessments` | Total number of valid assessments completed by this participant |

#### Nature exposure (main independent variables)

| Column | Description |
|--------|-------------|
| `tree` | Perceived surrounding tree density: `"None"`, `"Few"`, `"Moderate"`, `"Many"`, `"Dense"` |
| `wildlife` | Saw or heard wildlife: `"Yes"`, `"No"`, `"Not sure"` |
| `water` | Saw or heard water features: `"Yes"`, `"No"`, `"Not sure"` |

#### Mental well-being outcome

| Column | Description |
|--------|-------------|
| `mh` | Composite momentary well-being score (sum of 6 items; range −60 to +60; primary outcome) |

#### Individual-level covariates

| Column | Description |
|--------|-------------|
| `gender` | `"FEMALE"` / `"MALE"` |
| `age` | Age in years |
| `education_3` | Education: `"Less than high school"`, `"High school"`, `"Higher education"` |
| `work_status` | `"Unemployed"`, `"Student"`, `"Part-time"`, `"Freelance"`, `"Full-time"` |
| `work_hours_per_day` | Average daily working hours |
| `income_individual` | Monthly individual income (CNY): `"<3k"`, `"3–6k"`, `"6–10k"`, `"10–50k"`, `">50k"` |
| `income_household` | Monthly household income (CNY): `"<10k"`, `"10–30k"`, `"30–60k"`, `"60–100k"`, `">100k"` |
| `chronic_disease` | Any diagnosed chronic disease: `1` = yes, `0` = no |
| `bmi` | Body mass index (kg/m²) |
| `ever_rural` | `"Lived rural"` / `"Never lived rural"` |

#### Baseline mental health

| Column | Description |
|--------|-------------|
| `phq_score` | PHQ-9 depressive symptom score (0–27; ≥10 = moderate–severe) |
| `pss_score` | PSS-10 perceived stress score (0–40; ≤13 = low, 14–26 = moderate, >26 = high) |
| `who_score` | WHO-5 Well-Being Index score (0–25; <13 = low well-being) |

#### City-level variables

| Column | Description |
|--------|-------------|
| `city` | City name. Cities with ≤ 5 study participants are masked as `"Other small city"` (19 of 55 cities are named; the remaining 36 are pooled). |
| `city_gdp_log` | Log-transformed city GDP in 2024 (100 million CNY) |
| `city_pop_density_log` | Log-transformed urban population in 2024 (10,000 persons) |
| `city_pop_density_2022` | Population density in 2022 (persons/km²) |

---

## Reproducing the analyses

### Requirements

```r
install.packages(c(
  "dplyr", "readr", "lme4", "lmerTest",
  "marginaleffects", "ggplot2", "patchwork",
  "broom.mixed", "tibble", "writexl",
  "sf", "stringr"   # additionally required by sample_map.R
))
```

R ≥ 4.1.0 is recommended.

### Running

First obtain the dataset from the corresponding author (see **Data availability**). Place it together with the scripts in the same directory, set that directory as your working directory, then:

```r
# Reproduce Tables 1–2 and Figure 1:
source("main_analysis.R")

# Reproduce Figures 2–4 (all nine heterogeneity forest plots):
source("heterogeneity_figures.R")

# Reproduce the sample map (requires raw data with city names + shapefiles):
source("sample_map.R")
```

`main_analysis.R` runtime is approximately 20–40 minutes on a standard laptop. `heterogeneity_figures.R` is similarly intensive due to the random-slope subgroup models.

### Convergence warnings

A small number of random-slope models may emit `Model failed to converge` warnings from `lme4`. These arise because some random-slope variance components are estimated near their boundary. Re-fitting with alternative optimisers (`bobyqa`, `Nelder_Mead`) produced estimates within rounding error of those in the manuscript. The warnings can be treated as informational and do not invalidate the results.

### Output files

| File | Contents |
|------|----------|
| `results_table1.xlsx` | Table 1 — baseline random-intercept model coefficients |
| `results_table2.xlsx` | Table 2 — Mundlak within/between decomposition |
| `figure1.pdf` / `figure1.png` | Figure 1 — violin + marginal prediction plots |
| `figure2_tree_heterogeneity.png` | Figure 2a — forest plot, tree × individual/socioeconomic moderators |
| `figure3_wildlife_heterogeneity.png` | Figure 2b — forest plot, wildlife × individual/socioeconomic moderators |
| `figure4_water_heterogeneity.png` | Figure 2c — forest plot, water × individual/socioeconomic moderators |
| `figure5_tree_city_heterogeneity.png` | Figure 3a — forest plot, tree × city-level moderators |
| `figure6_wildlife_city_heterogeneity.png` | Figure 3b — forest plot, wildlife × city-level moderators |
| `figure7_water_city_heterogeneity.png` | Figure 3c — forest plot, water × city-level moderators |
| `figure8_tree_mentalhealth_heterogeneity.png` | Figure 4a — forest plot, tree × baseline mental health |
| `figure9_wildlife_mentalhealth_heterogeneity.png` | Figure 4b — forest plot, wildlife × baseline mental health |
| `figure10_water_mentalhealth_heterogeneity.png` | Figure 4c — forest plot, water × baseline mental health |
| `figures/sample_map_Albers.png` | Sample map — participant bubble map across the 55 study cities (from `sample_map.R`) |

---

## Inclusion criterion and sample size

Of 988 recruited participants across 59 cities (23,246 assessments), the analysis applies a **≥ 25% response rate** threshold (≥ 11 valid EMA assessments), consistent with the 25th percentile of the completion distribution reported in Table S3. After this filter:

- **811 participants** (unadjusted models)
- **756 participants** (adjusted models; requires complete covariate data)
- **22,515 assessments** (unadjusted)
- **20,958 assessments** (adjusted)

---

## Ethics

Data collection was approved by the Ethics Review Committee of the Research Center for Eco-Environmental Sciences, Chinese Academy of Sciences (approval no. AEWC-RCEES-2024043). Participants provided informed consent.

## Data availability

The data supporting the findings of this study are available from the corresponding author upon reasonable request.

