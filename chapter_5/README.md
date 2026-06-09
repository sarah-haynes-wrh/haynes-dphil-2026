# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India
# A Nested Sub-study of the SMARThealth Pregnancy 2 (SHP2) Trial

## Overview

This chapter examines postpartum anaemia and iron deficiency within a nested sub-study of the SMARThealth Pregnancy 2 (SHP2) randomised controlled trial in rural India. Analyses focus on: anaemia prevalence and aetiology at 12 months postpartum; comparison of point-of-care capillary vs laboratory venous haemoglobin; iron biomarker classification agreement; CRP–biomarker associations; and mixed-effects regression models for anaemia and iron deficiency anaemia outcomes.

**Sample:** n = 599 participants from the SHP2 sub-study  
**Primary timepoint:** 12 months postpartum  
**Clustering:** Village-level random effects; PHC-level sensitivity analysis included

## Scripts and Outputs

| Script | Figure/Table | Description |
|--------|-------------|-------------|
| `master_analysis_script.R` | All main results | **Primary analysis script** — runs all regression models, descriptive statistics, biomarker agreement analyses, and sensitivity analyses (PHC clustering). See section headers within the script. |
| `figure_5_4_5_iron_deficiency_cascade.R` | Figures 5.4–5.5 | Iron deficiency prevalence by biomarker definition; iron status in anaemic women (ferritin vs sTfR) |
| `analysis_anaemia_characterisation.R` | Supplementary | Anaemia morphology characterisation (hypochromia, anisocytosis, MCV categories) |
| `table_5_2_birth_outcomes.R` | Table 5.2 | Obstetric and delivery outcomes by trial arm |
| `table_nutritional_deficiency_by_arm.R` | Table 5.4 | Nutritional deficiency classifications by trial arm and anaemia status |

## Master Script Structure

The `master_analysis_script.R` is organised into the following sections:

1. **Setup & Libraries** — package loading, seed setting
2. **Data Loading & Preparation** — read Excel, variable coding, numeric coercion
3. **Descriptive Statistics** — baseline characteristics (Table 5.1)
4. **POC vs Venous Haemoglobin** — Bland-Altman analysis (Figure 5.2), classification agreement (Figure 5.3)
5. **Anaemia & Nutritional Deficiency Classification** — prevalence tables (Table 5.4)
6. **Iron Deficiency Biomarker Analysis** — cascade plots (Figure 5.4), sTfR-ferritin agreement (Figure 5.5)
7. **CRP & Inflammation Analysis** — BRINDA ferritin correction, scatter plots (Figure 5.6)
8. **Regression Models** — mixed-effects models for venous anaemia, IDA, and continuous Hb (Table 5.5)
9. **Sensitivity Analysis** — PHC-level clustering models vs village-level
10. **Birth Outcomes** — delivery outcomes by trial arm (Table 5.2)

## Statistical Methods

- **Primary models:** Mixed-effects logistic regression (anaemia, IDA) and linear regression (continuous Hb) with village-level random intercepts (`lme4::glmer` / `lmer`)
- **Covariates:** Trial arm, baseline POC Hb (z-score), maternal age (z-score), parity, BMI (z-score), gestational age (z-score), log CRP (models 1–2 only)
- **Sensitivity analysis:** PHC-level clustering substituted for village-level clustering
- **Biomarker agreement:** Cohen's kappa, percent agreement, and Bland-Altman analysis
- **CRP adjustment:** BRINDA correction applied for ferritin; CRP categories used for stratified analyses

## Data

Data are from the SMARThealth Pregnancy 2 trial (India). Primary data file: `master_results_april_2026_hepcidin_birthoutcomes.xlsx` (sheet: `biomarkers_only`). A secondary file `clean_birthOutcomesData.xlsx` is used for birth outcomes.

Data access is subject to institutional data governance. Contact the SHP2 trial team for data access requests.

Update file paths at the top of each script before running:
```r
df <- read_excel("path/to/master_results_april_2026_hepcidin_birthoutcomes.xlsx", sheet = "biomarkers_only")
```

## Key Packages

```r
library(tidyverse)
library(readxl)
library(lme4)
library(broom.mixed)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(scales)
library(psych)
```

## Data folder

Scripts expect a `data/` subfolder within `chapter_5/` containing the relevant data file(s).
Create it and place your data file there before running any script:

```
mkdir chapter_5/data
```

Expected files: `master_results_april_2026_hepcidin_birthoutcomes.xlsx` (sheets: `biomarkers_only`, `master_clean`), `clean_birthOutcomesData.xlsx`
