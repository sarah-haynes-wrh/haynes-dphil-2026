# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers During Pregnancy
# A Laboratory Sub-Analysis of the PANDA Randomised Controlled Trial

## Overview

This chapter presents a laboratory sub-analysis of the PANDA (Prophylactic ANtenatal iron Daily vs Alternate-day) randomised controlled trial, examining iron biomarker trajectories under three prophylactic oral iron supplementation regimens (daily, alternate-day, and 3× weekly) during pregnancy.

Key biomarkers analysed: ferritin, transferrin saturation (TSAT), soluble transferrin receptor (sTfR), hepcidin, haemoglobin (Hb), MCV, MCH, serum iron, and transferrin.

**Primary statistical analysis:** CRP-adjusted linear mixed-effects models (LMMs) with participants as random effects, comparing within-group and between-group changes from baseline to follow-up.

**Published paper:** Haynes S, Armitage AE, Roy N, Morovat A, Drakesmith H, Churchill D, Stanworth SJ. *Blood Advances*. 2026. doi:10.1182/bloodadvances.2026019740

## Scripts and Outputs

| Script | Figure/Table | Description |
|--------|-------------|-------------|
| `analysis_lmm_crp_adjusted_all_biomarkers.R` | Tables 4.3–4.4 | **Primary analysis:** CRP-adjusted LMMs for all biomarkers; within- and between-group changes |
| `table_4_3_back_transformed_means.R` | Table 4.3 | Back-transformed LMM marginal means with 95% CIs (Word export) |
| `figure_4_6_violin_plots_all_biomarkers.R` | Figure 4.6 | Biomarker distributions by supplementation group and timepoint |
| `figure_4_7_forest_plot_within_group_changes.R` | Figure 4.7 | Forest plot of adjusted within-group biomarker changes |
| `figure_crp_categories_stacked_bar.R` | Supplementary | CRP category distribution by group and timepoint |
| `figure_5_6_crp_biomarker_scatter.R` | Figure 5.6 | CRP–biomarker associations (Spearman correlations + scatter plots) |
| `analysis_tsat_ferritin_discordance.R` | Supplementary | TSAT and ferritin discordance analysis |
| `analysis_ferritin_tsat_crp_ttest.R` | Supplementary | Ferritin/TSAT discordance by CRP category |

## Statistical Methods

- **Model structure:** `log(biomarker) ~ group × time + log10(CRP) + (1 | participant_id)`
- **Software:** `lme4::lmer()` with Kenward-Roger degrees of freedom via `lmerTest`
- **Contrasts:** Estimated marginal means (`emmeans`); within-group changes by group; between-group differences in change (difference-in-differences)
- **CRP adjustment:** Log10-transformed CRP included as a covariate to account for acute-phase response effects on ferritin and other biomarkers
- **Back-transformation:** Log-scale estimates back-transformed to original units for Table 4.3

## Data

Data are from the PANDA trial (Oxford, UK). The primary data file is `CRP Analysis Master Data.xlsx`. Access to trial data is subject to data governance agreements. Contact the corresponding author of the published paper for data access requests.

Update the file path at the top of each script before running:
```r
df <- read_excel("path/to/CRP Analysis Master Data.xlsx")
```

## Key Packages

```r
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(readxl)
library(ggplot2)
library(patchwork)
library(gghalves)
library(flextable)
library(officer)
library(pbkrtest)
```

### Violin plot variants (Figure 4.6)

Three violin scripts are provided, each covering a different biomarker set:

| Script | Biomarkers | Use |
|--------|-----------|-----|
| `figure_4_6_violin_plots_all_biomarkers.R` | Hb, MCV, MCH, CRP, Ferritin, TSAT, sTfR, Hepcidin | Main 8-biomarker panel |
| `figure_4_6_violin_plots_with_ferritin_index_portrait.R` | Above + Ferritin Index, TIBC, Transferrin (portrait layout) | Extended 11-biomarker panel |
| `figure_4_6_violin_plots_with_ferritin_index_landscape.R` | Same as portrait, landscape orientation | Alternative layout |
| `figure_4_6_split_violin_iron_deficiency_stfr.R` | Ferritin, TSAT, sTfR, Hb (4-panel split violin, log scale) | Focused iron deficiency panel |

## Data folder

Scripts expect a `data/` subfolder within `chapter_4/` containing the relevant data file(s).
Create it and place your data file there before running any script:

```
mkdir chapter_4/data
```

Expected file: `CRP_Analysis_Master_Data.xlsx`
