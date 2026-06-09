# Anaemia and Iron Status Assessment in Maternal Health: From Local Guidelines to Global Context

**Author:** Dr Sarah Madeleine Haynes  
**Degree:** Doctor of Philosophy in Women's and Reproductive Health  
**Institution:** Nuffield Department of Women's and Reproductive Health, University of Oxford  
**College:** Jesus College  
**Term:** Trinity Term, 2026  
**Supervisors:** Professor Jane Hirst, Professor Simon Stanworth, Dr Margaret Smith

---

## Overview

This repository contains the R analysis code supporting the DPhil thesis *"Anaemia and Iron Status Assessment in Maternal Health: From Local Guidelines to Global Context"*. The thesis examines how iron status biomarkers are used to define, diagnose, and monitor iron deficiency and anaemia across different clinical and research settings in maternal health.

The thesis comprises four empirical chapters (Chapters 2–5), each addressed by a separate set of R scripts:

| Chapter | Study | Methods |
|---------|-------|---------|
| Chapter 2 | Variation in antenatal anaemia guidelines across 49 NHS sites | Descriptive cross-sectional audit; data visualisation |
| Chapter 3 | Systematic review of oral iron RCTs in pregnancy | Systematic review; bibliographic analysis; data visualisation |
| Chapter 4 | Iron biomarker trajectories in the PANDA RCT | Linear mixed-effects models (LMMs); CRP-adjusted LMMs; violin/forest plots |
| Chapter 5 | Postpartum anaemia in rural India (SHP2 nested sub-study) | Mixed-effects logistic/linear regression; biomarker agreement; descriptive tables |

---

## Repository Structure

```
thesis_haynes_2026/
│
├── README.md                         # This file
├── .gitignore                        # Files excluded from version control
├── session_info.R                    # R session info and package versions
│
├── chapter_2/                        # NHS Guideline Audit (Chapter 2)
│   ├── figure_2_3_uk_guidelines_heatmap.R
│   ├── figure_2_4_screening_heatmap.R
│   ├── figure_2_5_oral_iron_dosing.R
│   ├── figure_2_6_iv_iron_indications.R
│   ├── figure_2_7a_monitoring_post_oral_iron.R
│   ├── figure_2_7b_monitoring_post_iv_iron.R
│   ├── figure_treatment_duration.R
│   └── figure_definitions_oral_iron_response.R
│
├── chapter_3/                        # Systematic Review (Chapter 3)
│   ├── figure_3_2_countries_map.R
│   ├── figure_3_3_biomarker_use_by_year.R
│   ├── figure_3_4_biomarker_use_by_timepoint.R
│   ├── figure_3_5_biomarkers_by_income_level.R
│   ├── figure_3_6_risk_of_bias.R
│   ├── figure_response_definitions_piechart.R
│   └── table_3_4_studies_by_timepoint_income.R
│
├── chapter_4/                        # PANDA Trial Analysis (Chapter 4)
│   ├── analysis_lmm_crp_adjusted_all_biomarkers.R   # Primary LMM analysis
│   ├── table_4_3_back_transformed_means.R            # Back-transformed LMM means table
│   ├── figure_4_6_violin_plots_all_biomarkers.R      # Figure 4.6
│   ├── figure_4_7_forest_plot_within_group_changes.R # Figure 4.7
│   ├── figure_crp_categories_stacked_bar.R
│   ├── figure_5_6_crp_biomarker_scatter.R
│   ├── analysis_tsat_ferritin_discordance.R
│   └── analysis_ferritin_tsat_crp_ttest.R
│
└── chapter_5/                        # SHP2 India Sub-study (Chapter 5)
    ├── master_analysis_script.R                            # Primary analysis (with PHC sensitivity)
    ├── figure_5_4_5_iron_deficiency_cascade.R
    ├── analysis_anaemia_characterisation.R
    ├── table_5_2_birth_outcomes.R
    └── table_nutritional_deficiency_by_arm.R
```

---

## Data Availability

Data used in these analyses are not publicly available due to participant confidentiality agreements. Researchers interested in data access should contact the relevant trial data custodians:

- **Chapter 4 (PANDA trial):** Contact the corresponding author of [Haynes et al., *Blood Advances*, 2026](https://doi.org/10.1182/bloodadvances.2026019740).
- **Chapter 5 (SMARThealth Pregnancy 2 trial):** Contact via the SMARThealth Pregnancy trial team; data access subject to institutional data governance processes.

Scripts are provided for transparency and reproducibility. Scripts assume appropriately obtained data files are placed in a local `data/` directory within each chapter folder (see each chapter's README for expected filenames).

---

## Dependencies

All analyses were conducted in R. Key packages used across chapters:

| Package | Version | Use |
|---------|---------|-----|
| `tidyverse` | ≥2.0 | Data wrangling and ggplot2 visualisation |
| `readxl` | ≥1.4 | Reading Excel data files |
| `lme4` | ≥1.1 | Linear and generalised linear mixed-effects models |
| `lmerTest` | ≥3.1 | P-values for LMMs (Satterthwaite/Kenward-Roger) |
| `emmeans` | ≥1.9 | Estimated marginal means and contrasts |
| `ggplot2` | ≥3.5 | Data visualisation |
| `sf` | ≥1.0 | Spatial data (map figures) |
| `rnaturalearth` | ≥1.0 | Country/region map data |
| `flextable` | ≥0.9 | Formatted tables for Word export |
| `officer` | ≥0.6 | Word document export |
| `patchwork` | ≥1.2 | Multi-panel figure composition |
| `gghalves` | ≥0.1 | Split violin plots |

Install all required packages:

```r
install.packages(c(
  "tidyverse", "readxl", "lme4", "lmerTest", "emmeans",
  "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
  "flextable", "officer", "patchwork", "gghalves",
  "broom.mixed", "corrplot", "scales", "psych", "viridis",
  "patchwork", "RColorBrewer", "pbkrtest", "performance"
))
```

---

## Citation

If you use code from this repository, please cite the relevant published work:

> Haynes S, Armitage AE, Roy N, Morovat A, Drakesmith H, Churchill D, Stanworth SJ. Impact of Oral Iron Dosing Regimens on Iron Biomarkers in Pregnancy: Insights from the PANDA Trial. *Blood Advances*. 2026; bloodadvances.2026019740. doi:10.1182/bloodadvances.2026019740.

---

## Statement on Use of Artificial Intelligence

In accordance with University of Oxford guidance, generative AI tools (ChatGPT, Claude) were used during code development for language refinement, proofreading, and technical troubleshooting. All statistical analysis, interpretation, and scientific conclusions were conceived and verified by the author. The author takes full responsibility for all code in this repository.

---

## Contact

Sarah Madeleine Haynes  
Nuffield Department of Women's and Reproductive Health  
University of Oxford
