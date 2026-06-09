# Chapter 3: Assessing Response to Oral Iron in Pregnancy
# A Systematic Review of Randomised Controlled Trials

## Overview

This chapter presents a systematic review of 31 randomised controlled trials of oral iron therapy in pregnancy, examining how haematological response to treatment is monitored across studies. The review covers biomarker selection, assessment timing, and definitions of response, stratified by country income level.

The PRISMA-compliant search and screening process identified 31 eligible RCTs. Data were extracted on biomarkers used, timepoints assessed, and definitions of response.

## Scripts and Outputs

| Script | Figure/Table | Description |
|--------|-------------|-------------|
| `figure_3_2_countries_map.R` | Figure 3.2 | World map of countries included in the systematic review |
| `figure_3_3_biomarker_use_by_year.R` | Figure 3.3 | Biomarker use by publication year |
| `figure_3_4_biomarker_use_by_timepoint.R` | Figure 3.4 | Biomarker use by assessment timepoint |
| `figure_3_5_biomarkers_by_income_level.R` | Figure 3.5 | Biomarker use by country income level (heatmap) |
| `figure_3_6_risk_of_bias.R` | Figure 3.6 | Risk of bias summary (RoB 2 tool) |
| `figure_response_definitions_piechart.R` | Supplementary | Definitions of response to oral iron across guidelines |
| `table_3_4_studies_by_timepoint_income.R` | Table 3.4 | Studies by timepoint and country income level |

## Data

The systematic review data (study characteristics, biomarker use, timepoints) are read from an Excel file (`SR_Results_Graphs_withoutAbbas.xlsx`). This file is not publicly available but reflects the data extraction performed by the review team. Risk of bias data are read from `rob2_table.csv`.

To reproduce figures, update file paths at the top of each script to point to your local copy of the data files.

## Key Packages

```r
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)
```

## Data folder

Scripts expect a `data/` subfolder within `chapter_3/` containing the relevant data file(s).
Create it and place your data file there before running any script:

```
mkdir chapter_3/data
```

Expected files: `SR_Results_Graphs.xlsx`, `rob2_table.csv`
