# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# A Descriptive Cross-Sectional Study of 49 NHS Sites

## Overview

This chapter evaluates how 49 NHS site-level antenatal anaemia guidelines align with the 2020 British Society for Haematology (BSH) national guidelines. Guidelines were collected and systematically audited across key domains: screening, diagnosis, oral iron treatment, IV iron indications, monitoring, and treatment duration.

## Scripts and Outputs

| Script | Figure/Table | Description |
|--------|-------------|-------------|
| `figure_2_3_uk_guidelines_heatmap.R` | Figure 2.3 | Heatmap of NHS site guidelines collected by UK region |
| `figure_2_4_screening_heatmap.R` | Figure 2.4 | Geographic variation in additional routine anaemia screening |
| `figure_2_5_oral_iron_dosing.R` | Figure 2.5 | Oral iron dosing recommendations across NHS site guidelines |
| `figure_2_6_iv_iron_indications.R` | Figure 2.6 | Haemoglobin thresholds for IV iron across NHS site guidelines |
| `figure_2_7a_monitoring_post_oral_iron.R` | Figure 2.7 (oral) | Monitoring recommendations after oral iron by test and timing |
| `figure_2_7b_monitoring_post_iv_iron.R` | Figure 2.7 (IV) | Monitoring recommendations after IV iron by test and timing |
| `figure_treatment_duration.R` | Supplementary | Iron treatment duration recommendations |
| `figure_definitions_oral_iron_response.R` | Supplementary | Definitions of adequate response to oral iron |

## Data

Data were extracted from 49 NHS site antenatal anaemia guidelines. The structured data extraction tool (domains and items listed in Table 2.2 of the thesis) was used by the primary author. Guidelines were obtained through NHS Trust Freedom of Information requests and publicly available sources.

Data are hardcoded within scripts as summary counts consistent with published Table 2.3 and Figures 2.3–2.7.

## Key Packages

```r
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(dplyr)
library(tidyr)
```

## Data folder

Scripts expect a `data/` subfolder within `chapter_2/` containing the relevant data file(s).
Create it and place your data file there before running any script:

```
mkdir chapter_2/data
```
