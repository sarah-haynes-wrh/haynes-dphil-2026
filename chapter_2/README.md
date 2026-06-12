## Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
### A Descriptive Cross-Sectional Study of 49 NHS Sites

### Overview

This chapter evaluates how 49 NHS site-level antenatal anaemia guidelines align with the 2020 British Society for Haematology (BSH) national guidelines. Guidelines were collected and systematically audited across key domains: screening, diagnosis, oral iron treatment, IV iron indications, monitoring, and treatment duration.

---

### Scripts and Outputs

| Script | Figure | Description |
|---|---|---|
| `figure_2_3_heatmap.R` | Figure 2.3a–b | Bubble map of guideline counts by UK region; bar chart of annual births by region |
| `figure_2_4_heatmap_screening.R` | Figure 2.4 | Geographic variation in additional routine anaemia screening |
| `figure_2_5_oral_iron_dosing.R` | Figure 2.5 | Oral iron dosing recommendations across NHS site guidelines |
| `figure_2_6_IV_iron_indications.R` | Figure 2.6 | Haemoglobin thresholds for IV iron across NHS site guidelines |
| `2_7_monitoring.R` | Figure 2.7a–b | Monitoring recommendations after oral and IV iron by biomarker and timing |

---

### Data

Data were extracted from 49 NHS site antenatal anaemia guidelines using a structured data extraction tool (domains and items listed in Table 2.2 of the thesis). Guidelines were obtained through NHS Trust Freedom of Information requests and publicly available sources.

All data are hardcoded within scripts as summary counts, consistent with published Table 2.3 and Figures 2.3–2.7. No external data files are required.

---

### Key Packages

```r
library(ggplot2)
library(maps)
library(scales)
library(dplyr)
library(tidyr)
```
