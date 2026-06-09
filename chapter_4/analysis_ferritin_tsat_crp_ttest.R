# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: analysis_ferritin_tsat_crp_ttest.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ================== Packages ==================
library(dplyr)
library(tidyr)
library(readxl)

# If you're reading from Excel:
# ================== Read Excel ==================
dat <- read_excel(file.path("data", "CRP_Analysis_Master_Data.xlsx"))


# Ensure numeric + logCRP
dat2 <- dat %>%
  mutate(
    ferritin = as.numeric(ferritin),
    tsat     = as.numeric(tsat),
    CRP      = as.numeric(CRP),
    logCRP   = log(CRP + 0.1),   # small constant so CRP=0 doesn't break log
    ID_ferritin30 = !is.na(ferritin) & ferritin < 30,
    ID_tsat20     = !is.na(tsat) & tsat < 20
  ) %>%
  # classify into the 4 cells of the 2x2
  mutate(
    ID_cell = case_when(
      !ID_ferritin30 & !ID_tsat20 ~ "Ferr≥30 & TSAT≥20",
      !ID_ferritin30 &  ID_tsat20 ~ "Ferr≥30 & TSAT<20",  # low TSAT / high ferritin
      ID_ferritin30 & !ID_tsat20 ~ "Ferr<30 & TSAT≥20",
      ID_ferritin30 &  ID_tsat20 ~ "Ferr<30 & TSAT<20"
    ),
    low_tsat_high_ferr = (ID_cell == "Ferr≥30 & TSAT<20")
  )

# ----------------------------
# 1) Primary test: t-test of logCRP in low TSAT/high ferritin vs all others
# ----------------------------
tt_all <- t.test(logCRP ~ low_tsat_high_ferr, data = dat2)
tt_all

# Optional: show group summaries on the original CRP scale too
dat2 %>%
  group_by(low_tsat_high_ferr) %>%
  summarise(
    n = sum(!is.na(logCRP)),
    median_CRP = median(CRP, na.rm = TRUE),
    IQR_CRP = IQR(CRP, na.rm = TRUE),
    mean_logCRP = mean(logCRP, na.rm = TRUE),
    sd_logCRP = sd(logCRP, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 2) Do the same test stratified by timepoint (recommended)
# ----------------------------
tt_by_time <- dat2 %>%
  group_by(time) %>%
  group_modify(~{
    out <- t.test(logCRP ~ low_tsat_high_ferr, data = .x)
    tibble(
      n = nrow(.x),
      mean_diff = unname(out$estimate[2] - out$estimate[1]),
      p_value = out$p.value,
      conf_low = out$conf.int[1],
      conf_high = out$conf.int[2]
    )
  }) %>%
  ungroup()

tt_by_time

# ----------------------------
# 3) Alternative: compare logCRP across all 4 cells (ANOVA)
# ----------------------------
anova_fit <- aov(logCRP ~ ID_cell, data = dat2)
summary(anova_fit)

# If assumptions concern you, use non-parametric:
kruskal.test(CRP ~ ID_cell, data = dat2)


dat2 %>%
  group_by(ID_cell) %>%
  summarise(
    n = sum(!is.na(CRP)),
    median_CRP = median(CRP, na.rm = TRUE),
    IQR_CRP = IQR(CRP, na.rm = TRUE),
    .groups = "drop"
  )

dat2 %>%
  group_by(time, ID_cell) %>%
  summarise(
    n = sum(!is.na(CRP)),
    median_CRP = median(CRP, na.rm = TRUE),
    IQR_CRP = IQR(CRP, na.rm = TRUE),
    .groups = "drop"
  )

