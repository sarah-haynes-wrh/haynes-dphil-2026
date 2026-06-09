# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: analysis_tsat_ferritin_discordance.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ================== Packages ==================
library(dplyr)
library(tidyr)
library(readxl)

# If you're reading from Excel:
# ================== Read Excel ==================
dat <- read_excel(file.path("data", "CRP_Analysis_Master_Data.xlsx"))

# If your data is already in a data frame called dat, ensure numeric columns are numeric:
dat <- dat %>%
  mutate(
    ferritin = as.numeric(ferritin),
    tsat     = as.numeric(tsat)
  )

# ================== Define iron deficiency flags ==================
dat_id <- dat %>%
  mutate(
    ID_ferritin30 = !is.na(ferritin) & ferritin < 30,
    ID_tsat20     = !is.na(tsat) & tsat < 20,
    ID_either     = ID_ferritin30 | ID_tsat20,
    ID_both       = ID_ferritin30 & ID_tsat20,
    ID_disagree   = ID_ferritin30 != ID_tsat20
  )

# ================== Overall agreement table ==================
# 2x2 table counts
tab_counts <- with(dat_id, table(ID_ferritin30, ID_tsat20, useNA = "no"))
tab_counts

# Row/col percentages (optional)
prop.table(tab_counts, margin = 1)  # row %
prop.table(tab_counts, margin = 2)  # col %
prop.table(tab_counts)              # overall %

# Simple agreement metrics (optional)
agree_rate <- mean(dat_id$ID_ferritin30 == dat_id$ID_tsat20, na.rm = TRUE)
agree_rate

# ================== See who disagrees ==================
# People classified as ID by ferritin only or TSAT only
disagreements <- dat_id %>%
  filter(ID_disagree) %>%
  transmute(
    id, group, time,
    ferritin, tsat,
    ID_ferritin30, ID_tsat20
  ) %>%
  arrange(time, group, id)

disagreements

# ================== Summaries by time and group ==================
summary_by_group_time <- dat_id %>%
  group_by(time, group) %>%
  summarise(
    n = n(),
    n_ferritin30 = sum(ID_ferritin30, na.rm = TRUE),
    n_tsat20     = sum(ID_tsat20, na.rm = TRUE),
    n_both       = sum(ID_both, na.rm = TRUE),
    n_either     = sum(ID_either, na.rm = TRUE),
    n_disagree   = sum(ID_disagree, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_group_time

# ================== Optional: per-participant comparison at a given timepoint ==================
# e.g., restrict to FollowUp only (change "FollowUp" to your exact label)
followup_tab <- dat_id %>%
  filter(time == "FollowUp") %>%
  { with(., table(ID_ferritin30, ID_tsat20, useNA = "no")) }

followup_tab
