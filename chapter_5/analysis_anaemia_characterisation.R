# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India (SHP2)
# Script: analysis_anaemia_characterisation.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
library(tidyverse)
library(ggplot2)
library(readxl)
library(knitr)

excel_file_path <- file.path("data", "master_results_april_2026_hepcidin_birthoutcomes.xlsx")
sheet_name <- "biomarkers_only"


library(tidyverse)

# ============================================================
# ANAEMIA MORPHOLOGY / HYPOCHROMIA / ANISOCYTOSIS
# ============================================================

# 1. Create anaemia status if not already present
df <- df %>%
  mutate(
    anaemia_status = case_when(
      !is.na(venous_hb) & venous_hb < 120 ~ "anaemic",
      !is.na(venous_hb) & venous_hb >= 120 ~ "non-anaemic",
      TRUE ~ NA_character_
    )
  )

# 2. Restrict to women with venous-defined anaemia
df_an_morph <- df %>%
  filter(anaemia_status == "anaemic") %>%
  mutate(
    morphology = case_when(
      !is.na(mcv_fl) & mcv_fl < 80 ~ "Microcytic",
      !is.na(mcv_fl) & mcv_fl > 100 ~ "Macrocytic",
      !is.na(mcv_fl) & mcv_fl >= 80 & mcv_fl <= 100 ~ "Normocytic",
      TRUE ~ NA_character_
    ),
    hypochromia = case_when(
      !is.na(mch_pg) ~ as.integer(mch_pg < 27),
      TRUE ~ NA_integer_
    ),
    anisocytosis = case_when(
      !is.na(rdw_cv_pct) ~ as.integer(rdw_cv_pct > 14.5),
      TRUE ~ NA_integer_
    ),
    mentzer_lt13 = case_when(
      !is.na(mentzer_index) ~ as.integer(mentzer_index < 13),
      TRUE ~ NA_integer_
    )
  )

# 3. Morphology summary
morph_table <- df_an_morph %>%
  filter(!is.na(morphology)) %>%
  count(morphology, name = "n") %>%
  mutate(
    denominator = sum(n),
    pct = round(n / denominator * 100, 1)
  )

morph_table

# 4. Hypochromia / anisocytosis / Mentzer summary
feature_table <- tibble(
  Characteristic = c("Hypochromia (MCH <27 pg)", "Anisocytosis (RDW-CV >14.5%)", "Mentzer index <13"),
  n = c(
    sum(df_an_morph$hypochromia == 1, na.rm = TRUE),
    sum(df_an_morph$anisocytosis == 1, na.rm = TRUE),
    sum(df_an_morph$mentzer_lt13 == 1, na.rm = TRUE)
  ),
  Denominator = c(
    sum(!is.na(df_an_morph$hypochromia)),
    sum(!is.na(df_an_morph$anisocytosis)),
    sum(!is.na(df_an_morph$mentzer_lt13))
  )
) %>%
  mutate(
    Percent = round(n / Denominator * 100, 1)
  )

feature_table

# 5. Combined supplementary table
supp_morph_table <- bind_rows(
  morph_table %>%
    transmute(
      Characteristic = morphology,
      n = n,
      Denominator = denominator,
      Percent = pct
    ),
  feature_table
)

supp_morph_table

# 6. Optional printout
cat("\nRed cell morphology among women with venous-defined anaemia\n")
print(morph_table)

cat("\nAdditional haematological features among women with venous-defined anaemia\n")
print(feature_table)

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# ============================================================
# Create anaemia status and deficiency indicators
# ============================================================

df_tab <- df %>%
  mutate(
    anaemia_status = case_when(
      !is.na(venous_hb) & venous_hb < 120 ~ "Anaemic",
      !is.na(venous_hb) & venous_hb >= 120 ~ "Non-anaemic",
      TRUE ~ NA_character_
    ),
    trial_arm_clean = case_when(
      tx_arm == "control" ~ "Control",
      tx_arm == "intervention" ~ "Intervention",
      TRUE ~ NA_character_
    ),
    ferritin_lt15 = case_when(
      !is.na(aiims_ferr) ~ as.integer(aiims_ferr < 15),
      TRUE ~ NA_integer_
    ),
    ferritin_lt30 = case_when(
      !is.na(aiims_ferr) ~ as.integer(aiims_ferr < 30),
      TRUE ~ NA_integer_
    ),
    stfr_gte155 = case_when(
      !is.na(stfr_mgL) ~ as.integer(stfr_mgL >= 1.55),
      TRUE ~ NA_integer_
    ),
    ferritin_index_gte103 = case_when(
      !is.na(ferritin_index) ~ as.integer(ferritin_index >= 1.03),
      TRUE ~ NA_integer_
    ),
    b12_def = case_when(
      !is.na(aiims_b12) ~ as.integer(aiims_b12 < 200),
      TRUE ~ NA_integer_
    ),
    b12_borderline = case_when(
      !is.na(aiims_b12) ~ as.integer(aiims_b12 >= 200 & aiims_b12 < 300),
      TRUE ~ NA_integer_
    ),
    folate_def = case_when(
      !is.na(aiims_fol) ~ as.integer(aiims_fol < 2),
      TRUE ~ NA_integer_
    ),
    folate_borderline = case_when(
      !is.na(aiims_fol) ~ as.integer(aiims_fol >= 2 & aiims_fol < 4),
      TRUE ~ NA_integer_
    ),
    crp_elevated = case_when(
      !is.na(aiims_crp) ~ as.integer(aiims_crp > 5),
      TRUE ~ NA_integer_
    )
  )

# ============================================================
# Helper function
# ============================================================

fmt_count <- function(data, var) {
  denom <- sum(!is.na(data[[var]]))
  numer <- sum(data[[var]] == 1, na.rm = TRUE)
  pct <- ifelse(denom > 0, round(numer / denom * 100, 1), NA_real_)
  sprintf("%d/%d (%.1f%%)", numer, denom, pct)
}

# ============================================================
# Variables to report
# ============================================================

vars_to_report <- tribble(
  ~Section,                ~Variable,                  ~Label,
  "Iron Deficiency",       "ferritin_lt15",           "Ferritin <15 µg/L",
  "Iron Deficiency",       "ferritin_lt30",           "Ferritin <30 µg/L",
  "Iron Deficiency",       "stfr_gte155",             "sTfR ≥1.55 mg/L",
  "Iron Deficiency",       "ferritin_index_gte103",   "Ferritin index ≥1.03",
  "Vitamin B12 Status",    "b12_def",                 "Deficient (<200 pg/mL)",
  "Vitamin B12 Status",    "b12_borderline",          "Borderline (200–300 pg/mL)",
  "Folate Status",         "folate_def",              "Deficient (<2 ng/mL)",
  "Folate Status",         "folate_borderline",       "Borderline (2–4 ng/mL)",
  "Inflammation",          "crp_elevated",            "Elevated CRP (>5 mg/L)"
)

# ============================================================
# Build table
# ============================================================

table_5_5_extended <- vars_to_report %>%
  mutate(
    Overall = map_chr(Variable, ~ fmt_count(df_tab, .x)),
    Control = map_chr(Variable, ~ fmt_count(filter(df_tab, trial_arm_clean == "Control"), .x)),
    Intervention = map_chr(Variable, ~ fmt_count(filter(df_tab, trial_arm_clean == "Intervention"), .x)),
    Anaemic = map_chr(Variable, ~ fmt_count(filter(df_tab, anaemia_status == "Anaemic"), .x)),
    `Non-anaemic` = map_chr(Variable, ~ fmt_count(filter(df_tab, anaemia_status == "Non-anaemic"), .x))
  ) %>%
  select(Section, Variable, Label, Overall, Control, Intervention, Anaemic, `Non-anaemic`)

table_5_5_extended

table_5_5_extended %>%
  select(-Variable) %>%
  print(n = Inf)

names(df)