# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India (SHP2)
# Script: table_nutritional_deficiency_by_arm.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ============================================================
# Generate Table 2B: Biomarkers Stratified by Anaemia Status and Trial Arm
# + Nutritional Deficiency Table by Trial Arm and Anaemia Status
# SMARThealth Pregnancy Sub-Study
# ============================================================

library(tidyverse)
library(readxl)

# ── 1. LOAD DATA ─────────────────────────────────────────────
file_path <- file.path("data", "master_results_april_2026_hepcidin_birthoutcomes.xlsx")
df <- read_excel(file_path, sheet = "biomarkers_only")

# ── 2. CLEAN DATA ────────────────────────────────────────────
df <- df %>%
  mutate(
    # Handle <100 B12
    aiims_b12 = if_else(aiims_b12 == "<100", "99", as.character(aiims_b12)),
    
    # Convert relevant variables to numeric
    across(
      c(
        poc_hb_baseline, poc_hb_postpartum, venous_hb,
        aiims_b12, aiims_crp, aiims_fol, aiims_ferr,
        stfr_nmol, stfr_mgL, ferritin_index, aiims_hepcidin,
        hct_pct, rbc_10e6_ul, mcv_fl, mch_pg, mchc_g_dl,
        rdw_sd_fl, rdw_cv_pct, mentzer_index,
        wbc_total_10e3_ul, platelets_10e3_ul
      ),
      ~ as.numeric(.)
    ),
    
    # Remove implausible venous Hb outliers if present
    venous_hb = if_else(!is.na(venous_hb) & venous_hb > 250, NA_real_, venous_hb),
    
    # Trial arm
    tx_arm = factor(tx_arm, levels = c("Control", "Intervention")),
    
    # Anaemia classification
    anaemia_venous = !is.na(venous_hb) & venous_hb < 120,
    
    # Stratification variable
    strata = case_when(
      tx_arm == "Control" & !anaemia_venous ~ "Control_NotAnaemic",
      tx_arm == "Control" & anaemia_venous  ~ "Control_Anaemic",
      tx_arm == "Intervention" & !anaemia_venous ~ "Intervention_NotAnaemic",
      tx_arm == "Intervention" & anaemia_venous  ~ "Intervention_Anaemic",
      TRUE ~ NA_character_
    ),
    
    # Nutritional deficiency indicators
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
    ),
    multifactorial_def = case_when(
      rowSums(
        cbind(ferritin_lt30 == 1, b12_def == 1, folate_def == 1),
        na.rm = TRUE
      ) >= 2 ~ 1L,
      rowSums(
        cbind(!is.na(ferritin_lt30), !is.na(b12_def), !is.na(folate_def))
      ) > 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ── 3. ANALYSIS DATASET ──────────────────────────────────────
df_analysis <- df %>% 
  filter(!is.na(venous_hb))

# ── 4. FUNCTION: MEDIAN [IQR] ────────────────────────────────
med_iqr <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
    median(x),
    quantile(x, 0.25),
    quantile(x, 0.75)
  )
}

# ── 5. VARIABLES TO SUMMARISE ────────────────────────────────
biomarkers <- c(
  "venous_hb"          = "Venous haemoglobin (g/L)",
  "aiims_ferr"         = "Ferritin (µg/L)",
  "stfr_mgL"           = "Soluble transferrin receptor (mg/L)",
  "ferritin_index"     = "Ferritin index (sTfR/log ferritin)",
  "aiims_hepcidin"     = "Hepcidin (ng/mL)",
  "aiims_b12"          = "Vitamin B12 (pg/mL)",
  "aiims_fol"          = "Folate (ng/mL)",
  "aiims_crp"          = "CRP (mg/L)",
  "mcv_fl"             = "MCV (fL)",
  "mch_pg"             = "MCH (pg)",
  "mchc_g_dl"          = "MCHC (g/dL)",
  "rdw_cv_pct"         = "RDW-CV (%)",
  "rbc_10e6_ul"        = "RBC count (×10⁶/µL)",
  "wbc_total_10e3_ul"  = "WBC total (×10³/µL)",
  "platelets_10e3_ul"  = "Platelets (×10³/µL)",
  "mentzer_index"      = "Mentzer index (MCV/RBC)"
)

# ── 6. BUILD RESULTS TABLE 2B ────────────────────────────────
results <- tibble(
  Variable = unname(biomarkers)
)

# Overall column
results$Overall <- map_chr(
  names(biomarkers),
  ~ med_iqr(df_analysis[[.x]])
)

# By trial arm / anaemia strata
for (grp in c(
  "Control_NotAnaemic",
  "Control_Anaemic",
  "Intervention_NotAnaemic",
  "Intervention_Anaemic"
)) {
  results[[grp]] <- map_chr(
    names(biomarkers),
    ~ med_iqr(
      df_analysis %>%
        filter(strata == grp) %>%
        pull(all_of(.x))
    )
  )
}

# ── 7. SAMPLE SIZES ──────────────────────────────────────────
cat("\n=== SAMPLE SIZES ===\n")
cat("Overall:", nrow(df_analysis), "\n")
cat("Control Not Anaemic:", sum(df_analysis$strata == "Control_NotAnaemic", na.rm = TRUE), "\n")
cat("Control Anaemic:", sum(df_analysis$strata == "Control_Anaemic", na.rm = TRUE), "\n")
cat("Intervention Not Anaemic:", sum(df_analysis$strata == "Intervention_NotAnaemic", na.rm = TRUE), "\n")
cat("Intervention Anaemic:", sum(df_analysis$strata == "Intervention_Anaemic", na.rm = TRUE), "\n")

# ── 8. PRINT RESULTS TABLE 2B ────────────────────────────────
cat("\n=== TABLE 2B ===\n\n")
print(results, n = Inf, width = Inf)

# ── 9. SAVE OUTPUT TABLE 2B ──────────────────────────────────
write.csv(results, "Table2B_Stratified_Biomarkers.csv", row.names = FALSE)

cat("\n✓ Table saved to: Table2B_Stratified_Biomarkers.csv\n")
cat("✓ Import this CSV to create formatted Word table\n")



# ============================================================
# TABLE 5.5 EXTENDED: NUTRITIONAL DEFICIENCY BY ARM AND ANAEMIA STATUS
# ============================================================

# Helper function: n/N (%)
fmt_count <- function(data, var) {
  denom <- sum(!is.na(data[[var]]))
  numer <- sum(data[[var]] == 1, na.rm = TRUE)
  pct <- ifelse(denom > 0, round(numer / denom * 100, 1), NA_real_)
  sprintf("%d/%d (%.1f%%)", numer, denom, pct)
}

nutritional_vars <- tribble(
  ~Section,                ~Variable,                ~Label,
  "Iron Deficiency",       "ferritin_lt15",         "Ferritin <15 µg/L",
  "Iron Deficiency",       "ferritin_lt30",         "Ferritin <30 µg/L",
  "Iron Deficiency",       "stfr_gte155",           "sTfR ≥1.55 mg/L",
  "Iron Deficiency",       "ferritin_index_gte103", "Ferritin index ≥1.03",
  "Vitamin B12 Status",    "b12_def",               "Deficient (<200 pg/mL)",
  "Vitamin B12 Status",    "b12_borderline",        "Borderline (200–300 pg/mL)",
  "Folate Status",         "folate_def",            "Deficient (<2 ng/mL)",
  "Folate Status",         "folate_borderline",     "Borderline (2–4 ng/mL)",
  "Inflammation",          "crp_elevated",          "Elevated CRP (>5 mg/L)",
  "Nutritional Deficiency Pattern", "multifactorial_def", "≥2 concurrent deficiencies*",
)

table_5_5_extended <- nutritional_vars %>%
  mutate(
    # Full cohort columns: denominator depends ONLY on each biomarker's missingness
    Overall = map_chr(Variable, ~ fmt_count(df, .x)),
    Control = map_chr(Variable, ~ fmt_count(filter(df, tx_arm == "Control"), .x)),
    Intervention = map_chr(Variable, ~ fmt_count(filter(df, tx_arm == "Intervention"), .x)),
    
    # Anaemia-status columns: must restrict to those with venous Hb available
    Anaemic = map_chr(
      Variable,
      ~ fmt_count(filter(df, !is.na(venous_hb) & anaemia_venous), .x)
    ),
    Non_Anaemic = map_chr(
      Variable,
      ~ fmt_count(filter(df, !is.na(venous_hb) & !anaemia_venous), .x)
    )
  ) %>%
  select(Section, Label, Overall, Control, Intervention, Anaemic, Non_Anaemic)

cat("\n=== EXTENDED NUTRITIONAL DEFICIENCY TABLE ===\n\n")
print(table_5_5_extended, n = Inf, width = Inf)

write.csv(table_5_5_extended, "Table5_5_Extended_NutritionalDeficiency.csv", row.names = FALSE)

cat("\n✓ Extended table saved to: Table5_5_Extended_NutritionalDeficiency.csv\n")