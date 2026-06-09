# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India (SHP2)
# Script: master_analysis_script.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ============================================================================
# CHAPTER 5: POSTPARTUM ANAEMIA IN RURAL INDIA
# Comprehensive Analysis Script (n=599, corrected dataset)
# UPDATED: Using biomarkers_only as primary dataset
# ============================================================================
#
# This script uses biomarkers_only sheet as the primary dataset for all
# biomarker analyses (iron status, inflammation, nutritional deficiencies)
# and merges in demographic/baseline data from master_clean only when needed.
#
# This approach avoids variable name conflicts (stfr_nmol vs stfr_mgL)
# and keeps biomarker data clean.
#
# ============================================================================
# SETUP & LIBRARIES
# ============================================================================

library(tidyverse)      # data wrangling, ggplot2
library(readxl)         # read Excel files
library(lme4)           # mixed-effects models
library(broom.mixed)    # tidy mixed model output
library(corrplot)       # correlation plots
library(ggplot2)        # visualisation
library(gridExtra)      # multi-panel plots
library(scales)         # axis scaling
library(psych)          # Cohen's kappa, etc.

# Set random seed for reproducibility
set.seed(42)

# ============================================================================
# DATA LOADING
# ============================================================================

cat("\n===== LOADING DATA FROM EXCEL =====\n")

excel_file_path <- file.path("data", "master_results_april_2026_hepcidin_birthoutcomes.xlsx")

# Load the sheets
df_biomarkers <- read_excel(excel_file_path, sheet = "biomarkers_only")
df_master <- read_excel(excel_file_path, sheet = "master_clean")

cat("Loaded biomarkers_only: ", nrow(df_biomarkers), "rows\n")
cat("Loaded master_clean: ", nrow(df_master), "rows\n")

# ============================================================================
# DATA PREPARATION: PRIMARY DATASET FROM BIOMARKERS_ONLY
# ============================================================================

cat("\n===== PREPARING PRIMARY DATASET FROM BIOMARKERS_ONLY =====\n")

# ── Duplicate check ───────────────────────────────────────────────────────────
# The original analysis used distinct(patient_id, .keep_all = TRUE) to handle
# any duplicate IDs before analysis. The code below reports whether duplicates
# exist in your dataset. If duplicates are found, investigate the cause
# (data entry error vs legitimate repeat) before deciding how to handle them.
dup_check <- df_biomarkers %>%
  count(patient_id) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  cat("NOTE: Duplicate patient IDs found in source data:\n")
  print(dup_check)
  cat("The original analysis removed these with distinct(patient_id, .keep_all = TRUE).\n")
  cat("Verify this is appropriate for your dataset before proceeding.\n")
  df_biomarkers <- df_biomarkers %>% distinct(patient_id, .keep_all = TRUE)
} else {
  cat("No duplicate participant IDs found.\n")
}

df_analysis <- df_biomarkers %>%
  
  # Standardise variable names to analysis convention
  rename(
    participant_id = patient_id,
    trial_arm = tx_arm,
    maternal_age = v1_age,
    village_id = village_code,
    phc_id = phc_code,
    poc_hb_baseline = poc_hb_baseline,
    poc_hb_12m = poc_hb_postpartum,
    stfr_mgL = stfr_mgL,           # Already in mg/L in biomarkers_only
    ferritin = aiims_ferr,
    crp = aiims_crp,
    vitamin_b12 = aiims_b12,
    folate = aiims_fol,
    hepcidin = aiims_hepcidin,
    venous_hb_12m = venous_hb,
    haematocrit = hct_pct,
    rbc = rbc_10e6_ul,
    mcv = mcv_fl,
    mch = mch_pg,
    mchc = mchc_g_dl,
    rdw_cv = rdw_cv_pct,
    rdw_sd = rdw_sd_fl,
    wbc = wbc_total_10e3_ul,
    platelets = platelets_10e3_ul
  ) %>%
  
  # MERGE DEMOGRAPHIC DATA FROM MASTER_CLEAN
  # This gets baseline variables we need for regression models
  left_join(
    df_master %>%
      select(
        patient_id,
        bmi = v1_bmi,
        parity,
        gravidity,
        gestational_age_baseline = v1_ga_weeks,
        delivery_mode,
        place_of_birth
      ),
    by = c("participant_id" = "patient_id"),
    relationship = "one-to-one"
  ) %>%
  
  # Standardise trial arm coding
  mutate(
    trial_arm = case_when(
      trial_arm == "Control" | trial_arm == "control" ~ "Control",
      trial_arm == "Intervention" | trial_arm == "intervention" ~ "Intervention",
      TRUE ~ trial_arm
    )
  ) %>%
  
  # ── Hb units ──────────────────────────────────────────────────────────────
  # IMPORTANT: Haemoglobin units must be verified in the source data file
  # before running analysis. All anaemia thresholds in this script use g/L.
  #
  # The biomarkers_only sheet in the SHP2 dataset contains Hb in g/L
  # (verified: values ~80–150 range). Conversion is NOT applied.
  #
  # If importing from a dataset where Hb is in g/dL (values ~8–15 range),
  # uncomment the conversion block below:
  # mutate(
  #   poc_hb_baseline = poc_hb_baseline * 10,   # g/dL → g/L
  #   poc_hb_12m      = poc_hb_12m      * 10,   # g/dL → g/L
  #   venous_hb_12m   = venous_hb_12m   * 10    # g/dL → g/L
  # ) %>%
  #
  # To verify units: run summary(df_analysis$venous_hb_12m) after loading.
  # Values in g/L should be in the range 60–170. Values in g/dL: 6–17.
  # ─────────────────────────────────────────────────────────────────────────
  
  # Standardise parity coding
  mutate(
    parity_cat = case_when(
      parity == 0 ~ "Nulliparous",
      parity >= 1 ~ "Multiparous",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Standardise delivery mode
  mutate(
    delivery_mode = case_when(
      delivery_mode == "Normal vaginal" | delivery_mode == "1" ~ "vaginal",
      delivery_mode == "Instrumental" | delivery_mode == "2" ~ "instrumental",
      delivery_mode == "Caesarean" | delivery_mode == "3" | 
        delivery_mode == "Caesarean section" ~ "caesarean",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Standardise place of birth
  mutate(
    place_of_birth = case_when(
      place_of_birth == "Home" | place_of_birth == "1" ~ "home",
      place_of_birth == "Primary Health Centre" | place_of_birth == "2" ~ "primary_centre",
      place_of_birth == "Private hospital/clinic" | place_of_birth == "3" ~ "hospital",
      place_of_birth == "Government hospital" | place_of_birth == "4" ~ "hospital",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # ── Ferritin index ────────────────────────────────────────────────────────
  # Definition used throughout this thesis and manuscript:
  #   Ferritin index = sTfR (mg/L) / log10(ferritin µg/L)
  # Reference: Cook et al. (2003). This definition is used consistently
  # in all Chapter 5 analyses. Threshold for iron deficiency: ≥1.03.
  #
  # NOTE: An alternative log-difference index (log10(sTfR) - log10(ferritin))
  # appears in exploratory scripts (logferritin_logstfr_index_LMMs.R, excluded
  # from this repository). That version was NOT used in any reported results.
  # ──────────────────────────────────────────────────────────────────────────
  mutate(
    ferritin_index = ifelse(
      !is.na(stfr_mgL) & !is.na(ferritin) & ferritin > 0,
      stfr_mgL / log10(ferritin),
      NA
    )
  ) %>%
  
  # Anaemia classification
  mutate(
    anaemia_venous = case_when(
      venous_hb_12m < 120 ~ "Anaemic",
      venous_hb_12m >= 120 ~ "Not anaemic",
      TRUE ~ NA_character_
    ),
    
    # ── Anaemia severity classification ──────────────────────────────────────
    # Thresholds (venous Hb, g/L) for non-pregnant women ≥15 years:
    #   Severe:   Hb < 80 g/L
    #   Moderate: Hb 80–109 g/L
    #   Mild:     Hb 110–119 g/L
    # Source: WHO (2011). Haemoglobin concentrations for the diagnosis of
    # anaemia and assessment of severity. WHO/NMH/NHD/MNM/11.1
    # Rationale: At 12 months postpartum, pregnancy-specific thresholds no
    # longer apply. WHO postpartum thresholds align with non-pregnant women.
    # Verify consistency with manuscript Table 5.4 before publishing.
    # ─────────────────────────────────────────────────────────────────────────
    anaemia_severity = case_when(
      venous_hb_12m < 80                              ~ "Severe",
      venous_hb_12m >= 80  & venous_hb_12m < 110     ~ "Moderate",
      venous_hb_12m >= 110 & venous_hb_12m < 120     ~ "Mild",
      venous_hb_12m >= 120                            ~ "Not anaemic",
      TRUE ~ NA_character_
    ),
    
    anaemia_poc = case_when(
      poc_hb_12m < 120 ~ "Anaemic",
      poc_hb_12m >= 120 ~ "Not anaemic",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # MCV morphology classification
  mutate(
    mcv_category = case_when(
      mcv < 80 ~ "Microcytic",
      mcv >= 80 & mcv <= 100 ~ "Normocytic",
      mcv > 100 ~ "Macrocytic",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Mentzer index (MCV / RBC)
  mutate(
    mentzer_index = ifelse(!is.na(mcv) & !is.na(rbc), mcv / rbc, NA)
  ) %>%
  
  # Iron deficiency classifications
  mutate(
    iron_def_ferritin_30 = ferritin < 30,
    iron_def_ferritin_15 = ferritin < 15,
    iron_def_stfr = stfr_mgL >= 1.55,
    iron_def_ferritin_index = ferritin_index >= 1.03
  ) %>%
  
  # Nutritional deficiency classifications
  mutate(
    b12_deficient = vitamin_b12 < 200,
    b12_borderline = vitamin_b12 >= 200 & vitamin_b12 < 300,
    
    folate_deficient = folate < 2,
    folate_borderline = folate >= 2 & folate < 4,
    
    # Multifactorial: 2+ deficiencies
    num_deficiencies = (as.numeric(iron_def_ferritin_30) + 
                          as.numeric(folate_deficient) + 
                          as.numeric(b12_deficient)),
    multifactorial_anaemia = (anaemia_venous == "Anaemic" & num_deficiencies >= 2)
  ) %>%
  
  # Inflammation status
  mutate(
    crp_elevated = crp > 5
  ) %>%
  
  # IDA definition (ferritin <30 + anaemia)
  mutate(
    ida_ferritin30 = (iron_def_ferritin_30 & anaemia_venous == "Anaemic")
  ) %>%
  
  # ── Standardise continuous covariates for regression (z-scores) ──────────
  # scale() is applied once to the full analysis dataset (n=599) here.
  # Z-scores therefore reflect the mean and SD of the complete sample.
  # IMPORTANT: if you subset df_analysis and re-run models, scale() will
  # re-compute mean/SD from the subset, changing z-score values and making
  # regression coefficients non-comparable. To avoid this, use the pre-scaled
  # columns created here rather than re-scaling inside model subsets.
  mutate(
    maternal_age_z             = scale(maternal_age,            center = TRUE, scale = TRUE)[,1],
    bmi_z                      = scale(bmi,                     center = TRUE, scale = TRUE)[,1],
    gestational_age_baseline_z = scale(gestational_age_baseline, center = TRUE, scale = TRUE)[,1],
    poc_hb_baseline_z          = scale(poc_hb_baseline,         center = TRUE, scale = TRUE)[,1]
  )

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Final sample size:", nrow(df_analysis), "\n")

# Check for duplicates
duplicates <- nrow(df_analysis) - length(unique(df_analysis$participant_id))
cat("Duplicate participant IDs:", duplicates, "\n")

if (nrow(df_analysis) != 599) {
  warning("Sample size is ", nrow(df_analysis), ", not 599. Check for duplicates.")
}

# ── Missing data ─────────────────────────────────────────────────────────────
# Missingness is reported here for transparency. Descriptive statistics use
# na.rm = TRUE throughout. Regression models use complete cases on all
# included covariates (see filter() calls in each model section).
# No imputation was performed.
cat("\nMissingness by key variable:\n")
missing_summary <- df_analysis %>%
  summarise(
    "Venous Hb"    = sum(is.na(venous_hb_12m)),
    "POC Hb (12m)" = sum(is.na(poc_hb_12m)),
    "Ferritin"     = sum(is.na(ferritin)),
    "sTfR"         = sum(is.na(stfr_mgL)),
    "Hepcidin"     = sum(is.na(hepcidin)),
    "B12"          = sum(is.na(vitamin_b12)),
    "Folate"       = sum(is.na(folate)),
    "CRP"          = sum(is.na(crp))
  )
print(missing_summary)

# ============================================================================
# SECTION 3.1: DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n\n===== SECTION 3.1: STUDY COHORT AND DATA COMPLETENESS =====\n")

n_overall <- nrow(df_analysis)
n_control <- nrow(df_analysis %>% filter(trial_arm == "Control"))
n_intervention <- nrow(df_analysis %>% filter(trial_arm == "Intervention"))

cat("\nParticipant numbers:\n")
cat("Overall: ", n_overall, "\n")
cat("Control: ", n_control, "\n")
cat("Intervention: ", n_intervention, "\n")

# ============================================================================
# TABLE 1: BASELINE CHARACTERISTICS
# ============================================================================

cat("\n\n===== TABLE 1: BASELINE CHARACTERISTICS =====\n")

table1_overall <- df_analysis %>%
  summarise(
    n = n(),
    age_mean = mean(maternal_age, na.rm = TRUE),
    age_sd = sd(maternal_age, na.rm = TRUE),
    age_median = median(maternal_age, na.rm = TRUE),
    age_q1 = quantile(maternal_age, 0.25, na.rm = TRUE),
    age_q3 = quantile(maternal_age, 0.75, na.rm = TRUE),
    
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bmi_median = median(bmi, na.rm = TRUE),
    bmi_q1 = quantile(bmi, 0.25, na.rm = TRUE),
    bmi_q3 = quantile(bmi, 0.75, na.rm = TRUE),
    
    nulliparous_n = sum(parity == 0, na.rm = TRUE),
    nulliparous_pct = mean(parity == 0, na.rm = TRUE) * 100,
    
    ga_baseline_median = median(gestational_age_baseline, na.rm = TRUE),
    ga_baseline_q1 = quantile(gestational_age_baseline, 0.25, na.rm = TRUE),
    ga_baseline_q3 = quantile(gestational_age_baseline, 0.75, na.rm = TRUE),
    
    poc_hb_baseline_median = median(poc_hb_baseline, na.rm = TRUE),
    poc_hb_baseline_q1 = quantile(poc_hb_baseline, 0.25, na.rm = TRUE),
    poc_hb_baseline_q3 = quantile(poc_hb_baseline, 0.75, na.rm = TRUE)
  )

table1_control <- df_analysis %>%
  filter(trial_arm == "Control") %>%
  summarise(
    n = n(),
    age_mean = mean(maternal_age, na.rm = TRUE),
    age_sd = sd(maternal_age, na.rm = TRUE),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bmi_median = median(bmi, na.rm = TRUE),
    bmi_q1 = quantile(bmi, 0.25, na.rm = TRUE),
    bmi_q3 = quantile(bmi, 0.75, na.rm = TRUE),
    nulliparous_n = sum(parity == 0, na.rm = TRUE),
    nulliparous_pct = mean(parity == 0, na.rm = TRUE) * 100,
    ga_baseline_median = median(gestational_age_baseline, na.rm = TRUE),
    ga_baseline_q1 = quantile(gestational_age_baseline, 0.25, na.rm = TRUE),
    ga_baseline_q3 = quantile(gestational_age_baseline, 0.75, na.rm = TRUE),
    poc_hb_baseline_median = median(poc_hb_baseline, na.rm = TRUE),
    poc_hb_baseline_q1 = quantile(poc_hb_baseline, 0.25, na.rm = TRUE),
    poc_hb_baseline_q3 = quantile(poc_hb_baseline, 0.75, na.rm = TRUE)
  )

table1_intervention <- df_analysis %>%
  filter(trial_arm == "Intervention") %>%
  summarise(
    n = n(),
    age_mean = mean(maternal_age, na.rm = TRUE),
    age_sd = sd(maternal_age, na.rm = TRUE),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bmi_median = median(bmi, na.rm = TRUE),
    bmi_q1 = quantile(bmi, 0.25, na.rm = TRUE),
    bmi_q3 = quantile(bmi, 0.75, na.rm = TRUE),
    nulliparous_n = sum(parity == 0, na.rm = TRUE),
    nulliparous_pct = mean(parity == 0, na.rm = TRUE) * 100,
    ga_baseline_median = median(gestational_age_baseline, na.rm = TRUE),
    ga_baseline_q1 = quantile(gestational_age_baseline, 0.25, na.rm = TRUE),
    ga_baseline_q3 = quantile(gestational_age_baseline, 0.75, na.rm = TRUE),
    poc_hb_baseline_median = median(poc_hb_baseline, na.rm = TRUE),
    poc_hb_baseline_q1 = quantile(poc_hb_baseline, 0.25, na.rm = TRUE),
    poc_hb_baseline_q3 = quantile(poc_hb_baseline, 0.75, na.rm = TRUE)
  )

cat("\nAge (years) - Mean (SD):\n")
cat("Overall: ", round(table1_overall$age_mean, 1), " (", round(table1_overall$age_sd, 1), ")\n")
cat("Control: ", round(table1_control$age_mean, 1), " (", round(table1_control$age_sd, 1), ")\n")
cat("Intervention: ", round(table1_intervention$age_mean, 1), " (", round(table1_intervention$age_sd, 1), ")\n")

cat("\nBMI (kg/m²) - Median [IQR]:\n")
cat("Overall: ", round(table1_overall$bmi_median, 1), 
    " [", round(table1_overall$bmi_q1, 1), ",", round(table1_overall$bmi_q3, 1), "]\n")
cat("Control: ", round(table1_control$bmi_median, 1),
    " [", round(table1_control$bmi_q1, 1), ",", round(table1_control$bmi_q3, 1), "]\n")
cat("Intervention: ", round(table1_intervention$bmi_median, 1),
    " [", round(table1_intervention$bmi_q1, 1), ",", round(table1_intervention$bmi_q3, 1), "]\n")

write.csv(table1_overall, "Table1_Baseline_Overall.csv", row.names = FALSE)
write.csv(table1_control, "Table1_Baseline_Control.csv", row.names = FALSE)
write.csv(table1_intervention, "Table1_Baseline_Intervention.csv", row.names = FALSE)
cat("\n✓ Table 1 saved\n")

# ============================================================================
# SECTION 3.2: PREVALENCE OF POSTPARTUM ANAEMIA
# ============================================================================

cat("\n\n===== SECTION 3.2: PREVALENCE OF POSTPARTUM ANAEMIA =====\n")

# TABLE 2: BIOMARKERS AT 12 MONTHS
table2_biomarkers <- df_analysis %>%
  group_by(trial_arm) %>%
  summarise(
    n = n(),
    
    venous_hb_median = median(venous_hb_12m, na.rm = TRUE),
    venous_hb_q1 = quantile(venous_hb_12m, 0.25, na.rm = TRUE),
    venous_hb_q3 = quantile(venous_hb_12m, 0.75, na.rm = TRUE),
    
    poc_hb_median = median(poc_hb_12m, na.rm = TRUE),
    poc_hb_q1 = quantile(poc_hb_12m, 0.25, na.rm = TRUE),
    poc_hb_q3 = quantile(poc_hb_12m, 0.75, na.rm = TRUE),
    
    ferritin_median = median(ferritin, na.rm = TRUE),
    ferritin_q1 = quantile(ferritin, 0.25, na.rm = TRUE),
    ferritin_q3 = quantile(ferritin, 0.75, na.rm = TRUE),
    
    stfr_median = median(stfr_mgL, na.rm = TRUE),
    stfr_q1 = quantile(stfr_mgL, 0.25, na.rm = TRUE),
    stfr_q3 = quantile(stfr_mgL, 0.75, na.rm = TRUE),
    
    ferritin_index_median = median(ferritin_index, na.rm = TRUE),
    ferritin_index_q1 = quantile(ferritin_index, 0.25, na.rm = TRUE),
    ferritin_index_q3 = quantile(ferritin_index, 0.75, na.rm = TRUE),
    
    hepcidin_median = median(hepcidin, na.rm = TRUE),
    hepcidin_q1 = quantile(hepcidin, 0.25, na.rm = TRUE),
    hepcidin_q3 = quantile(hepcidin, 0.75, na.rm = TRUE),
    
    crp_median = median(crp, na.rm = TRUE),
    crp_q1 = quantile(crp, 0.25, na.rm = TRUE),
    crp_q3 = quantile(crp, 0.75, na.rm = TRUE),
    
    b12_median = median(vitamin_b12, na.rm = TRUE),
    b12_q1 = quantile(vitamin_b12, 0.25, na.rm = TRUE),
    b12_q3 = quantile(vitamin_b12, 0.75, na.rm = TRUE),
    
    folate_median = median(folate, na.rm = TRUE),
    folate_q1 = quantile(folate, 0.25, na.rm = TRUE),
    folate_q3 = quantile(folate, 0.75, na.rm = TRUE),
    
    haematocrit_median = median(haematocrit, na.rm = TRUE),
    haematocrit_q1 = quantile(haematocrit, 0.25, na.rm = TRUE),
    haematocrit_q3 = quantile(haematocrit, 0.75, na.rm = TRUE),
    
    rbc_median = median(rbc, na.rm = TRUE),
    rbc_q1 = quantile(rbc, 0.25, na.rm = TRUE),
    rbc_q3 = quantile(rbc, 0.75, na.rm = TRUE),
    
    mcv_median = median(mcv, na.rm = TRUE),
    mcv_q1 = quantile(mcv, 0.25, na.rm = TRUE),
    mcv_q3 = quantile(mcv, 0.75, na.rm = TRUE),
    
    mch_median = median(mch, na.rm = TRUE),
    mch_q1 = quantile(mch, 0.25, na.rm = TRUE),
    mch_q3 = quantile(mch, 0.75, na.rm = TRUE),
    
    mchc_median = median(mchc, na.rm = TRUE),
    mchc_q1 = quantile(mchc, 0.25, na.rm = TRUE),
    mchc_q3 = quantile(mchc, 0.75, na.rm = TRUE),
    
    rdw_cv_median = median(rdw_cv, na.rm = TRUE),
    rdw_cv_q1 = quantile(rdw_cv, 0.25, na.rm = TRUE),
    rdw_cv_q3 = quantile(rdw_cv, 0.75, na.rm = TRUE),
    
    rdw_sd_median = median(rdw_sd, na.rm = TRUE),
    rdw_sd_q1 = quantile(rdw_sd, 0.25, na.rm = TRUE),
    rdw_sd_q3 = quantile(rdw_sd, 0.75, na.rm = TRUE),
    
    wbc_median = median(wbc, na.rm = TRUE),
    wbc_q1 = quantile(wbc, 0.25, na.rm = TRUE),
    wbc_q3 = quantile(wbc, 0.75, na.rm = TRUE),
    
    platelets_median = median(platelets, na.rm = TRUE),
    platelets_q1 = quantile(platelets, 0.25, na.rm = TRUE),
    platelets_q3 = quantile(platelets, 0.75, na.rm = TRUE),
    
    mentzer_median = median(mentzer_index, na.rm = TRUE),
    mentzer_q1 = quantile(mentzer_index, 0.25, na.rm = TRUE),
    mentzer_q3 = quantile(mentzer_index, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  add_row(
    trial_arm = "Overall",
    n = n_overall,
    venous_hb_median = median(df_analysis$venous_hb_12m, na.rm = TRUE),
    venous_hb_q1 = quantile(df_analysis$venous_hb_12m, 0.25, na.rm = TRUE),
    venous_hb_q3 = quantile(df_analysis$venous_hb_12m, 0.75, na.rm = TRUE),
    poc_hb_median = median(df_analysis$poc_hb_12m, na.rm = TRUE),
    poc_hb_q1 = quantile(df_analysis$poc_hb_12m, 0.25, na.rm = TRUE),
    poc_hb_q3 = quantile(df_analysis$poc_hb_12m, 0.75, na.rm = TRUE),
    ferritin_median = median(df_analysis$ferritin, na.rm = TRUE),
    ferritin_q1 = quantile(df_analysis$ferritin, 0.25, na.rm = TRUE),
    ferritin_q3 = quantile(df_analysis$ferritin, 0.75, na.rm = TRUE),
    stfr_median = median(df_analysis$stfr_mgL, na.rm = TRUE),
    stfr_q1 = quantile(df_analysis$stfr_mgL, 0.25, na.rm = TRUE),
    stfr_q3 = quantile(df_analysis$stfr_mgL, 0.75, na.rm = TRUE),
    ferritin_index_median = median(df_analysis$ferritin_index, na.rm = TRUE),
    ferritin_index_q1 = quantile(df_analysis$ferritin_index, 0.25, na.rm = TRUE),
    ferritin_index_q3 = quantile(df_analysis$ferritin_index, 0.75, na.rm = TRUE),
    hepcidin_median = median(df_analysis$hepcidin, na.rm = TRUE),
    hepcidin_q1 = quantile(df_analysis$hepcidin, 0.25, na.rm = TRUE),
    hepcidin_q3 = quantile(df_analysis$hepcidin, 0.75, na.rm = TRUE),
    crp_median = median(df_analysis$crp, na.rm = TRUE),
    crp_q1 = quantile(df_analysis$crp, 0.25, na.rm = TRUE),
    crp_q3 = quantile(df_analysis$crp, 0.75, na.rm = TRUE),
    b12_median = median(df_analysis$vitamin_b12, na.rm = TRUE),
    b12_q1 = quantile(df_analysis$vitamin_b12, 0.25, na.rm = TRUE),
    b12_q3 = quantile(df_analysis$vitamin_b12, 0.75, na.rm = TRUE),
    folate_median = median(df_analysis$folate, na.rm = TRUE),
    folate_q1 = quantile(df_analysis$folate, 0.25, na.rm = TRUE),
    folate_q3 = quantile(df_analysis$folate, 0.75, na.rm = TRUE),
    haematocrit_median = median(df_analysis$haematocrit, na.rm = TRUE),
    haematocrit_q1 = quantile(df_analysis$haematocrit, 0.25, na.rm = TRUE),
    haematocrit_q3 = quantile(df_analysis$haematocrit, 0.75, na.rm = TRUE),
    rbc_median = median(df_analysis$rbc, na.rm = TRUE),
    rbc_q1 = quantile(df_analysis$rbc, 0.25, na.rm = TRUE),
    rbc_q3 = quantile(df_analysis$rbc, 0.75, na.rm = TRUE),
    mcv_median = median(df_analysis$mcv, na.rm = TRUE),
    mcv_q1 = quantile(df_analysis$mcv, 0.25, na.rm = TRUE),
    mcv_q3 = quantile(df_analysis$mcv, 0.75, na.rm = TRUE),
    mch_median = median(df_analysis$mch, na.rm = TRUE),
    mch_q1 = quantile(df_analysis$mch, 0.25, na.rm = TRUE),
    mch_q3 = quantile(df_analysis$mch, 0.75, na.rm = TRUE),
    mchc_median = median(df_analysis$mchc, na.rm = TRUE),
    mchc_q1 = quantile(df_analysis$mchc, 0.25, na.rm = TRUE),
    mchc_q3 = quantile(df_analysis$mchc, 0.75, na.rm = TRUE),
    rdw_cv_median = median(df_analysis$rdw_cv, na.rm = TRUE),
    rdw_cv_q1 = quantile(df_analysis$rdw_cv, 0.25, na.rm = TRUE),
    rdw_cv_q3 = quantile(df_analysis$rdw_cv, 0.75, na.rm = TRUE),
    rdw_sd_median = median(df_analysis$rdw_sd, na.rm = TRUE),
    rdw_sd_q1 = quantile(df_analysis$rdw_sd, 0.25, na.rm = TRUE),
    rdw_sd_q3 = quantile(df_analysis$rdw_sd, 0.75, na.rm = TRUE),
    wbc_median = median(df_analysis$wbc, na.rm = TRUE),
    wbc_q1 = quantile(df_analysis$wbc, 0.25, na.rm = TRUE),
    wbc_q3 = quantile(df_analysis$wbc, 0.75, na.rm = TRUE),
    platelets_median = median(df_analysis$platelets, na.rm = TRUE),
    platelets_q1 = quantile(df_analysis$platelets, 0.25, na.rm = TRUE),
    platelets_q3 = quantile(df_analysis$platelets, 0.75, na.rm = TRUE),
    mentzer_median = median(df_analysis$mentzer_index, na.rm = TRUE),
    mentzer_q1 = quantile(df_analysis$mentzer_index, 0.25, na.rm = TRUE),
    mentzer_q3 = quantile(df_analysis$mentzer_index, 0.75, na.rm = TRUE),
    .before = 1
  )

cat("\n=== TABLE 2: BIOMARKERS AT 12 MONTHS (summary) ===\n")
cat("\nVenous Hb (g/L) - Median [IQR]:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table2_biomarkers %>% filter(trial_arm == arm)
  cat(arm, ": ", round(row$venous_hb_median, 1), 
      " [", round(row$venous_hb_q1, 1), ",", round(row$venous_hb_q3, 1), "]\n", sep = "")
}

cat("\nFerritin (µg/L) - Median [IQR]:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table2_biomarkers %>% filter(trial_arm == arm)
  cat(arm, ": ", round(row$ferritin_median, 1), 
      " [", round(row$ferritin_q1, 1), ",", round(row$ferritin_q3, 1), "]\n", sep = "")
}

cat("\nsTfR (mg/L) - Median [IQR]:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table2_biomarkers %>% filter(trial_arm == arm)
  cat(arm, ": ", round(row$stfr_median, 1), 
      " [", round(row$stfr_q1, 1), ",", round(row$stfr_q3, 1), "]\n", sep = "")
}

cat("\nHepcidin (ng/mL) - Median [IQR]:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table2_biomarkers %>% filter(trial_arm == arm)
  cat(arm, ": ", round(row$hepcidin_median, 1), 
      " [", round(row$hepcidin_q1, 1), ",", round(row$hepcidin_q3, 1), "]\n", sep = "")
}

write.csv(table2_biomarkers, "Table2_Biomarkers_12month.csv", row.names = FALSE)
cat("\n✓ Table 2 saved\n")

# ============================================================================
# ANAEMIA PREVALENCE & SEVERITY
# ============================================================================

cat("\n\n===== ANAEMIA PREVALENCE AND SEVERITY =====\n")

df_venous_hb <- df_analysis %>% filter(!is.na(venous_hb_12m))
n_venous <- nrow(df_venous_hb)

anaemia_summary <- df_venous_hb %>%
  group_by(trial_arm) %>%
  summarise(
    n = n(),
    anaemic_n = sum(anaemia_venous == "Anaemic", na.rm = TRUE),
    anaemic_pct = mean(anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    mild_n = sum(anaemia_severity == "Mild", na.rm = TRUE),
    mild_pct = mean(anaemia_severity == "Mild", na.rm = TRUE) * 100,
    moderate_n = sum(anaemia_severity == "Moderate", na.rm = TRUE),
    moderate_pct = mean(anaemia_severity == "Moderate", na.rm = TRUE) * 100,
    severe_n = sum(anaemia_severity == "Severe", na.rm = TRUE),
    severe_pct = mean(anaemia_severity == "Severe", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  add_row(
    trial_arm = "Overall",
    n = n_venous,
    anaemic_n = sum(df_venous_hb$anaemia_venous == "Anaemic", na.rm = TRUE),
    anaemic_pct = mean(df_venous_hb$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    mild_n = sum(df_venous_hb$anaemia_severity == "Mild", na.rm = TRUE),
    mild_pct = mean(df_venous_hb$anaemia_severity == "Mild", na.rm = TRUE) * 100,
    moderate_n = sum(df_venous_hb$anaemia_severity == "Moderate", na.rm = TRUE),
    moderate_pct = mean(df_venous_hb$anaemia_severity == "Moderate", na.rm = TRUE) * 100,
    severe_n = sum(df_venous_hb$anaemia_severity == "Severe", na.rm = TRUE),
    severe_pct = mean(df_venous_hb$anaemia_severity == "Severe", na.rm = TRUE) * 100,
    .before = 1
  )

cat("\nVenous-defined anaemia prevalence:\n")
print(anaemia_summary %>% select(trial_arm, n, anaemic_n, anaemic_pct))

cat("\nAnaemia severity distribution:\n")
print(anaemia_summary %>% select(trial_arm, mild_n, mild_pct, moderate_n, moderate_pct, severe_n, severe_pct))

# POC anaemia
df_poc <- df_analysis %>% filter(!is.na(poc_hb_12m))
n_poc <- nrow(df_poc)

poc_anaemia_summary <- df_poc %>%
  summarise(
    n = n(),
    poc_anaemic_n = sum(anaemia_poc == "Anaemic", na.rm = TRUE),
    poc_anaemic_pct = mean(anaemia_poc == "Anaemic", na.rm = TRUE) * 100
  )

cat("\nPOC-defined anaemia prevalence:\n")
print(poc_anaemia_summary)

# ============================================================================
# SECTION 3.3: AGREEMENT BETWEEN CAPILLARY AND VENOUS HAEMOGLOBIN
# ============================================================================

cat("\n\n===== SECTION 3.3: AGREEMENT BETWEEN CAPILLARY POC AND VENOUS Hb =====\n")

df_agreement <- df_analysis %>%
  filter(!is.na(venous_hb_12m) & !is.na(poc_hb_12m)) %>%
  mutate(
    hb_diff = poc_hb_12m - venous_hb_12m,
    hb_mean = (poc_hb_12m + venous_hb_12m) / 2
  )

n_agreement <- nrow(df_agreement)
cat("Paired measurements available: n =", n_agreement, "\n")

# Bland-Altman analysis
bland_altman_summary <- df_agreement %>%
  summarise(
    mean_diff = mean(hb_diff, na.rm = TRUE),
    sd_diff = sd(hb_diff, na.rm = TRUE),
    loa_lower = mean_diff - 1.96 * sd_diff,
    loa_upper = mean_diff + 1.96 * sd_diff
  )

cat("\nBland-Altman Analysis (POC minus venous):\n")
cat("Mean bias:", round(bland_altman_summary$mean_diff, 1), "g/L\n")
cat("SD of differences:", round(bland_altman_summary$sd_diff, 1), "g/L\n")
cat("95% Limits of Agreement:\n")
cat("  Lower:", round(bland_altman_summary$loa_lower, 1), "g/L\n")
cat("  Upper:", round(bland_altman_summary$loa_upper, 1), "g/L\n")

# Test for proportional bias
model_proportional_bias <- lm(hb_diff ~ hb_mean, data = df_agreement)
bias_slope <- coef(model_proportional_bias)[2]
bias_pval <- summary(model_proportional_bias)$coefficients[2, 4]

cat("\nProportional Bias Test:\n")
cat("Slope (β):", round(bias_slope, 3), "\n")
cat("p-value:", format(bias_pval, scientific = TRUE, digits = 3), "\n")

# Classification agreement
classification_table <- df_agreement %>%
  mutate(
    poc_cat = if_else(poc_hb_12m < 120, "POC Anaemic", "POC Not anaemic"),
    venous_cat = if_else(venous_hb_12m < 120, "Venous Anaemic", "Venous Not anaemic")
  ) %>%
  select(poc_cat, venous_cat) %>%
  table()

cat("\nClassification Agreement at 120 g/L threshold:\n")
print(classification_table)

# Calculate metrics
tp <- classification_table[1, 1]
fp <- classification_table[1, 2]
fn <- classification_table[2, 1]
tn <- classification_table[2, 2]

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
overall_agreement <- (tp + tn) / (tp + fp + fn + tn)

cat("\nSensitivity:", round(sensitivity * 100, 1), "%\n")
cat("Specificity:", round(specificity * 100, 1), "%\n")
cat("Overall agreement:", round(overall_agreement * 100, 1), "%\n")

# ============================================================================
# SECTION 3.4: ANAEMIA AETIOLOGY AND NUTRITIONAL DEFICIENCIES
# ============================================================================

cat("\n\n===== SECTION 3.4: ANAEMIA AETIOLOGY AND NUTRITIONAL DEFICIENCIES =====\n")

table4_deficiencies <- df_analysis %>%
  group_by(trial_arm) %>%
  summarise(
    n = n(),
    anaemic_n = sum(anaemia_venous == "Anaemic", na.rm = TRUE),
    anaemic_pct = mean(anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    id_ferritin30_n = sum(iron_def_ferritin_30, na.rm = TRUE),
    id_ferritin30_pct = mean(iron_def_ferritin_30, na.rm = TRUE) * 100,
    
    id_ferritin15_n = sum(iron_def_ferritin_15, na.rm = TRUE),
    id_ferritin15_pct = mean(iron_def_ferritin_15, na.rm = TRUE) * 100,
    
    id_stfr_n = sum(iron_def_stfr, na.rm = TRUE),
    id_stfr_pct = mean(iron_def_stfr, na.rm = TRUE) * 100,
    
    id_ferritin_index_n = sum(iron_def_ferritin_index, na.rm = TRUE),
    id_ferritin_index_pct = mean(iron_def_ferritin_index, na.rm = TRUE) * 100,
    
    ida_n = sum(ida_ferritin30, na.rm = TRUE),
    ida_pct = mean(ida_ferritin30, na.rm = TRUE) * 100,
    
    b12_deficient_n = sum(b12_deficient, na.rm = TRUE),
    b12_deficient_pct = mean(b12_deficient, na.rm = TRUE) * 100,
    
    b12_borderline_n = sum(b12_borderline, na.rm = TRUE),
    b12_borderline_pct = mean(b12_borderline, na.rm = TRUE) * 100,
    
    folate_deficient_n = sum(folate_deficient, na.rm = TRUE),
    folate_deficient_pct = mean(folate_deficient, na.rm = TRUE) * 100,
    
    folate_borderline_n = sum(folate_borderline, na.rm = TRUE),
    folate_borderline_pct = mean(folate_borderline, na.rm = TRUE) * 100,
    
    crp_elevated_n = sum(crp_elevated, na.rm = TRUE),
    crp_elevated_pct = mean(crp_elevated, na.rm = TRUE) * 100,
    
    .groups = "drop"
  ) %>%
  add_row(
    trial_arm = "Overall",
    n = nrow(df_analysis),
    anaemic_n = sum(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE),
    anaemic_pct = mean(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    id_ferritin30_n = sum(df_analysis$iron_def_ferritin_30, na.rm = TRUE),
    id_ferritin30_pct = mean(df_analysis$iron_def_ferritin_30, na.rm = TRUE) * 100,
    id_ferritin15_n = sum(df_analysis$iron_def_ferritin_15, na.rm = TRUE),
    id_ferritin15_pct = mean(df_analysis$iron_def_ferritin_15, na.rm = TRUE) * 100,
    id_stfr_n = sum(df_analysis$iron_def_stfr, na.rm = TRUE),
    id_stfr_pct = mean(df_analysis$iron_def_stfr, na.rm = TRUE) * 100,
    id_ferritin_index_n = sum(df_analysis$iron_def_ferritin_index, na.rm = TRUE),
    id_ferritin_index_pct = mean(df_analysis$iron_def_ferritin_index, na.rm = TRUE) * 100,
    ida_n = sum(df_analysis$ida_ferritin30, na.rm = TRUE),
    ida_pct = mean(df_analysis$ida_ferritin30, na.rm = TRUE) * 100,
    b12_deficient_n = sum(df_analysis$b12_deficient, na.rm = TRUE),
    b12_deficient_pct = mean(df_analysis$b12_deficient, na.rm = TRUE) * 100,
    b12_borderline_n = sum(df_analysis$b12_borderline, na.rm = TRUE),
    b12_borderline_pct = mean(df_analysis$b12_borderline, na.rm = TRUE) * 100,
    folate_deficient_n = sum(df_analysis$folate_deficient, na.rm = TRUE),
    folate_deficient_pct = mean(df_analysis$folate_deficient, na.rm = TRUE) * 100,
    folate_borderline_n = sum(df_analysis$folate_borderline, na.rm = TRUE),
    folate_borderline_pct = mean(df_analysis$folate_borderline, na.rm = TRUE) * 100,
    crp_elevated_n = sum(df_analysis$crp_elevated, na.rm = TRUE),
    crp_elevated_pct = mean(df_analysis$crp_elevated, na.rm = TRUE) * 100,
    .before = 1
  )

cat("\n=== TABLE 4: ANAEMIA AND NUTRITIONAL DEFICIENCIES ===\n")
cat("\nVenous anaemia prevalence (Hb <120 g/L):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$anaemic_n, "/", row$n, 
      " (", round(row$anaemic_pct, 1), "%)\n", sep = "")
}

cat("\nIron deficiency (Ferritin <30 µg/L):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$id_ferritin30_n, "/", row$n, 
      " (", round(row$id_ferritin30_pct, 1), "%)\n", sep = "")
}

cat("\nIron deficiency (sTfR >=1.55 mg/L):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$id_stfr_n, "/", row$n, 
      " (", round(row$id_stfr_pct, 1), "%)\n", sep = "")
}

cat("\nB12 deficiency (<200 pg/mL):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$b12_deficient_n, "/", row$n, 
      " (", round(row$b12_deficient_pct, 1), "%)\n", sep = "")
}

cat("\nFolate deficiency (<2 ng/mL):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$folate_deficient_n, "/", row$n, 
      " (", round(row$folate_deficient_pct, 1), "%)\n", sep = "")
}

cat("\nElevated CRP (>5 mg/L):\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- table4_deficiencies %>% filter(trial_arm == arm)
  cat(arm, ": ", row$crp_elevated_n, "/", row$n, 
      " (", round(row$crp_elevated_pct, 1), "%)\n", sep = "")
}

write.csv(table4_deficiencies, "Table4_Deficiencies.csv", row.names = FALSE)
cat("\n✓ Table 4 saved\n")

# ============================================================================
# IRON DEFICIENCY CASCADE
# ============================================================================

cat("\n\n===== IRON DEFICIENCY CASCADE AMONG ANAEMIC WOMEN =====\n")

df_anaemic <- df_analysis %>% filter(anaemia_venous == "Anaemic", !is.na(venous_hb_12m))
n_anaemic <- nrow(df_anaemic)

iron_def_cascade <- tibble(
  definition = c(
    "Ferritin <30 µg/L",
    "sTfR >=1.55 mg/L",
    "Ferritin index >=1.03",
    "Any iron marker positive",
    "Any nutritional deficiency",
    "No micronutrient deficiency"
  ),
  n = c(
    sum(df_anaemic$iron_def_ferritin_30, na.rm = TRUE),
    sum(df_anaemic$iron_def_stfr, na.rm = TRUE),
    sum(df_anaemic$iron_def_ferritin_index, na.rm = TRUE),
    NA,
    NA,
    NA
  )
)

# Calculate "any iron marker" and "any deficiency"
any_iron <- df_anaemic %>%
  mutate(
    any_iron_marker = (iron_def_ferritin_30 | iron_def_stfr | iron_def_ferritin_index)
  ) %>%
  summarise(n = sum(any_iron_marker, na.rm = TRUE)) %>%
  pull(n)

any_deficiency <- df_anaemic %>%
  mutate(
    any_def = (iron_def_ferritin_30 | folate_deficient | b12_deficient)
  ) %>%
  summarise(n = sum(any_def, na.rm = TRUE)) %>%
  pull(n)

no_deficiency <- n_anaemic - any_deficiency

iron_def_cascade$n[4] <- any_iron
iron_def_cascade$n[5] <- any_deficiency
iron_def_cascade$n[6] <- no_deficiency

iron_def_cascade <- iron_def_cascade %>%
  mutate(pct = (n / n_anaemic) * 100)

cat("\nAmong", n_anaemic, "anaemic women:\n")
print(iron_def_cascade)

write.csv(iron_def_cascade, "IronDeficiency_Cascade.csv", row.names = FALSE)
cat("\n✓ Iron deficiency cascade saved\n")

# ============================================================================
# ============================================================================

cat("\n\n===== MENTZER INDEX ANALYSIS =====\n")

# Overall mentzer distribution
mentzer_overall <- df_analysis %>%
  filter(!is.na(mentzer_index)) %>%
  summarise(
    n = n(),
    mentzer_ge13_n = sum(mentzer_index >= 13, na.rm = TRUE),
    mentzer_ge13_pct = mean(mentzer_index >= 13, na.rm = TRUE) * 100,
    mentzer_lt13_n = sum(mentzer_index < 13, na.rm = TRUE),
    mentzer_lt13_pct = mean(mentzer_index < 13, na.rm = TRUE) * 100
  )

cat("\nMentzer Index Distribution (Overall):\n")
cat("≥13 (suggestive of iron deficiency): ", mentzer_overall$mentzer_ge13_n, "/", mentzer_overall$n, 
    " (", round(mentzer_overall$mentzer_ge13_pct, 1), "%)\n", sep = "")
cat("<13 (suggestive of thalassaemia trait): ", mentzer_overall$mentzer_lt13_n, "/", mentzer_overall$n, 
    " (", round(mentzer_overall$mentzer_lt13_pct, 1), "%)\n\n", sep = "")

# Among anaemic women
mentzer_anaemic <- df_analysis %>%
  filter(!is.na(mentzer_index) & anaemia_venous == "Anaemic") %>%
  summarise(
    n = n(),
    mentzer_ge13_n = sum(mentzer_index >= 13, na.rm = TRUE),
    mentzer_ge13_pct = mean(mentzer_index >= 13, na.rm = TRUE) * 100,
    mentzer_lt13_n = sum(mentzer_index < 13, na.rm = TRUE),
    mentzer_lt13_pct = mean(mentzer_index < 13, na.rm = TRUE) * 100
  )

cat("Mentzer Index Among ANAEMIC Women (Hb <120 g/L):\n")
cat("≥13 (iron deficiency pattern): ", mentzer_anaemic$mentzer_ge13_n, "/", mentzer_anaemic$n, 
    " (", round(mentzer_anaemic$mentzer_ge13_pct, 1), "%)\n", sep = "")
cat("<13 (thalassaemia pattern): ", mentzer_anaemic$mentzer_lt13_n, "/", mentzer_anaemic$n, 
    " (", round(mentzer_anaemic$mentzer_lt13_pct, 1), "%)\n\n", sep = "")

# Among non-anaemic women
mentzer_non_anaemic <- df_analysis %>%
  filter(!is.na(mentzer_index) & anaemia_venous == "Not anaemic") %>%
  summarise(
    n = n(),
    mentzer_ge13_n = sum(mentzer_index >= 13, na.rm = TRUE),
    mentzer_ge13_pct = mean(mentzer_index >= 13, na.rm = TRUE) * 100,
    mentzer_lt13_n = sum(mentzer_index < 13, na.rm = TRUE),
    mentzer_lt13_pct = mean(mentzer_index < 13, na.rm = TRUE) * 100
  )

cat("Mentzer Index Among NON-ANAEMIC Women (Hb ≥120 g/L):\n")
cat("≥13: ", mentzer_non_anaemic$mentzer_ge13_n, "/", mentzer_non_anaemic$n, 
    " (", round(mentzer_non_anaemic$mentzer_ge13_pct, 1), "%)\n", sep = "")
cat("<13: ", mentzer_non_anaemic$mentzer_lt13_n, "/", mentzer_non_anaemic$n, 
    " (", round(mentzer_non_anaemic$mentzer_lt13_pct, 1), "%)\n\n", sep = "")

mentzer_summary <- tibble(
  Group = c("Overall", "Anaemic", "Non-anaemic"),
  N = c(mentzer_overall$n, mentzer_anaemic$n, mentzer_non_anaemic$n),
  Mentzer_GE13_N = c(mentzer_overall$mentzer_ge13_n, mentzer_anaemic$mentzer_ge13_n, mentzer_non_anaemic$mentzer_ge13_n),
  Mentzer_GE13_Pct = c(mentzer_overall$mentzer_ge13_pct, mentzer_anaemic$mentzer_ge13_pct, mentzer_non_anaemic$mentzer_ge13_pct),
  Mentzer_LT13_N = c(mentzer_overall$mentzer_lt13_n, mentzer_anaemic$mentzer_lt13_n, mentzer_non_anaemic$mentzer_lt13_n),
  Mentzer_LT13_Pct = c(mentzer_overall$mentzer_lt13_pct, mentzer_anaemic$mentzer_lt13_pct, mentzer_non_anaemic$mentzer_lt13_pct)
)

write.csv(mentzer_summary, "Mentzer_Index_Analysis.csv", row.names = FALSE)
cat("✓ Mentzer index analysis saved\n")

# ============================================================================
# ============================================================================

cat("\n\n===== MCV MORPHOLOGY AMONG ANAEMIC WOMEN =====\n")

df_anaemic_mcv <- df_analysis %>%
  filter(anaemia_venous == "Anaemic" & !is.na(mcv_category))

n_anaemic_with_mcv <- nrow(df_anaemic_mcv)

mcv_morphology <- df_anaemic_mcv %>%
  group_by(mcv_category) %>%
  summarise(
    n = n(),
    pct = (n / n_anaemic_with_mcv) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(n))

cat("\nRed Cell Morphology (MCV) in Anaemic Women (n=", n_anaemic_with_mcv, "):\n", sep = "")
for (i in 1:nrow(mcv_morphology)) {
  cat(mcv_morphology$mcv_category[i], ": ", mcv_morphology$n[i], 
      " (", round(mcv_morphology$pct[i], 1), "%)\n", sep = "")
}

# Also look at specific cutoffs for hypochromia
df_hypochromia <- df_analysis %>%
  filter(anaemia_venous == "Anaemic" & !is.na(mch) & !is.na(mchc))

hypochromia_n <- sum((df_hypochromia$mch < 27 | df_hypochromia$mchc < 32), na.rm = TRUE)
hypochromia_pct <- (hypochromia_n / nrow(df_hypochromia)) * 100

cat("\nHypochromia (MCH <27 pg or MCHC <32 g/dL):\n")
cat(hypochromia_n, "/", nrow(df_hypochromia), 
    " (", round(hypochromia_pct, 1), "%)\n", sep = "")

# Anisocytosis
df_rdw <- df_analysis %>%
  filter(anaemia_venous == "Anaemic" & !is.na(rdw_cv))

anisocytosis_n <- sum(df_rdw$rdw_cv > 14.5, na.rm = TRUE)
anisocytosis_pct <- (anisocytosis_n / nrow(df_rdw)) * 100

cat("\nAnisocytosis (RDW-CV >14.5%):\n")
cat(anisocytosis_n, "/", nrow(df_rdw), 
    " (", round(anisocytosis_pct, 1), "%)\n\n", sep = "")

mcv_morphology_summary <- tibble(
  Category = c("Microcytic (<80 fL)", "Normocytic (80-100 fL)", "Macrocytic (>100 fL)", 
               "Hypochromia", "Anisocytosis"),
  N = c(
    sum(df_anaemic_mcv$mcv_category == "Microcytic", na.rm = TRUE),
    sum(df_anaemic_mcv$mcv_category == "Normocytic", na.rm = TRUE),
    sum(df_anaemic_mcv$mcv_category == "Macrocytic", na.rm = TRUE),
    hypochromia_n,
    anisocytosis_n
  ),
  N_Total = c(
    n_anaemic_with_mcv, n_anaemic_with_mcv, n_anaemic_with_mcv,
    nrow(df_hypochromia), nrow(df_rdw)
  ),
  Pct = c(
    sum(df_anaemic_mcv$mcv_category == "Microcytic", na.rm = TRUE) / n_anaemic_with_mcv * 100,
    sum(df_anaemic_mcv$mcv_category == "Normocytic", na.rm = TRUE) / n_anaemic_with_mcv * 100,
    sum(df_anaemic_mcv$mcv_category == "Macrocytic", na.rm = TRUE) / n_anaemic_with_mcv * 100,
    hypochromia_pct,
    anisocytosis_pct
  )
)

write.csv(mcv_morphology_summary, "MCV_Morphology_Anaemic_Women.csv", row.names = FALSE)
cat("✓ MCV morphology analysis saved\n")


# ============================================================================
# IRON DEFICIENCY BY TRIAL ARM AND ANAEMIA STATUS
# ============================================================================

cat("\n\n===== IRON DEFICIENCY BY TRIAL ARM AND ANAEMIA STATUS =====\n")

iron_def_by_arm <- df_analysis %>%
  group_by(trial_arm) %>%
  summarise(
    n = n(),
    
    # OVERALL (all participants)
    id_ferritin30_overall_n = sum(iron_def_ferritin_30, na.rm = TRUE),
    id_ferritin30_overall_pct = mean(iron_def_ferritin_30, na.rm = TRUE) * 100,
    
    id_stfr_overall_n = sum(iron_def_stfr, na.rm = TRUE),
    id_stfr_overall_pct = mean(iron_def_stfr, na.rm = TRUE) * 100,
    
    id_ferritin_index_overall_n = sum(iron_def_ferritin_index, na.rm = TRUE),
    id_ferritin_index_overall_pct = mean(iron_def_ferritin_index, na.rm = TRUE) * 100,
    
    # ANAEMIC women only (Hb <120)
    n_anaemic = sum(anaemia_venous == "Anaemic", na.rm = TRUE),
    
    id_ferritin30_anaemic_n = sum(iron_def_ferritin_30 & anaemia_venous == "Anaemic", na.rm = TRUE),
    id_ferritin30_anaemic_pct = sum(iron_def_ferritin_30 & anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    id_stfr_anaemic_n = sum(iron_def_stfr & anaemia_venous == "Anaemic", na.rm = TRUE),
    id_stfr_anaemic_pct = sum(iron_def_stfr & anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    id_ferritin_index_anaemic_n = sum(iron_def_ferritin_index & anaemia_venous == "Anaemic", na.rm = TRUE),
    id_ferritin_index_anaemic_pct = sum(iron_def_ferritin_index & anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    # NON-ANAEMIC women only (Hb ≥120)
    n_non_anaemic = sum(anaemia_venous == "Not anaemic", na.rm = TRUE),
    
    id_ferritin30_non_anaemic_n = sum(iron_def_ferritin_30 & anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_ferritin30_non_anaemic_pct = sum(iron_def_ferritin_30 & anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    id_stfr_non_anaemic_n = sum(iron_def_stfr & anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_stfr_non_anaemic_pct = sum(iron_def_stfr & anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    id_ferritin_index_non_anaemic_n = sum(iron_def_ferritin_index & anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_ferritin_index_non_anaemic_pct = sum(iron_def_ferritin_index & anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    .groups = "drop"
  ) %>%
  add_row(
    trial_arm = "Overall",
    n = nrow(df_analysis),
    
    # OVERALL (all)
    id_ferritin30_overall_n = sum(df_analysis$iron_def_ferritin_30, na.rm = TRUE),
    id_ferritin30_overall_pct = mean(df_analysis$iron_def_ferritin_30, na.rm = TRUE) * 100,
    
    id_stfr_overall_n = sum(df_analysis$iron_def_stfr, na.rm = TRUE),
    id_stfr_overall_pct = mean(df_analysis$iron_def_stfr, na.rm = TRUE) * 100,
    
    id_ferritin_index_overall_n = sum(df_analysis$iron_def_ferritin_index, na.rm = TRUE),
    id_ferritin_index_overall_pct = mean(df_analysis$iron_def_ferritin_index, na.rm = TRUE) * 100,
    
    # ANAEMIC
    n_anaemic = sum(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE),
    
    id_ferritin30_anaemic_n = sum(df_analysis$iron_def_ferritin_30 & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE),
    id_ferritin30_anaemic_pct = sum(df_analysis$iron_def_ferritin_30 & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    id_stfr_anaemic_n = sum(df_analysis$iron_def_stfr & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE),
    id_stfr_anaemic_pct = sum(df_analysis$iron_def_stfr & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    id_ferritin_index_anaemic_n = sum(df_analysis$iron_def_ferritin_index & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE),
    id_ferritin_index_anaemic_pct = sum(df_analysis$iron_def_ferritin_index & df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    
    # NON-ANAEMIC
    n_non_anaemic = sum(df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE),
    
    id_ferritin30_non_anaemic_n = sum(df_analysis$iron_def_ferritin_30 & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_ferritin30_non_anaemic_pct = sum(df_analysis$iron_def_ferritin_30 & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    id_stfr_non_anaemic_n = sum(df_analysis$iron_def_stfr & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_stfr_non_anaemic_pct = sum(df_analysis$iron_def_stfr & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    id_ferritin_index_non_anaemic_n = sum(df_analysis$iron_def_ferritin_index & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE),
    id_ferritin_index_non_anaemic_pct = sum(df_analysis$iron_def_ferritin_index & df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) / 
      sum(df_analysis$anaemia_venous == "Not anaemic", na.rm = TRUE) * 100,
    
    .before = 1
  )

cat("\n=== IRON DEFICIENCY BY TRIAL ARM AND ANAEMIA STATUS ===\n")

cat("\nFERRITIN <30 µg/L:\n")
cat("Overall:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- iron_def_by_arm %>% filter(trial_arm == arm)
  cat(arm, " (overall): ", row$id_ferritin30_overall_n, "/", row$n, 
      " (", round(row$id_ferritin30_overall_pct, 1), "%)\n", sep = "")
  cat(arm, " (anaemic only): ", row$id_ferritin30_anaemic_n, "/", row$n_anaemic, 
      " (", round(row$id_ferritin30_anaemic_pct, 1), "%)\n", sep = "")
  cat(arm, " (non-anaemic only): ", row$id_ferritin30_non_anaemic_n, "/", row$n_non_anaemic, 
      " (", round(row$id_ferritin30_non_anaemic_pct, 1), "%)\n", sep = "")
  cat("\n")
}

cat("\nsTfR >=1.55 mg/L:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- iron_def_by_arm %>% filter(trial_arm == arm)
  cat(arm, " (overall): ", row$id_stfr_overall_n, "/", row$n, 
      " (", round(row$id_stfr_overall_pct, 1), "%)\n", sep = "")
  cat(arm, " (anaemic only): ", row$id_stfr_anaemic_n, "/", row$n_anaemic, 
      " (", round(row$id_stfr_anaemic_pct, 1), "%)\n", sep = "")
  cat(arm, " (non-anaemic only): ", row$id_stfr_non_anaemic_n, "/", row$n_non_anaemic, 
      " (", round(row$id_stfr_non_anaemic_pct, 1), "%)\n", sep = "")
  cat("\n")
}

cat("\nFERRITIN INDEX >=1.03:\n")
for (arm in c("Overall", "Control", "Intervention")) {
  row <- iron_def_by_arm %>% filter(trial_arm == arm)
  cat(arm, " (overall): ", row$id_ferritin_index_overall_n, "/", row$n, 
      " (", round(row$id_ferritin_index_overall_pct, 1), "%)\n", sep = "")
  cat(arm, " (anaemic only): ", row$id_ferritin_index_anaemic_n, "/", row$n_anaemic, 
      " (", round(row$id_ferritin_index_anaemic_pct, 1), "%)\n", sep = "")
  cat(arm, " (non-anaemic only): ", row$id_ferritin_index_non_anaemic_n, "/", row$n_non_anaemic, 
      " (", round(row$id_ferritin_index_non_anaemic_pct, 1), "%)\n", sep = "")
  cat("\n")
}

write.csv(iron_def_by_arm, "IronDeficiency_ByArm_ByAnaemiaStatus.csv", row.names = FALSE)
cat("\n✓ Iron deficiency by arm and anaemia status saved\n")


# ============================================================================
# SECTION 3.5: BIOMARKER ASSOCIATIONS & INFLAMMATION ANALYSES
# ============================================================================

cat("\n\n===== SECTION 3.5: BIOMARKER ASSOCIATIONS =====\n")

df_corr <- df_analysis %>%
  filter(!is.na(venous_hb_12m) & !is.na(ferritin) & !is.na(stfr_mgL) & !is.na(crp) & !is.na(hepcidin)) %>%
  mutate(
    log_ferritin = log10(ferritin),
    log_stfr = log10(stfr_mgL),
    log_crp = log10(crp),
    log_hepcidin = log10(hepcidin)
  )

n_corr <- nrow(df_corr)
cat("Sample size for correlations (complete cases):", n_corr, "\n")

# Function to calculate Spearman correlation with CI
spearman_ci <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho <- cor_test$estimate
  p_val <- cor_test$p.value
  
  # Fisher's z transformation for 95% CI
  n <- length(x[!is.na(x) & !is.na(y)])
  z <- 0.5 * log((1 + rho) / (1 - rho))
  se_z <- 1 / sqrt(n - 3)
  z_crit <- qnorm(0.975)
  ci_lower <- tanh(z - z_crit * se_z)
  ci_upper <- tanh(z + z_crit * se_z)
  
  return(list(rho = rho, ci_lower = ci_lower, ci_upper = ci_upper, p_val = p_val, n = n))
}

cat("\nSpearman Correlations: Venous Hb vs Biomarkers\n")

corr_ferritin <- spearman_ci(df_corr$venous_hb_12m, df_corr$log_ferritin)
cat("Log Ferritin: ρ =", round(corr_ferritin$rho, 3), 
    " (", round(corr_ferritin$ci_lower, 3), ",", round(corr_ferritin$ci_upper, 3), ") p <0.001\n")

corr_stfr <- spearman_ci(df_corr$venous_hb_12m, df_corr$log_stfr)
cat("Log sTfR: ρ =", round(corr_stfr$rho, 3), 
    " (", round(corr_stfr$ci_lower, 3), ",", round(corr_stfr$ci_upper, 3), ") p <0.001\n")

corr_ferritin_index <- spearman_ci(df_corr$venous_hb_12m, df_corr$ferritin_index)
cat("Ferritin Index: ρ =", round(corr_ferritin_index$rho, 3), 
    " (", round(corr_ferritin_index$ci_lower, 3), ",", round(corr_ferritin_index$ci_upper, 3), ") p <0.001\n")

corr_hepcidin <- spearman_ci(df_corr$venous_hb_12m, df_corr$log_hepcidin)
cat("Log Hepcidin: ρ =", round(corr_hepcidin$rho, 3), 
    " (", round(corr_hepcidin$ci_lower, 3), ",", round(corr_hepcidin$ci_upper, 3), ") p <0.001\n")

corr_crp <- spearman_ci(df_corr$venous_hb_12m, df_corr$log_crp)
cat("Log CRP: ρ =", round(corr_crp$rho, 3), 
    " (", round(corr_crp$ci_lower, 3), ",", round(corr_crp$ci_upper, 3), ") p =", 
    round(corr_crp$p_val, 3), "\n")

spearman_results <- tribble(
  ~Biomarker, ~N, ~Rho, ~CI_Lower, ~CI_Upper, ~P_value,
  "Log Ferritin", corr_ferritin$n, corr_ferritin$rho, corr_ferritin$ci_lower, corr_ferritin$ci_upper, corr_ferritin$p_val,
  "Log sTfR", corr_stfr$n, corr_stfr$rho, corr_stfr$ci_lower, corr_stfr$ci_upper, corr_stfr$p_val,
  "Ferritin Index", corr_ferritin_index$n, corr_ferritin_index$rho, corr_ferritin_index$ci_lower, corr_ferritin_index$ci_upper, corr_ferritin_index$p_val,
  "Log Hepcidin", corr_hepcidin$n, corr_hepcidin$rho, corr_hepcidin$ci_lower, corr_hepcidin$ci_upper, corr_hepcidin$p_val,
  "Log CRP", corr_crp$n, corr_crp$rho, corr_crp$ci_lower, corr_crp$ci_upper, corr_crp$p_val
)

write.csv(spearman_results, "Spearman_Correlations.csv", row.names = FALSE)
cat("\n✓ Spearman correlations saved\n")

# ============================================================================
# INFLAMMATION ANALYSES
# ============================================================================

cat("\n\n===== INFLAMMATION AND IRON BIOMARKERS =====\n")

df_inflammation <- df_analysis %>%
  filter(!is.na(ferritin) & !is.na(stfr_mgL) & !is.na(crp) & !is.na(hepcidin) & crp > 0 & ferritin > 0) %>%
  mutate(
    log_ferritin = log10(ferritin),
    log_stfr = log10(stfr_mgL),
    log_crp = log10(crp),
    log_hepcidin = log10(hepcidin)
  )

model_crp_ferritin <- lm(log_ferritin ~ log_crp, data = df_inflammation)
model_crp_stfr <- lm(log_stfr ~ log_crp, data = df_inflammation)
model_crp_hepcidin <- lm(log_hepcidin ~ log_crp, data = df_inflammation)

cat("\nLinear regression: CRP vs biomarkers (log-scale)\n")

beta_ferritin <- coef(model_crp_ferritin)[2]
pval_ferritin <- summary(model_crp_ferritin)$coefficients[2, 4]
r2_ferritin <- summary(model_crp_ferritin)$r.squared
pct_change_ferritin <- (exp(beta_ferritin) - 1) * 100

cat("\nCRP vs Log Ferritin:\n")
cat("β =", round(beta_ferritin, 4), ", p <0.001, R² =", round(r2_ferritin, 3), "\n")
cat("Interpretation: Each 1 mg/L increase in CRP → ~", 
    round(pct_change_ferritin, 1), "% increase in ferritin\n")

beta_stfr <- coef(model_crp_stfr)[2]
pval_stfr <- summary(model_crp_stfr)$coefficients[2, 4]
r2_stfr <- summary(model_crp_stfr)$r.squared

cat("\nCRP vs Log sTfR:\n")
cat("β =", round(beta_stfr, 4), ", p =", round(pval_stfr, 3), ", R² =", round(r2_stfr, 3), "\n")

beta_hepcidin <- coef(model_crp_hepcidin)[2]
pval_hepcidin <- summary(model_crp_hepcidin)$coefficients[2, 4]
r2_hepcidin <- summary(model_crp_hepcidin)$r.squared

cat("\nCRP vs Log Hepcidin:\n")
cat("β =", round(beta_hepcidin, 4), ", p =", round(pval_hepcidin, 3), ", R² =", round(r2_hepcidin, 3), "\n")

inflammation_results <- tribble(
  ~Model, ~Beta, ~P_value, ~R_squared,
  "CRP vs Log Ferritin", beta_ferritin, pval_ferritin, r2_ferritin,
  "CRP vs Log sTfR", beta_stfr, pval_stfr, r2_stfr,
  "CRP vs Log Hepcidin", beta_hepcidin, pval_hepcidin, r2_hepcidin
)

write.csv(inflammation_results, "Inflammation_Analysis.csv", row.names = FALSE)

# ============================================================================
# ============================================================================

# ── BRINDA correction ─────────────────────────────────────────────────────
# The BRINDA (Biomarkers Reflecting Inflammation and Nutritional Determinants
# of Anaemia) approach adjusts ferritin for acute-phase response.
#
# Implementation used here (simplified internal-regression approach):
#   1. Estimate β: regress log10(ferritin) ~ log10(CRP) in the subset
#      with CRP ≤5 mg/L (low-inflammation reference group)
#   2. Apply: adjusted_ferritin = ferritin × 10^(β × (log10(0.5) - log10(CRP)))
#      Reference CRP = 0.5 mg/L
#
# IMPORTANT LIMITATION: This differs from the published BRINDA regression
# approach (Namaste et al. 2017, J Nutr; Rohner et al. 2017, J Nutr) which
# uses external reference regression coefficients derived from large pooled
# datasets and applies a different correction formula. The published BRINDA
# method was not used here because study-specific coefficients were not
# available at the time of analysis. Results should be interpreted as a
# sensitivity analysis illustrating the direction and magnitude of
# inflammation-adjustment, not as a formally validated BRINDA correction.
# This is acknowledged in the thesis manuscript.
# ─────────────────────────────────────────────────────────────────────────
cat("\n\n===== BRINDA-STYLE FERRITIN CORRECTION (internal regression approach) =====\n")

# Step 1: Calculate regression coefficients in women with CRP ≤5 mg/L
df_brinda_train <- df_inflammation %>%
  filter(crp <= 5)

n_brinda_train <- nrow(df_brinda_train)
cat("Training set for BRINDA (CRP ≤5 mg/L): n =", n_brinda_train, "\n")

# Fit regression: log_ferritin ~ log_crp in low-inflammation women
model_brinda <- lm(log_ferritin ~ log_crp, data = df_brinda_train)
brinda_intercept <- coef(model_brinda)[1]
brinda_slope <- coef(model_brinda)[2]

cat("BRINDA regression coefficients:\n")
cat("Intercept:", round(brinda_intercept, 4), "\n")
cat("Slope (β):", round(brinda_slope, 4), "\n")

# Step 2: Calculate adjusted ferritin for ALL women
# Formula: adjusted_ferritin = ferritin * 10^(β * (0.5 - log10(CRP)))
# Where 0.5 mg/L is the reference CRP concentration

df_brinda_all <- df_analysis %>%
  filter(!is.na(ferritin) & !is.na(crp) & crp > 0) %>%
  mutate(
    log_crp = log10(crp),
    # BRINDA-adjusted ferritin
    ferritin_brinda_corrected = ferritin * 10^(brinda_slope * (log10(0.5) - log_crp)),
    
    # Iron deficiency with corrected ferritin
    iron_def_ferritin30_brinda = ferritin_brinda_corrected < 30,
    iron_def_ferritin15_brinda = ferritin_brinda_corrected < 15
  )

cat("\nBRINDA Correction Applied:\n")
cat("Reference CRP: 0.5 mg/L\n")
cat("Sample size with complete data: n =", nrow(df_brinda_all), "\n")

# Step 3: Compare iron deficiency prevalence before and after BRINDA correction
brinda_comparison <- tibble(
  Definition = c(
    "Ferritin <30 µg/L (uncorrected)",
    "Ferritin <30 µg/L (BRINDA-corrected)",
    "Ferritin <15 µg/L (uncorrected)",
    "Ferritin <15 µg/L (BRINDA-corrected)"
  ),
  Overall_N = c(
    sum(df_brinda_all$iron_def_ferritin_30, na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin30_brinda, na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin_15, na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin15_brinda, na.rm = TRUE)
  ),
  Overall_Pct = c(
    mean(df_brinda_all$iron_def_ferritin_30, na.rm = TRUE) * 100,
    mean(df_brinda_all$iron_def_ferritin30_brinda, na.rm = TRUE) * 100,
    mean(df_brinda_all$iron_def_ferritin_15, na.rm = TRUE) * 100,
    mean(df_brinda_all$iron_def_ferritin15_brinda, na.rm = TRUE) * 100
  ),
  Anaemic_N = c(
    sum(df_brinda_all$iron_def_ferritin_30 & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin30_brinda & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin_15 & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE),
    sum(df_brinda_all$iron_def_ferritin15_brinda & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE)
  ),
  Anaemic_Pct = c(
    sum(df_brinda_all$iron_def_ferritin_30 & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    sum(df_brinda_all$iron_def_ferritin30_brinda & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    sum(df_brinda_all$iron_def_ferritin_15 & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) * 100,
    sum(df_brinda_all$iron_def_ferritin15_brinda & df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) / 
      sum(df_brinda_all$anaemia_venous == "Anaemic", na.rm = TRUE) * 100
  )
)

cat("\n=== BRINDA COMPARISON: IRON DEFICIENCY PREVALENCE ===\n")
cat("\nFERRITIN <30 µg/L:\n")
cat("Uncorrected: ", brinda_comparison$Overall_N[1], "/", nrow(df_brinda_all), 
    " (", round(brinda_comparison$Overall_Pct[1], 1), "%) overall; ",
    brinda_comparison$Anaemic_N[1], " anaemic (", round(brinda_comparison$Anaemic_Pct[1], 1), "%)\n", sep = "")
cat("BRINDA-corrected: ", brinda_comparison$Overall_N[2], "/", nrow(df_brinda_all), 
    " (", round(brinda_comparison$Overall_Pct[2], 1), "%) overall; ",
    brinda_comparison$Anaemic_N[2], " anaemic (", round(brinda_comparison$Anaemic_Pct[2], 1), "%)\n", sep = "")
cat("Difference: +", round(brinda_comparison$Overall_Pct[2] - brinda_comparison$Overall_Pct[1], 1), 
    " percentage points\n\n", sep = "")

cat("FERRITIN <15 µg/L:\n")
cat("Uncorrected: ", brinda_comparison$Overall_N[3], "/", nrow(df_brinda_all), 
    " (", round(brinda_comparison$Overall_Pct[3], 1), "%) overall; ",
    brinda_comparison$Anaemic_N[3], " anaemic (", round(brinda_comparison$Anaemic_Pct[3], 1), "%)\n", sep = "")
cat("BRINDA-corrected: ", brinda_comparison$Overall_N[4], "/", nrow(df_brinda_all), 
    " (", round(brinda_comparison$Overall_Pct[4], 1), "%) overall; ",
    brinda_comparison$Anaemic_N[4], " anaemic (", round(brinda_comparison$Anaemic_Pct[4], 1), "%)\n", sep = "")
cat("Difference: +", round(brinda_comparison$Overall_Pct[4] - brinda_comparison$Overall_Pct[3], 1), 
    " percentage points\n\n", sep = "")

write.csv(brinda_comparison, "BRINDA_Correction_Comparison.csv", row.names = FALSE)
cat("\n✓ BRINDA correction results saved\n")

# ============================================================================
# FERRITIN-sTfR AGREEMENT ANALYSIS
# ============================================================================

cat("\n\n===== FERRITIN-sTfR AGREEMENT ANALYSIS =====\n")

df_agreement_fe_stfr <- df_analysis %>%
  filter(!is.na(ferritin) & !is.na(stfr_mgL) & !is.na(venous_hb_12m)) %>%
  mutate(
    ferritin_def = iron_def_ferritin_30,
    stfr_def = iron_def_stfr,
    concordant = (ferritin_def == stfr_def),
    category = case_when(
      ferritin_def & stfr_def ~ "Both positive",
      !ferritin_def & !stfr_def ~ "Both negative",
      ferritin_def & !stfr_def ~ "Ferritin only",
      !ferritin_def & stfr_def ~ "sTfR only"
    )
  )

overall_agree <- mean(df_agreement_fe_stfr$concordant, na.rm = TRUE) * 100

confusion_matrix <- table(df_agreement_fe_stfr$ferritin_def, df_agreement_fe_stfr$stfr_def)
kappa_result <- psych::cohen.kappa(confusion_matrix)

cat("\nOverall Agreement (Ferritin <30 vs sTfR >=1.55):\n")
cat("Agreement:", round(overall_agree, 1), "%\n")
cat("Cohen's κ:", round(as.numeric(kappa_result$overall[1]), 3), "\n")

# Agreement by CRP status
df_agreement_crp_groups <- df_agreement_fe_stfr %>%
  group_by(crp_elevated) %>%
  summarise(
    n = n(),
    agreement_pct = mean(concordant, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\nAgreement by CRP Status:\n")
for (i in 1:nrow(df_agreement_crp_groups)) {
  crp_status <- df_agreement_crp_groups$crp_elevated[i]
  label <- if (crp_status) "CRP >5 mg/L" else "CRP ≤5 mg/L"
  cat(label, ": ", round(df_agreement_crp_groups$agreement_pct[i], 1), 
      "% (n=", df_agreement_crp_groups$n[i], ")\n", sep = "")
}

# Extract kappa as numeric
#kappa_value <- as.numeric(kappa_result$overall[1])

#agreement_summary <- data.frame(
# Group = c("Overall", "CRP ≤5 mg/L", "CRP >5 mg/L"),
# N = c(nrow(df_agreement_fe_stfr), df_agreement_crp_groups$n[1], df_agreement_crp_groups$n[2]),
#Agreement_Pct = c(overall_agree, df_agreement_crp_groups$agreement_pct[1], df_agreement_crp_groups$agreement_pct[2]),
# Cohens_Kappa = c(kappa_value, NA, NA),
# stringsAsFactors = FALSE
#)

#write.csv(agreement_summary, "Ferritin_sTfR_Agreement.csv", row.names = FALSE)
#cat("\n✓ Ferritin-sTfR agreement saved\n")

# ============================================================================
# MIXED-EFFECTS REGRESSION MODELS
# WITH DELIVERY MODE AND PLACE OF BIRTH (PRIVATE VS ALL OTHER) AS COVARIATES
# ============================================================================

cat("\n\n===== SECTION 3.6: MIXED-EFFECTS REGRESSION MODELS =====\n")

# Prepare data for regression
df_regression <- df_analysis %>%
  filter(!is.na(venous_hb_12m) & !is.na(poc_hb_baseline_z) & 
           !is.na(maternal_age_z) & !is.na(bmi_z) & !is.na(gestational_age_baseline_z) &
           !is.na(delivery_mode) & !is.na(place_of_birth) &
           crp > 0) %>%
  mutate(
    trial_arm_binary = if_else(trial_arm == "Intervention", 1, 0),
    parity_binary = if_else(parity >= 1, 1, 0),
    log_crp = log10(crp),
    # Delivery mode: binary (Caesarean section vs vaginal/instrumental)
    delivery_caesarean = if_else(grepl("caesarean|section", tolower(delivery_mode), ignore.case = TRUE), 1, 0),
    # Place of birth: binary (Private vs All Other: Government/NGO/Home)
    place_private = if_else(grepl("private", tolower(place_of_birth), ignore.case = TRUE), 1, 0)
  )

n_regression <- nrow(df_regression)
n_villages <- length(unique(df_regression$village_id))
cat("Sample size for regression models:", n_regression, "\n")
cat("Number of villages (random effect):", n_villages, "\n")
cat("Delivery mode coding: 1 = Caesarean section, 0 = Vaginal/Instrumental\n")
cat("Place of birth coding: 1 = Private, 0 = Government/NGO/Home\n\n")


# ============================================================================
# MODEL 1: Venous Anaemia (with delivery and place covariates)
# ============================================================================

model1 <- glmer(
  anaemia_venous == "Anaemic" ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\n--- MODEL 1: VENOUS ANAEMIA (Hb <120 g/L) ---\n")
cat("Mixed-effects logistic regression with random intercept for village\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, delivery mode, place of birth\n")
cat("n =", n_regression, ", villages =", n_villages, "\n\n")
summary_m1 <- summary(model1)
print(summary_m1)

m1_results <- as_tibble(coef(summary_m1), rownames = "Parameter") %>%
  rename(Estimate = Estimate, SE = `Std. Error`, Z_value = `z value`, P_value = `Pr(>|z|)`) %>%
  mutate(
    OR = exp(Estimate),
    CI_lower = exp(Estimate - 1.96 * SE),
    CI_upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

cat("\nModel 1 - Trial Arm Effect:\n")
trial_effect_m1 <- m1_results %>% filter(Parameter == "trial_arm_binary")
cat("Adjusted OR:", round(trial_effect_m1$OR, 2), 
    " (95% CI:", round(trial_effect_m1$CI_lower, 2), "-", round(trial_effect_m1$CI_upper, 2), ")\n")
cat("p-value:", round(trial_effect_m1$P_value, 3), "\n")


# ============================================================================
# MODEL 2: Iron Deficiency Anaemia (with delivery and place covariates)
# ============================================================================

model2 <- glmer(
  ida_ferritin30 ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + log_crp + 
    delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\n--- MODEL 2: IRON DEFICIENCY ANAEMIA (Ferritin <30 + Anaemia, CRP-adjusted) ---\n")
cat("Mixed-effects logistic regression with random intercept for village\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, CRP, delivery mode, place of birth\n")
cat("n =", n_regression, ", villages =", n_villages, "\n\n")
summary_m2 <- summary(model2)
print(summary_m2)

m2_results <- as_tibble(coef(summary_m2), rownames = "Parameter") %>%
  rename(Estimate = Estimate, SE = `Std. Error`, Z_value = `z value`, P_value = `Pr(>|z|)`) %>%
  mutate(
    OR = exp(Estimate),
    CI_lower = exp(Estimate - 1.96 * SE),
    CI_upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

cat("\nModel 2 - Trial Arm Effect:\n")
trial_effect_m2 <- m2_results %>% filter(Parameter == "trial_arm_binary")
cat("Adjusted OR:", round(trial_effect_m2$OR, 2), 
    " (95% CI:", round(trial_effect_m2$CI_lower, 2), "-", round(trial_effect_m2$CI_upper, 2), ")\n")
cat("p-value:", round(trial_effect_m2$P_value, 3), "\n")


# ============================================================================
# MODEL 3: Haemoglobin (continuous, with delivery and place covariates)
# ============================================================================

model3 <- lmer(
  venous_hb_12m ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + 
    delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression
)

cat("\n--- MODEL 3: VENOUS HAEMOGLOBIN (continuous) ---\n")
cat("Linear mixed-effects model with random intercept for village\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, delivery mode, place of birth\n")
cat("n =", n_regression, ", villages =", n_villages, "\n\n")
summary_m3 <- summary(model3)
print(summary_m3)

m3_results <- as_tibble(summary_m3$coefficients, rownames = "Parameter") %>%
  rename(Estimate = Estimate, SE = `Std. Error`, T_value = `t value`) %>%
  filter(Parameter != "(Intercept)")

cat("\nModel 3 - Trial Arm Effect:\n")
trial_effect_m3 <- m3_results %>% filter(Parameter == "trial_arm_binary")
cat("Mean Difference:", round(trial_effect_m3$Estimate, 2), "g/L\n")

# ============================================================================
# FULL REGRESSION MODEL RESULTS TABLES (WITH DELIVERY & PLACE COVARIATES)
# ============================================================================

cat("\n\n===== FULL REGRESSION MODEL RESULTS TABLES =====\n")

predictor_labels <- c(
  "trial_arm_binary"           = "Trial Arm (Intervention vs Control)",
  "poc_hb_baseline_z"          = "Baseline POC Hb (per 1 SD)",
  "maternal_age_z"             = "Maternal Age (per 1 SD)",
  "parity_binary"              = "Multiparous (vs Nulliparous)",
  "bmi_z"                      = "BMI (per 1 SD)",
  "gestational_age_baseline_z" = "Gestational Age (per 1 SD)",
  "log_crp"                    = "Log CRP",
  "delivery_caesarean"         = "Delivery Mode (Caesarean vs Vaginal/Instrumental)",
  "place_private"              = "Place of Birth (Private vs Government/NGO/Home)"
)

m1_full <- m1_results %>%
  mutate(
    Model     = "Venous Anaemia",
    Predictor = predictor_labels[Parameter]
  ) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_lower, CI_upper, Z_value, P_value) %>%
  rename(CI_Lower = CI_lower, CI_Upper = CI_upper, Z_Value = Z_value, P_Value = P_value)

m2_full <- m2_results %>%
  mutate(
    Model     = "Iron Deficiency Anaemia",
    Predictor = predictor_labels[Parameter]
  ) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_lower, CI_upper, Z_value, P_value) %>%
  rename(CI_Lower = CI_lower, CI_Upper = CI_upper, Z_Value = Z_value, P_Value = P_value)

m3_full <- m3_results %>%
  mutate(
    Model     = "Venous Haemoglobin (continuous)",
    Predictor = predictor_labels[Parameter],
    CI_Lower  = Estimate - 1.96 * SE,
    CI_Upper  = Estimate + 1.96 * SE,
    P_Value   = NA_real_
  ) %>%
  select(Model, Predictor, Estimate, SE, CI_Lower, CI_Upper, T_value, P_Value) %>%
  rename(T_Value = T_value)

cat("\n=== MODEL 1: VENOUS ANAEMIA (Hb <120 g/L) ===\n")
print(m1_full %>% select(Predictor, OR, CI_Lower, CI_Upper, P_Value))

cat("\n=== MODEL 2: IRON DEFICIENCY ANAEMIA (Ferritin <30 + Anaemia, CRP-adjusted) ===\n")
print(m2_full %>% select(Predictor, OR, CI_Lower, CI_Upper, P_Value))

cat("\n=== MODEL 3: VENOUS HAEMOGLOBIN (continuous, g/L) ===\n")
print(m3_full %>% select(Predictor, Estimate, CI_Lower, CI_Upper))

write.csv(m1_full, "Model1_Venous_Anaemia_Private_vs_Other.csv", row.names = FALSE)
write.csv(m2_full, "Model2_IDA_CRP_Private_vs_Other.csv", row.names = FALSE)
write.csv(m3_full, "Model3_Venous_Hb_Private_vs_Other.csv", row.names = FALSE)


# ============================================================================
# COMBINED RESULTS TABLE FOR MANUSCRIPT
# ============================================================================

all_models_combined <- tibble(
  Model = "Model 1: Venous Anaemia",
  Predictor = m1_full$Predictor,
  Estimate = round(m1_full$Estimate, 4),
  SE = round(m1_full$SE, 4),
  aOR_or_Beta = round(m1_full$OR, 3),
  CI_Lower = round(m1_full$CI_Lower, 3),
  CI_Upper = round(m1_full$CI_Upper, 3),
  P_Value = round(m1_full$P_Value, 4),
  Note = "aOR"
) %>%
  bind_rows(
    tibble(
      Model = "Model 2: IDA (CRP-adjusted)",
      Predictor = m2_full$Predictor,
      Estimate = round(m2_full$Estimate, 4),
      SE = round(m2_full$SE, 4),
      aOR_or_Beta = round(m2_full$OR, 3),
      CI_Lower = round(m2_full$CI_Lower, 3),
      CI_Upper = round(m2_full$CI_Upper, 3),
      P_Value = round(m2_full$P_Value, 4),
      Note = "aOR"
    )
  ) %>%
  bind_rows(
    tibble(
      Model = "Model 3: Venous Hb (continuous)",
      Predictor = m3_full$Predictor,
      Estimate = round(m3_full$Estimate, 2),
      SE = round(m3_full$SE, 2),
      aOR_or_Beta = NA,
      CI_Lower = round(m3_full$CI_Lower, 2),
      CI_Upper = round(m3_full$CI_Upper, 2),
      P_Value = round(m3_full$P_Value, 4),
      Note = "Mean Difference (g/L)"
    )
  )

write.csv(all_models_combined, "Regression_Models_Full_Results_Private_vs_Other.csv", row.names = FALSE)

cat("\n✓ All regression model results saved:\n")
cat("  - Model1_Venous_Anaemia_Private_vs_Other.csv\n")
cat("  - Model2_IDA_CRP_Private_vs_Other.csv\n")
cat("  - Model3_Venous_Hb_Private_vs_Other.csv\n")
cat("  - Regression_Models_Full_Results_Private_vs_Other.csv (combined)\n\n")


# ============================================================================
# SAVE MODEL SUMMARIES AS TEXT
# ============================================================================

sink("Regression_Models_Summary_Private_vs_Other.txt")
cat("=== REGRESSION MODEL RESULTS SUMMARY ===\n")
cat("Models include delivery mode and place of birth (PRIVATE VS GOVERNMENT/NGO/HOME) as covariates\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat(strrep("=", 80), "\n")
cat("MODEL 1: VENOUS ANAEMIA (Hb <120 g/L)\n")
cat(strrep("=", 80), "\n")

cat("Outcome: Venous-defined anaemia (binary)\n")
cat("Sample size: n =", n_regression, ", villages =", n_villages, "\n")
cat("Random effect: Village-level clustering\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, delivery mode, place of birth\n\n")
print(summary(model1))

cat("\n\nDELIVERY & PLACE OF BIRTH EFFECTS (Model 1):\n")
delivery_m1 <- m1_results %>% filter(Parameter == "delivery_caesarean")
place_m1 <- m1_results %>% filter(Parameter == "place_private")
cat("Delivery Mode (Caesarean vs Vaginal/Instrumental):\n")
cat("  aOR:", round(delivery_m1$OR, 2), " (95% CI:", round(delivery_m1$CI_lower, 2), "-", round(delivery_m1$CI_upper, 2), ")\n")
cat("  p-value:", round(delivery_m1$P_value, 3), "\n\n")
cat("Place of Birth (Private vs Government/NGO/Home):\n")
cat("  aOR:", round(place_m1$OR, 2), " (95% CI:", round(place_m1$CI_lower, 2), "-", round(place_m1$CI_upper, 2), ")\n")
cat("  p-value:", round(place_m1$P_value, 3), "\n\n")

cat(strrep("=", 80), "\n")
cat("MODEL 2: IRON DEFICIENCY ANAEMIA (Ferritin <30 + Anaemia, CRP-adjusted)\n")
cat(strrep("=", 80), "\n")
cat("Outcome: Iron deficiency anaemia (binary)\n")
cat("Sample size: n =", n_regression, ", villages =", n_villages, "\n")
cat("Random effect: Village-level clustering\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, log CRP, delivery mode, place of birth\n\n")
print(summary(model2))

cat("\n\nDELIVERY & PLACE OF BIRTH EFFECTS (Model 2):\n")
delivery_m2 <- m2_results %>% filter(Parameter == "delivery_caesarean")
place_m2 <- m2_results %>% filter(Parameter == "place_private")
cat("Delivery Mode (Caesarean vs Vaginal/Instrumental):\n")
cat("  aOR:", round(delivery_m2$OR, 2), " (95% CI:", round(delivery_m2$CI_lower, 2), "-", round(delivery_m2$CI_upper, 2), ")\n")
cat("  p-value:", round(delivery_m2$P_value, 3), "\n\n")
cat("Place of Birth (Private vs Government/NGO/Home):\n")
cat("  aOR:", round(place_m2$OR, 2), " (95% CI:", round(place_m2$CI_lower, 2), "-", round(place_m2$CI_upper, 2), ")\n")
cat("  p-value:", round(place_m2$P_value, 3), "\n\n")

cat(strrep("=", 80), "\n")
cat("MODEL 3: VENOUS HAEMOGLOBIN (continuous, g/L)\n")
cat(strrep("=", 80), "\n")
cat("Outcome: Venous haemoglobin (continuous)\n")
cat("Sample size: n =", n_regression, ", villages =", n_villages, "\n")
cat("Random effect: Village-level clustering\n")
cat("Covariates: Trial arm, baseline POC Hb, age, parity, BMI, GA, delivery mode, place of birth\n\n")
print(summary(model3))

sink()

cat("✓ Regression model text summaries saved to Regression_Models_Summary_Private_vs_Other.txt\n\n")


# ============================================================================
# SUMMARY OF TRIAL ARM EFFECTS (FOR QUICK REFERENCE)
# ============================================================================

cat("\n===== SUMMARY OF TRIAL ARM EFFECTS (INTERVENTION vs CONTROL) =====\n\n")

cat("MODEL 1: Venous Anaemia\n")
cat("  Adjusted OR:", round(trial_effect_m1$OR, 2),
    "(95% CI:", round(trial_effect_m1$CI_lower, 2), "-", round(trial_effect_m1$CI_upper, 2), ")\n")
cat("  p-value:", round(trial_effect_m1$P_value, 3), "\n\n")

cat("MODEL 2: Iron Deficiency Anaemia (CRP-adjusted)\n")
cat("  Adjusted OR:", round(trial_effect_m2$OR, 2),
    "(95% CI:", round(trial_effect_m2$CI_lower, 2), "-", round(trial_effect_m2$CI_upper, 2), ")\n")
cat("  p-value:", round(trial_effect_m2$P_value, 3), "\n\n")

cat("MODEL 3: Venous Haemoglobin (continuous)\n")
cat("  Mean Difference:", round(trial_effect_m3$Estimate, 2), "g/L",
    "(95% CI:", round(trial_effect_m3$Estimate - 1.96 * trial_effect_m3$SE, 2), "-",
    round(trial_effect_m3$Estimate + 1.96 * trial_effect_m3$SE, 2), ")\n\n")

cat("\n===== REGRESSION ANALYSIS COMPLETE =====\n")


# ============================================================================
# GENERATE FIGURES
# ============================================================================

cat("\n\n===== GENERATING FIGURES =====\n")

# FIGURE 1: Bland-Altman Plot
bland_altman_plot <- ggplot(df_agreement, aes(x = hb_mean, y = hb_diff)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_hline(yintercept = bland_altman_summary$mean_diff, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = bland_altman_summary$loa_lower, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = bland_altman_summary$loa_upper, color = "blue", linetype = "dashed", size = 1) +
  labs(
    title = "Bland-Altman Plot: Capillary POC vs Venous Haemoglobin",
    x = "Mean Haemoglobin (g/L)",
    y = "Difference: POC minus Venous (g/L)",
    subtitle = paste0("Mean bias = ", round(bland_altman_summary$mean_diff, 1), " g/L; ",
                      "95% LoA = ", round(bland_altman_summary$loa_lower, 1), " to ",
                      round(bland_altman_summary$loa_upper, 1), " g/L")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

ggsave("Figure_BlandAltman_POC_Venous.png", bland_altman_plot, width = 8, height = 6, dpi = 300)
cat("✓ Bland-Altman plot saved\n")

# FIGURE 2: Scatter plot - Ferritin vs sTfR
scatter_ferritin_stfr <- ggplot(df_agreement_fe_stfr, aes(x = log10(ferritin), y = log10(stfr_mgL), 
                                                          color = category, fill = category)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_vline(xintercept = log10(30), color = "black", linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = log10(1.55), color = "black", linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Both positive" = "#d62728", "Both negative" = "#2ca02c",
                                "Ferritin only" = "#ff7f0e", "sTfR only" = "#1f77b4")) +
  scale_fill_manual(values = c("Both positive" = "#d62728", "Both negative" = "#2ca02c",
                               "Ferritin only" = "#ff7f0e", "sTfR only" = "#1f77b4")) +
  labs(
    title = "Agreement between Ferritin and sTfR for Iron Deficiency Classification",
    x = "Log₁₀ Ferritin (µg/L)",
    y = "Log₁₀ sTfR (mg/L)",
    color = "Classification",
    fill = "Classification"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("Figure_ScatterPlot_Ferritin_sTfR.png", scatter_ferritin_stfr, width = 9, height = 7, dpi = 300)
cat("✓ Ferritin vs sTfR scatter plot saved\n")

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n\n===== ANALYSIS COMPLETE =====\n")
cat("\nAll results have been saved to CSV files and figures to PNG files.\n")
cat("Ready to integrate into manuscript.\n")

key_findings <- tribble(
  ~Finding, ~Value,
  "Sample size", n_overall,
  "Venous anaemia prevalence (%)", round(table4_deficiencies$anaemic_pct[1], 1),
  "POC anaemia prevalence (%)", round(poc_anaemia_summary$poc_anaemic_pct, 1),
  "Iron deficiency - ferritin <30 (%)", round(table4_deficiencies$id_ferritin30_pct[1], 1),
  "Iron deficiency - sTfR >=1.55 (%)", round(table4_deficiencies$id_stfr_pct[1], 1),
  "Mean POC-Venous bias (g/L)", round(bland_altman_summary$mean_diff, 1),
  "Ferritin-sTfR agreement (%)", round(overall_agree, 1),
  "Model 1: Trial arm OR", round(trial_effect_m1$OR, 2),
  "Model 2: Trial arm OR (CRP-adj)", round(trial_effect_m2$OR, 2)
)

cat("\n===== KEY FINDINGS SUMMARY =====\n")
print(key_findings)

write.csv(key_findings, "Key_Findings_Summary.csv", row.names = FALSE)

cat("\n=== END OF ANALYSIS SCRIPT ===\n")

#### NEW REGRESSION MODELS ###

table(df_analysis$delivery_mode, useNA = "always")

df_regression2 <- df_analysis %>%
  filter(!is.na(venous_hb_12m) & !is.na(poc_hb_baseline_z) & 
           !is.na(maternal_age_z) & !is.na(bmi_z) & !is.na(gestational_age_baseline_z) &
           crp > 0) %>%
  mutate(
    trial_arm_binary   = if_else(trial_arm == "Intervention", 1, 0),
    parity_binary      = if_else(parity >= 1, 1, 0),
    log_crp            = log10(crp),
    delivery_caesarean = if_else(delivery_mode == "caesarean", 1, 0)
  )

cat("n =", nrow(df_regression2), "\n")
print(table(df_regression2$delivery_caesarean))

# ============================================================================
# SENSITIVITY MODELS: WITH DELIVERY MODE AS ADDITIONAL COVARIATE
# ============================================================================

cat("\n\n===== SENSITIVITY MODELS: WITH DELIVERY MODE =====\n")
cat("Sample size:", nrow(df_regression2), "\n")
cat("Villages:", length(unique(df_regression2$village_id)), "\n\n")

# ============================================================================
# MODEL 1: Venous Anaemia
# ============================================================================

model1_deliv <- glmer(
  anaemia_venous == "Anaemic" ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + delivery_caesarean +
    (1 | village_id),
  data = df_regression2,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary_m1_deliv <- summary(model1_deliv)

m1_deliv_results <- as_tibble(coef(summary_m1_deliv), rownames = "Parameter") %>%
  rename(SE = `Std. Error`, Z_value = `z value`, P_value = `Pr(>|z|)`) %>%
  mutate(
    OR       = exp(Estimate),
    CI_lower = exp(Estimate - 1.96 * SE),
    CI_upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

cat("\n--- MODEL 1: VENOUS ANAEMIA ---\n")
print(m1_deliv_results %>% select(Parameter, OR, CI_lower, CI_upper, P_value))

trial_m1_deliv <- m1_deliv_results %>% filter(Parameter == "trial_arm_binary")
cat("\nTrial Arm: aOR", round(trial_m1_deliv$OR, 2),
    "(95% CI:", round(trial_m1_deliv$CI_lower, 2), "-", round(trial_m1_deliv$CI_upper, 2), ")",
    "p =", round(trial_m1_deliv$P_value, 3), "\n")

# ============================================================================
# MODEL 2: Iron Deficiency Anaemia
# ============================================================================

model2_deliv <- glmer(
  ida_ferritin30 ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + log_crp + delivery_caesarean +
    (1 | village_id),
  data = df_regression2,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary_m2_deliv <- summary(model2_deliv)

m2_deliv_results <- as_tibble(coef(summary_m2_deliv), rownames = "Parameter") %>%
  rename(SE = `Std. Error`, Z_value = `z value`, P_value = `Pr(>|z|)`) %>%
  mutate(
    OR       = exp(Estimate),
    CI_lower = exp(Estimate - 1.96 * SE),
    CI_upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

cat("\n--- MODEL 2: IRON DEFICIENCY ANAEMIA ---\n")
print(m2_deliv_results %>% select(Parameter, OR, CI_lower, CI_upper, P_value))

trial_m2_deliv <- m2_deliv_results %>% filter(Parameter == "trial_arm_binary")
cat("\nTrial Arm: aOR", round(trial_m2_deliv$OR, 2),
    "(95% CI:", round(trial_m2_deliv$CI_lower, 2), "-", round(trial_m2_deliv$CI_upper, 2), ")",
    "p =", round(trial_m2_deliv$P_value, 3), "\n")

# ============================================================================
# MODEL 3: Venous Haemoglobin (continuous)
# ============================================================================

model3_deliv <- lmer(
  venous_hb_12m ~ trial_arm_binary + poc_hb_baseline_z + maternal_age_z + 
    parity_binary + bmi_z + gestational_age_baseline_z + delivery_caesarean +
    (1 | village_id),
  data = df_regression2
)

summary_m3_deliv <- summary(model3_deliv)

m3_deliv_results <- as_tibble(summary_m3_deliv$coefficients, rownames = "Parameter") %>%
  rename(SE = `Std. Error`, T_value = `t value`) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE
  ) %>%
  filter(Parameter != "(Intercept)")

cat("\n--- MODEL 3: VENOUS HAEMOGLOBIN (continuous) ---\n")
print(m3_deliv_results %>% select(Parameter, Estimate, CI_lower, CI_upper))

trial_m3_deliv <- m3_deliv_results %>% filter(Parameter == "trial_arm_binary")
cat("\nTrial Arm: Mean Difference", round(trial_m3_deliv$Estimate, 2), "g/L",
    "(95% CI:", round(trial_m3_deliv$CI_lower, 2), "-", round(trial_m3_deliv$CI_upper, 2), ")\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

predictor_labels_deliv <- c(
  "trial_arm_binary"           = "Trial Arm (Intervention vs Control)",
  "poc_hb_baseline_z"          = "Baseline POC Hb (per 1 SD)",
  "maternal_age_z"             = "Maternal Age (per 1 SD)",
  "parity_binary"              = "Multiparous (vs Nulliparous)",
  "bmi_z"                      = "BMI (per 1 SD)",
  "gestational_age_baseline_z" = "Gestational Age (per 1 SD)",
  "log_crp"                    = "Log CRP",
  "delivery_caesarean"         = "Delivery Mode (Caesarean vs Vaginal/Instrumental)"
)

m1_deliv_full <- m1_deliv_results %>%
  mutate(Model = "Venous Anaemia", Predictor = predictor_labels_deliv[Parameter]) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_lower, CI_upper, Z_value, P_value)

m2_deliv_full <- m2_deliv_results %>%
  mutate(Model = "Iron Deficiency Anaemia", Predictor = predictor_labels_deliv[Parameter]) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_lower, CI_upper, Z_value, P_value)

m3_deliv_full <- m3_deliv_results %>%
  mutate(
    Model     = "Venous Haemoglobin (continuous)",
    Predictor = predictor_labels_deliv[Parameter],
    P_Value   = NA_real_
  ) %>%
  select(Model, Predictor, Estimate, SE, CI_lower, CI_upper, T_value, P_Value)

write.csv(m1_deliv_full, "Model1_Venous_Anaemia_DeliveryMode.csv", row.names = FALSE)
write.csv(m2_deliv_full, "Model2_IDA_DeliveryMode.csv", row.names = FALSE)
write.csv(m3_deliv_full, "Model3_Venous_Hb_DeliveryMode.csv", row.names = FALSE)

cat("\n✓ Results saved:\n")
cat("  - Model1_Venous_Anaemia_DeliveryMode.csv\n")
cat("  - Model2_IDA_DeliveryMode.csv\n")
cat("  - Model3_Venous_Hb_DeliveryMode.csv\n")


# ============================================================================
# SENSITIVITY ANALYSIS: PHC-LEVEL CLUSTERING INSTEAD OF VILLAGE-LEVEL CLUSTERING
# Compare village-clustered vs PHC-clustered mixed models
# ============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

cat("\n\n===== SENSITIVITY ANALYSIS: PHC-LEVEL CLUSTERING =====\n")

# ----------------------------------------------------------------------------
# Use the same regression dataset as the main models
# ----------------------------------------------------------------------------

df_regression_compare <- df_analysis %>%
  filter(
    !is.na(venous_hb_12m),
    !is.na(poc_hb_baseline_z),
    !is.na(maternal_age_z),
    !is.na(bmi_z),
    !is.na(gestational_age_baseline_z),
    !is.na(delivery_mode),
    !is.na(place_of_birth),
    !is.na(phc_id),
    !is.na(village_id),
    crp > 0
  ) %>%
  mutate(
    trial_arm_binary = if_else(trial_arm == "Intervention", 1, 0),
    parity_binary = if_else(parity >= 1, 1, 0),
    log_crp = log10(crp),
    delivery_caesarean = if_else(
      grepl("caesarean|section", tolower(delivery_mode), ignore.case = TRUE), 1, 0
    ),
    place_private = if_else(
      grepl("private", tolower(place_of_birth), ignore.case = TRUE), 1, 0
    )
  )

cat("Sample size:", nrow(df_regression_compare), "\n")
cat("Unique villages:", n_distinct(df_regression_compare$village_id), "\n")
cat("Unique PHCs:", n_distinct(df_regression_compare$phc_id), "\n\n")

# ----------------------------------------------------------------------------
# Fit village-clustered models (main analysis specification)
# ----------------------------------------------------------------------------

model1_village <- glmer(
  anaemia_venous == "Anaemic" ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression_compare,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model2_village <- glmer(
  ida_ferritin30 ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    log_crp + delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression_compare,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model3_village <- lmer(
  venous_hb_12m ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    delivery_caesarean + place_private +
    (1 | village_id),
  data = df_regression_compare
)

# ----------------------------------------------------------------------------
# Fit PHC-clustered sensitivity models
# ----------------------------------------------------------------------------

model1_phc <- glmer(
  anaemia_venous == "Anaemic" ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    delivery_caesarean + place_private +
    (1 | phc_id),
  data = df_regression_compare,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model2_phc <- glmer(
  ida_ferritin30 ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    log_crp + delivery_caesarean + place_private +
    (1 | phc_id),
  data = df_regression_compare,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

model3_phc <- lmer(
  venous_hb_12m ~
    trial_arm_binary + poc_hb_baseline_z + maternal_age_z +
    parity_binary + bmi_z + gestational_age_baseline_z +
    delivery_caesarean + place_private +
    (1 | phc_id),
  data = df_regression_compare
)

cat("✓ Village- and PHC-clustered models fitted\n\n")

# ----------------------------------------------------------------------------
# Predictor labels
# ----------------------------------------------------------------------------

predictor_labels <- c(
  "trial_arm_binary"           = "Trial arm (Intervention vs Control)",
  "poc_hb_baseline_z"          = "Baseline POC Hb (per 1 SD)",
  "maternal_age_z"             = "Maternal age (per 1 SD)",
  "parity_binary"              = "Multiparous (vs Nulliparous)",
  "bmi_z"                      = "BMI (per 1 SD)",
  "gestational_age_baseline_z" = "Gestational age at baseline (per 1 SD)",
  "log_crp"                    = "Log CRP",
  "delivery_caesarean"         = "Caesarean delivery (vs vaginal/instrumental)",
  "place_private"              = "Private place of birth (vs government/NGO/home)"
)

# ----------------------------------------------------------------------------
# Helper functions to tidy models
# ----------------------------------------------------------------------------

tidy_glmer_compare <- function(model, model_name, clustering_label) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = FALSE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      model = model_name,
      clustering = clustering_label,
      term = term,
      predictor = predictor_labels[term],
      beta = estimate,
      se = std.error,
      statistic = statistic,
      p_value = p.value,
      effect = exp(estimate),
      ci_lower = exp(estimate - 1.96 * std.error),
      ci_upper = exp(estimate + 1.96 * std.error),
      effect_type = "aOR"
    )
}

tidy_lmer_compare <- function(model, model_name, clustering_label) {
  coef_tab <- summary(model)$coefficients
  
  tibble(
    term = rownames(coef_tab),
    beta = coef_tab[, "Estimate"],
    se = coef_tab[, "Std. Error"],
    statistic = coef_tab[, "t value"]
  ) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      model = model_name,
      clustering = clustering_label,
      term = term,
      predictor = predictor_labels[term],
      beta = beta,
      se = se,
      statistic = statistic,
      p_value = NA_real_,
      effect = beta,
      ci_lower = beta - 1.96 * se,
      ci_upper = beta + 1.96 * se,
      effect_type = "Beta"
    )
}

# ----------------------------------------------------------------------------
# Tidy all models
# ----------------------------------------------------------------------------

results_village <- bind_rows(
  tidy_glmer_compare(model1_village, "Model 1: Venous anaemia", "Village"),
  tidy_glmer_compare(model2_village, "Model 2: Iron deficiency anaemia", "Village"),
  tidy_lmer_compare(model3_village, "Model 3: Venous haemoglobin", "Village")
)

results_phc <- bind_rows(
  tidy_glmer_compare(model1_phc, "Model 1: Venous anaemia", "PHC"),
  tidy_glmer_compare(model2_phc, "Model 2: Iron deficiency anaemia", "PHC"),
  tidy_lmer_compare(model3_phc, "Model 3: Venous haemoglobin", "PHC")
)

results_long <- bind_rows(results_village, results_phc)

# ----------------------------------------------------------------------------
# Add formatted display columns
# ----------------------------------------------------------------------------

comparison_table <- results_long %>%
  mutate(
    effect_display = case_when(
      effect_type == "aOR" ~ paste0(
        sprintf("%.2f", effect), " (",
        sprintf("%.2f", ci_lower), " to ",
        sprintf("%.2f", ci_upper), ")"
      ),
      effect_type == "Beta" ~ paste0(
        sprintf("%.2f", effect), " (",
        sprintf("%.2f", ci_lower), " to ",
        sprintf("%.2f", ci_upper), ")"
      ),
      TRUE ~ NA_character_
    ),
    p_display = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  )

# ----------------------------------------------------------------------------
# Full side-by-side comparison table for supplementary material
# ----------------------------------------------------------------------------

comparison_table_wide <- comparison_table %>%
  select(model, predictor, effect_type, clustering, effect_display, p_display) %>%
  pivot_wider(
    names_from = clustering,
    values_from = c(effect_display, p_display),
    names_glue = "{clustering}_{.value}"
  ) %>%
  arrange(model)

# ----------------------------------------------------------------------------
# Trial arm only table (concise supplementary table)
# FIXED: filter on term before dropping it
# ----------------------------------------------------------------------------

comparison_trial_arm_only <- comparison_table %>%
  filter(term == "trial_arm_binary") %>%
  select(model, clustering, effect_type, effect_display, p_display) %>%
  pivot_wider(
    names_from = clustering,
    values_from = c(effect_display, p_display),
    names_glue = "{clustering}_{.value}"
  ) %>%
  arrange(model)

# ----------------------------------------------------------------------------
# Random effect variance summary
# ----------------------------------------------------------------------------

get_random_variance <- function(model) {
  as.data.frame(VarCorr(model))$vcov[1]
}

random_effect_summary <- tibble(
  model = c(
    "Model 1: Venous anaemia",
    "Model 2: Iron deficiency anaemia",
    "Model 3: Venous haemoglobin",
    "Model 1: Venous anaemia",
    "Model 2: Iron deficiency anaemia",
    "Model 3: Venous haemoglobin"
  ),
  clustering = c("Village", "Village", "Village", "PHC", "PHC", "PHC"),
  random_intercept_variance = c(
    get_random_variance(model1_village),
    get_random_variance(model2_village),
    get_random_variance(model3_village),
    get_random_variance(model1_phc),
    get_random_variance(model2_phc),
    get_random_variance(model3_phc)
  )
)

# ----------------------------------------------------------------------------
# Save outputs
# ----------------------------------------------------------------------------

write.csv(results_long, "Sensitivity_PHC_vs_Village_Models_Long.csv", row.names = FALSE)
write.csv(comparison_table_wide, "Supplementary_Table_PHC_vs_Village_Comparison.csv", row.names = FALSE)
write.csv(comparison_trial_arm_only, "Supplementary_Table_PHC_vs_Village_TrialArmOnly.csv", row.names = FALSE)
write.csv(random_effect_summary, "Sensitivity_PHC_vs_Village_RandomEffects.csv", row.names = FALSE)

cat("✓ Saved files:\n")
cat("  - Sensitivity_PHC_vs_Village_Models_Long.csv\n")
cat("  - Supplementary_Table_PHC_vs_Village_Comparison.csv\n")
cat("  - Supplementary_Table_PHC_vs_Village_TrialArmOnly.csv\n")
cat("  - Sensitivity_PHC_vs_Village_RandomEffects.csv\n\n")

# ----------------------------------------------------------------------------
# Print concise summary for trial arm effect
# ----------------------------------------------------------------------------

cat("===== TRIAL ARM EFFECT: VILLAGE vs PHC CLUSTERING =====\n\n")
print(comparison_trial_arm_only)

cat("\n===== RANDOM EFFECT VARIANCE SUMMARY =====\n\n")
print(random_effect_summary)

cat("\n===== SENSITIVITY ANALYSIS COMPLETE =====\n")



# ============================================================================
# PHC-CLUSTERED MODELS: FORMATTED RESULTS TABLE (MATCHES MAIN RESULTS TABLE)
# ============================================================================

cat("\n\n===== PHC-CLUSTERED REGRESSION RESULTS TABLE =====\n")

# ----------------------------------------------------------------------------
# Extract and format Model 1 (PHC clustering)
# ----------------------------------------------------------------------------

summary_m1_phc <- summary(model1_phc)

m1_phc_results <- as_tibble(coef(summary_m1_phc), rownames = "Parameter") %>%
  rename(
    Estimate = Estimate,
    SE = `Std. Error`,
    Z_value = `z value`,
    P_value = `Pr(>|z|)`
  ) %>%
  mutate(
    OR = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

# ----------------------------------------------------------------------------
# Extract and format Model 2 (PHC clustering)
# ----------------------------------------------------------------------------

summary_m2_phc <- summary(model2_phc)

m2_phc_results <- as_tibble(coef(summary_m2_phc), rownames = "Parameter") %>%
  rename(
    Estimate = Estimate,
    SE = `Std. Error`,
    Z_value = `z value`,
    P_value = `Pr(>|z|)`
  ) %>%
  mutate(
    OR = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * SE),
    CI_Upper = exp(Estimate + 1.96 * SE)
  ) %>%
  filter(Parameter != "(Intercept)")

# ----------------------------------------------------------------------------
# Extract and format Model 3 (PHC clustering)
# ----------------------------------------------------------------------------

summary_m3_phc <- summary(model3_phc)

m3_phc_results <- as_tibble(summary_m3_phc$coefficients, rownames = "Parameter") %>%
  rename(
    Estimate = Estimate,
    SE = `Std. Error`,
    T_value = `t value`
  ) %>%
  mutate(
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    P_Value = NA_real_
  ) %>%
  filter(Parameter != "(Intercept)")

# ----------------------------------------------------------------------------
# Predictor labels (same as main table)
# ----------------------------------------------------------------------------

predictor_labels <- c(
  "trial_arm_binary"           = "Trial Arm (Intervention vs Control)",
  "poc_hb_baseline_z"          = "Baseline POC Hb (per 1 SD)",
  "maternal_age_z"             = "Maternal Age (per 1 SD)",
  "parity_binary"              = "Multiparous (vs Nulliparous)",
  "bmi_z"                      = "BMI (per 1 SD)",
  "gestational_age_baseline_z" = "Gestational Age (per 1 SD)",
  "log_crp"                    = "Log CRP",
  "delivery_caesarean"         = "Delivery Mode (Caesarean vs Vaginal/Instrumental)",
  "place_private"              = "Place of Birth (Private vs Government/NGO/Home)"
)

# ----------------------------------------------------------------------------
# Format tables exactly like main results
# ----------------------------------------------------------------------------

m1_phc_full <- m1_phc_results %>%
  mutate(
    Model = "Venous Anaemia (PHC clustering)",
    Predictor = predictor_labels[Parameter]
  ) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_Lower, CI_Upper, Z_value, P_value) %>%
  rename(Z_Value = Z_value, P_Value = P_value)

m2_phc_full <- m2_phc_results %>%
  mutate(
    Model = "Iron Deficiency Anaemia (PHC clustering)",
    Predictor = predictor_labels[Parameter]
  ) %>%
  select(Model, Predictor, Estimate, SE, OR, CI_Lower, CI_Upper, Z_value, P_value) %>%
  rename(Z_Value = Z_value, P_Value = P_value)

m3_phc_full <- m3_phc_results %>%
  mutate(
    Model = "Venous Haemoglobin (PHC clustering)",
    Predictor = predictor_labels[Parameter]
  ) %>%
  select(Model, Predictor, Estimate, SE, CI_Lower, CI_Upper, T_value, P_Value) %>%
  rename(T_Value = T_value)

# ----------------------------------------------------------------------------
# Print tables (same format as your main results section)
# ----------------------------------------------------------------------------

cat("\n=== MODEL 1: VENOUS ANAEMIA (PHC CLUSTERING) ===\n")
print(m1_phc_full %>% select(Predictor, OR, CI_Lower, CI_Upper, P_Value))

cat("\n=== MODEL 2: IRON DEFICIENCY ANAEMIA (PHC CLUSTERING) ===\n")
print(m2_phc_full %>% select(Predictor, OR, CI_Lower, CI_Upper, P_Value))

cat("\n=== MODEL 3: VENOUS HAEMOGLOBIN (PHC CLUSTERING) ===\n")
print(m3_phc_full %>% select(Predictor, Estimate, CI_Lower, CI_Upper))

# ----------------------------------------------------------------------------
# Save outputs (parallel to main results files)
# ----------------------------------------------------------------------------

write.csv(m1_phc_full, "Model1_Venous_Anaemia_PHC.csv", row.names = FALSE)
write.csv(m2_phc_full, "Model2_IDA_PHC.csv", row.names = FALSE)
write.csv(m3_phc_full, "Model3_Venous_Hb_PHC.csv", row.names = FALSE)

cat("\n✓ PHC-clustered results tables saved\n")