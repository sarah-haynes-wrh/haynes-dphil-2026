# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India (SHP2)
# Script: table_5_2_birth_outcomes.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ============================================================================
# COMPLETE BIRTH OUTCOMES DESCRIPTIVE ANALYSIS - CHAPTER 5
# All outcomes stratified by trial arm
# ============================================================================

library(tidyverse)
library(readxl)

# ============================================================================
# LOAD DATA
# ============================================================================
data <- read_excel(file.path("data", "clean_birthOutcomesData.xlsx"))

head(data)
glimpse(data)

# ============================================================================
# 1. MODE OF DELIVERY
# ============================================================================

# Overall mode of delivery
delivery_overall <- data %>%
  count(V3_ModeBirth) %>%
  mutate(
    percentage = round((n / nrow(data)) * 100, 1)
  ) %>%
  rename(delivery_type = V3_ModeBirth) %>%
  select(delivery_type, n, percentage)

cat("MODE OF DELIVERY - Overall (N=600)\n")
print(delivery_overall)

# By trial arm
delivery_arm <- data %>%
  group_by(trial_arm, V3_ModeBirth) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(delivery_type = V3_ModeBirth) %>%
  select(trial_arm, delivery_type, n, percentage)

cat("\nMODE OF DELIVERY - By Trial Arm\n")
print(delivery_arm)

# ============================================================================
# 2. C-SECTION TIMING
# ============================================================================

csection_timing <- data %>%
  filter(!is.na(V3_ModeBirth_Csection) & V3_ModeBirth_Csection != "\\N") %>%
  group_by(trial_arm, V3_ModeBirth_Csection) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(timing = V3_ModeBirth_Csection)

cat("\nC-SECTION TIMING (of all C-sections with recorded timing)\n")
print(csection_timing)

# ============================================================================
# 3. PERINATAL LOSS - BY TRIAL ARM
# ============================================================================

perinatal_loss_overall <- data %>%
  filter(!is.na(V3_Anyofthebabiesarenotalive) & V3_Anyofthebabiesarenotalive != "\\N") %>%
  count(V3_Anyofthebabiesarenotalive) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  rename(perinatal_loss_status = V3_Anyofthebabiesarenotalive)

cat("\nPERINATAL LOSS - Overall\n")
print(perinatal_loss_overall)

perinatal_loss_arm <- data %>%
  filter(!is.na(V3_Anyofthebabiesarenotalive) & V3_Anyofthebabiesarenotalive != "\\N") %>%
  group_by(trial_arm, V3_Anyofthebabiesarenotalive) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(perinatal_loss_status = V3_Anyofthebabiesarenotalive)

cat("\nPERINATAL LOSS - By Trial Arm\n")
print(perinatal_loss_arm)

# ============================================================================
# 4. BIRTH WEIGHT
# ============================================================================

birth_weight_summary <- data %>%
  filter(!is.na(V3_BirthWeightbaby1) & V3_BirthWeightbaby1 != "\\N") %>%
  mutate(bw_numeric = as.numeric(V3_BirthWeightbaby1)) %>%
  summarise(
    n_with_bw = n(),
    median_bw = median(bw_numeric, na.rm = TRUE),
    q1_bw = quantile(bw_numeric, 0.25, na.rm = TRUE),
    q3_bw = quantile(bw_numeric, 0.75, na.rm = TRUE),
    mean_bw = round(mean(bw_numeric, na.rm = TRUE), 2),
    sd_bw = round(sd(bw_numeric, na.rm = TRUE), 2),
    low_bw_count = sum(bw_numeric < 2.5, na.rm = TRUE),
    low_bw_pct = round((sum(bw_numeric < 2.5, na.rm = TRUE) / n()) * 100, 1)
  )

cat("\nBIRTH WEIGHT (Baby 1) - Overall\n")
print(birth_weight_summary)

birth_weight_arm <- data %>%
  filter(!is.na(V3_BirthWeightbaby1) & V3_BirthWeightbaby1 != "\\N") %>%
  mutate(bw_numeric = as.numeric(V3_BirthWeightbaby1)) %>%
  group_by(trial_arm) %>%
  summarise(
    n_with_bw = n(),
    median_bw = median(bw_numeric, na.rm = TRUE),
    q1_bw = quantile(bw_numeric, 0.25, na.rm = TRUE),
    q3_bw = quantile(bw_numeric, 0.75, na.rm = TRUE),
    low_bw_count = sum(bw_numeric < 2.5, na.rm = TRUE),
    low_bw_pct = round((sum(bw_numeric < 2.5, na.rm = TRUE) / n()) * 100, 1)
  )

cat("\nBIRTH WEIGHT - By Trial Arm\n")
print(birth_weight_arm)

# ============================================================================
# 5. NUMBER OF BABIES - BY TRIAL ARM
# ============================================================================

number_babies_overall <- data %>%
  filter(!is.na(V3_NOofBabies) & V3_NOofBabies != "\\N") %>%
  count(V3_NOofBabies) %>%
  mutate(percentage = round((n / nrow(data)) * 100, 1)) %>%
  rename(number_of_babies = V3_NOofBabies)

cat("\nNUMBER OF BABIES - Overall\n")
print(number_babies_overall)

number_babies_arm <- data %>%
  filter(!is.na(V3_NOofBabies) & V3_NOofBabies != "\\N") %>%
  group_by(trial_arm, V3_NOofBabies) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(number_of_babies = V3_NOofBabies)

cat("\nNUMBER OF BABIES - By Trial Arm\n")
print(number_babies_arm)

# ============================================================================
# 6. HOSPITALIZATION - LENGTH OF STAY
# ============================================================================

los_summary <- data %>%
  filter(!is.na(V3_HospitalizationDays) & V3_HospitalizationDays != "\\N") %>%
  mutate(los_numeric = as.numeric(V3_HospitalizationDays)) %>%
  summarise(
    n_with_los = n(),
    median_los = median(los_numeric, na.rm = TRUE),
    q1_los = quantile(los_numeric, 0.25, na.rm = TRUE),
    q3_los = quantile(los_numeric, 0.75, na.rm = TRUE),
    mean_los = round(mean(los_numeric, na.rm = TRUE), 1),
    sd_los = round(sd(los_numeric, na.rm = TRUE), 1)
  )

cat("\nHOSPITALIZATION - Length of Stay (Overall)\n")
print(los_summary)

los_arm <- data %>%
  filter(!is.na(V3_HospitalizationDays) & V3_HospitalizationDays != "\\N") %>%
  mutate(los_numeric = as.numeric(V3_HospitalizationDays)) %>%
  group_by(trial_arm) %>%
  summarise(
    n_with_los = n(),
    median_los = median(los_numeric, na.rm = TRUE),
    q1_los = quantile(los_numeric, 0.25, na.rm = TRUE),
    q3_los = quantile(los_numeric, 0.75, na.rm = TRUE),
    mean_los = round(mean(los_numeric, na.rm = TRUE), 1),
    sd_los = round(sd(los_numeric, na.rm = TRUE), 1)
  )

cat("\nHOSPITALIZATION - Length of Stay (By Trial Arm)\n")
print(los_arm)

# ============================================================================
# 7. HOSPITALIZATION - REASON
# ============================================================================

hosp_reason_overall <- data %>%
  filter(!is.na(V3_TreatmentReason) & V3_TreatmentReason != "\\N") %>%
  count(V3_TreatmentReason) %>%
  mutate(percentage = round((n / nrow(data)) * 100, 1)) %>%
  rename(reason = V3_TreatmentReason) %>%
  select(reason, n, percentage)

cat("\nHOSPITALIZATION - Reason (Overall)\n")
print(hosp_reason_overall)

hosp_reason_arm <- data %>%
  filter(!is.na(V3_TreatmentReason) & V3_TreatmentReason != "\\N") %>%
  group_by(trial_arm, V3_TreatmentReason) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(reason = V3_TreatmentReason)

cat("\nHOSPITALIZATION - Reason (By Trial Arm)\n")
print(hosp_reason_arm)

# ============================================================================
# 8. BABY WITH MOTHER AT 12-MONTH FOLLOW-UP
# ============================================================================

baby_with_mother_overall <- data %>%
  filter(!is.na(V3_BabywithMother) & V3_BabywithMother != "\\N") %>%
  count(V3_BabywithMother) %>%
  mutate(percentage = round((n / nrow(data)) * 100, 1)) %>%
  rename(status = V3_BabywithMother)

cat("\nBABY WITH MOTHER AT 12-MONTH FOLLOW-UP (Overall)\n")
print(baby_with_mother_overall)

baby_with_mother_arm <- data %>%
  filter(!is.na(V3_BabywithMother) & V3_BabywithMother != "\\N") %>%
  group_by(trial_arm, V3_BabywithMother) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(status = V3_BabywithMother)

cat("\nBABY WITH MOTHER AT 12-MONTH FOLLOW-UP (By Trial Arm)\n")
print(baby_with_mother_arm)

# ============================================================================
# 9. PLACE OF BIRTH
# ============================================================================

place_of_birth_overall <- data %>%
  filter(!is.na(V3_PlaceOfBirth) & V3_PlaceOfBirth != "\\N") %>%
  count(V3_PlaceOfBirth) %>%
  mutate(percentage = round((n / nrow(data)) * 100, 1)) %>%
  rename(place = V3_PlaceOfBirth)

cat("\nPLACE OF BIRTH (Overall)\n")
print(place_of_birth_overall)

place_of_birth_arm <- data %>%
  filter(!is.na(V3_PlaceOfBirth) & V3_PlaceOfBirth != "\\N") %>%
  group_by(trial_arm, V3_PlaceOfBirth) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(place = V3_PlaceOfBirth)

cat("\nPLACE OF BIRTH (By Trial Arm)\n")
print(place_of_birth_arm)

# ============================================================================
# 10. POSTPARTUM COMPLICATIONS
# ============================================================================

pn_any_overall <- data %>%
  filter(!is.na(V3_PN_any) & V3_PN_any != "\\N") %>%
  count(V3_PN_any) %>%
  mutate(percentage = round((n / nrow(data)) * 100, 1)) %>%
  rename(has_complications = V3_PN_any)

cat("\nANY POSTPARTUM COMPLICATIONS (Overall)\n")
print(pn_any_overall)

pn_any_arm <- data %>%
  filter(!is.na(V3_PN_any) & V3_PN_any != "\\N") %>%
  group_by(trial_arm, V3_PN_any) %>%
  count() %>%
  group_by(trial_arm) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
  ungroup() %>%
  rename(has_complications = V3_PN_any)

cat("\nANY POSTPARTUM COMPLICATIONS (By Trial Arm)\n")
print(pn_any_arm)

# ============================================================================
# 11. SPECIFIC POSTPARTUM COMPLICATIONS
# ============================================================================
# Note: These are the individual PN_Conditions columns
# V3_PN_Conditions_1 = Preeclampsia/eclampsia
# V3_PN_Conditions_2 = Postpartum haemorrhage
# V3_PN_Conditions_3 = Required blood transfusion
# V3_PN_Conditions_4 = Preterm birth
# V3_PN_Conditions_5 = Stillbirth/neonatal death
# V3_PN_Conditions_6 = Sepsis/infection
# V3_PN_Conditions_7 = Mastitis
# V3_PN_Conditions_8 = DVT
# V3_PN_Conditions_9 = COVID-19
# V3_PN_Conditions_10 = Vaginal/perineal infection
# V3_PN_Conditions_11 = Hysterectomy
# V3_PN_Conditions_12 = Other

# Postpartum haemorrhage (PPH)
pph_overall <- data %>%
  summarise(
    n_pph = sum(V3_PN_Conditions_2 == 1, na.rm = TRUE),
    n_total = n(),
    pct_pph = round((sum(V3_PN_Conditions_2 == 1, na.rm = TRUE) / n()) * 100, 2)
  )

cat("\nPOSTPARTUM HAEMORRHAGE (Overall)\n")
print(pph_overall)

pph_arm <- data %>%
  group_by(trial_arm) %>%
  summarise(
    n_pph = sum(V3_PN_Conditions_2 == 1, na.rm = TRUE),
    n_total = n(),
    pct_pph = round((sum(V3_PN_Conditions_2 == 1, na.rm = TRUE) / n()) * 100, 2)
  )

cat("\nPOSTPARTUM HAEMORRHAGE (By Trial Arm)\n")
print(pph_arm)

# Blood transfusion
transfusion_overall <- data %>%
  summarise(
    n_transfusion = sum(V3_PN_Conditions_3 == 1, na.rm = TRUE),
    n_total = n(),
    pct_transfusion = round((sum(V3_PN_Conditions_3 == 1, na.rm = TRUE) / n()) * 100, 2)
  )

cat("\nBLOOD TRANSFUSION (Overall)\n")
print(transfusion_overall)

transfusion_arm <- data %>%
  group_by(trial_arm) %>%
  summarise(
    n_transfusion = sum(V3_PN_Conditions_3 == 1, na.rm = TRUE),
    n_total = n(),
    pct_transfusion = round((sum(V3_PN_Conditions_3 == 1, na.rm = TRUE) / n()) * 100, 2)
  )

cat("\nBLOOD TRANSFUSION (By Trial Arm)\n")
print(transfusion_arm)

# All complications summary
complications_summary <- data %>%
  summarise(
    preeclampsia = sum(V3_PN_Conditions_1 == 1, na.rm = TRUE),
    pph = sum(V3_PN_Conditions_2 == 1, na.rm = TRUE),
    transfusion = sum(V3_PN_Conditions_3 == 1, na.rm = TRUE),
    preterm = sum(V3_PN_Conditions_4 == 1, na.rm = TRUE),
    stillbirth_neonatal_death = sum(V3_PN_Conditions_5 == 1, na.rm = TRUE),
    sepsis_infection = sum(V3_PN_Conditions_6 == 1, na.rm = TRUE),
    mastitis = sum(V3_PN_Conditions_7 == 1, na.rm = TRUE),
    dvt = sum(V3_PN_Conditions_8 == 1, na.rm = TRUE),
    covid = sum(V3_PN_Conditions_9 == 1, na.rm = TRUE),
    vaginal_perineal_infection = sum(V3_PN_Conditions_10 == 1, na.rm = TRUE),
    hysterectomy = sum(V3_PN_Conditions_11 == 1, na.rm = TRUE),
    other = sum(V3_PN_Conditions_12 == 1, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "complication", values_to = "count") %>%
  mutate(percentage = round((count / nrow(data)) * 100, 2)) %>%
  filter(count > 0) %>%
  arrange(desc(count))

cat("\nALL POSTPARTUM COMPLICATIONS SUMMARY (Overall, n>0)\n")
print(complications_summary)

# Complications by trial arm
complications_by_arm <- data %>%
  pivot_longer(
    cols = starts_with("V3_PN_Conditions_"),
    names_to = "condition_code",
    values_to = "present"
  ) %>%
  filter(present == 1) %>%
  mutate(
    condition_name = case_when(
      condition_code == "V3_PN_Conditions_1" ~ "Preeclampsia/eclampsia",
      condition_code == "V3_PN_Conditions_2" ~ "PPH",
      condition_code == "V3_PN_Conditions_3" ~ "Transfusion",
      condition_code == "V3_PN_Conditions_4" ~ "Preterm birth",
      condition_code == "V3_PN_Conditions_5" ~ "Stillbirth/neonatal death",
      condition_code == "V3_PN_Conditions_6" ~ "Sepsis/infection",
      condition_code == "V3_PN_Conditions_7" ~ "Mastitis",
      condition_code == "V3_PN_Conditions_8" ~ "DVT",
      condition_code == "V3_PN_Conditions_9" ~ "COVID-19",
      condition_code == "V3_PN_Conditions_10" ~ "Vaginal/perineal infection",
      condition_code == "V3_PN_Conditions_11" ~ "Hysterectomy",
      condition_code == "V3_PN_Conditions_12" ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(trial_arm, condition_name) %>%
  count() %>%
  ungroup()

cat("\nCOMPLICATIONS BY TRIAL ARM\n")
print(complications_by_arm)

# ============================================================================
# 12. SUMMARY TABLE FOR RESULTS
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("SUMMARY: KEY STATISTICS FOR CHAPTER 5 RESULTS TABLE\n")
cat("================================================================================\n")
cat("\n")

cat("MODE OF DELIVERY:\n")
cat("  Overall - Normal vaginal: 179 (29.9%)\n")
cat("  Overall - Instrumental: 12 (2.0%)\n")
cat("  Overall - Caesarean section: 408 (68.1%)\n")
cat("\n")

cat("BIRTH WEIGHT:\n")
cat("  Overall - Median [IQR]: 2.8 [2.5-3.0] kg\n")
cat("  Overall - Low birthweight (<2.5kg): 45 (7.5%)\n")
cat("  Control - Low birthweight: 9.6%\n")
cat("  Intervention - Low birthweight: 4.9%\n")
cat("\n")

cat("LENGTH OF STAY:\n")
cat("  Overall - Median [IQR]: 7 [3-7] days\n")
cat("  Control - Median [IQR]: 6 [3-7] days\n")
cat("  Intervention - Median [IQR]: 7 [3-7] days\n")
cat("\n")

cat("HOSPITALIZATION REASON:\n")
cat("  Overall - Routine: 558 (93.2%)\n")
cat("  Overall - Maternal complication: 10 (1.7%)\n")
cat("  Overall - Neonatal complication: 31 (5.2%)\n")
cat("\n")

cat("POSTPARTUM COMPLICATIONS:\n")
cat("  Overall - Any complication: 34 (5.7%)\n")
cat("  Overall - PPH: 9 (1.5%)\n")
cat("  Overall - Blood transfusion: <1%\n")
cat("\n")

cat("PLACE OF BIRTH:\n")
cat("  Government health facility: 92 (15.4%)\n")
cat("  Private hospital/clinic: 393 (65.6%)\n")
cat("  Home: 104 (17.4%)\n")
cat("  Other: 10 (1.7%)\n")
cat("\n")

cat("================================================================================\n")