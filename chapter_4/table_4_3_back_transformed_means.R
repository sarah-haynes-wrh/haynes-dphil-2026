# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: table_4_3_back_transformed_means.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Install if needed
 #install.packages(c("lme4", "lmerTest", "emmeans", "performance", "dplyr", "readxl"))
#install.packages("officer")
#install.packages("flextable")
#install.packages("pbkrtest")

library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library (readxl)
library(officer)  # for exporting to Word
library(flextable)  # for formatting the table
library(tidyr)
library(pbkrtest)

#Load XL FILE 
df <- read_excel(file.path("data", "CRP_Analysis_Master_Data.xlsx"))

run_biomarker_model <- function(df, outcome_var, log_var) {
  # 1. Define model formula dynamically
  formula <- as.formula(paste0(log_var, " ~ group * time + log10_CRP + (1 | id)"))
  
  # 2. Fit the model
  model <- lmer(formula, data = df)
  print(summary(model))
  
  # 3. Estimated marginal means
  emm <- emmeans(model, ~ group * time)
  
  # 4A. Within-group change from baseline to follow-up
  cat("\n==== Within-group change ====\n")
  within_change <- contrast(emm, "revpairwise", by = "group", simple = "time")
  print(summary(within_change))
  
  # 4B. Between-group difference in change
  cat("\n==== Between-group difference in change ====\n")
  diff_change <- contrast(emm, interaction = "pairwise", by = NULL)
  print(summary(diff_change))
  
  # 4C. Between-group difference at follow-up
  cat("\n==== Between-group difference at FollowUp ====\n")
  follow_up <- contrast(emmeans(model, ~ group | time, at = list(time = "FollowUp")), "pairwise")
  print(summary(follow_up))
  
  # 5. Model diagnostics
  cat("\n==== Model diagnostics ====\n")
  print(check_collinearity(model))
  print(check_normality(model))
  print(check_heteroscedasticity(model))
  
  invisible(model)  # return the model object without printing it again
}

df <- df %>%
  mutate(
    log10_CRP      = log10(CRP + 0.1),
    log10_stfr     = log10(stfr),
    log10_hepcidin = log10(hepcidin),
    log10_ferritin = log10(ferritin),
    log10_tsat     = log10(tsat)
  )

# sTfR model
cat("\n\n===== stfr model =====\n")
mod_stfr <- run_biomarker_model(df, "stfr", "log10_stfr")

# Hepcidin model
cat("\n\n===== hepcidin model =====\n")
mod_hepcidin <- run_biomarker_model(df, "hepcidin", "log10_hepcidin")

# Ferritin model
cat("\n\n===== ferritin model =====\n")
mod_ferritin <- run_biomarker_model(df, "ferritin", "log10_ferritin")

# TSAT model
cat("\n\n===== tsat model =====\n")
mod_tsat <- run_biomarker_model(df, "tsat", "log10_tsat")

#Results TABLE: 


#FuNTCION: 
extract_summary <- function(model, biomarker_name, unit) {
  
  # Estimated marginal means on log10 scale
  emm <- emmeans(model, ~ group * time)
  emm_df <- as.data.frame(emm)
  
  # Back-transform explicitly using 10^x
  emm_df <- emm_df %>%
    mutate(
      raw_mean   = 10 ^ emmean,
      raw_lower  = 10 ^ lower.CL,
      raw_upper  = 10 ^ upper.CL
    )
  
  # Calculate change and percent change
  summary_df <- emm_df %>%
    select(group, time, raw_mean, raw_lower, raw_upper) %>%
    pivot_wider(names_from = time, values_from = c(raw_mean, raw_lower, raw_upper)) %>%
    mutate(
      Absolute_Change = raw_mean_FollowUp - raw_mean_Baseline,
      Percent_Change = (Absolute_Change / raw_mean_Baseline) * 100,
      Biomarker = paste0(biomarker_name, " (", unit, ")")
    )
  
  # Within-group p-values
  within <- contrast(emm, "revpairwise", by = "group")
  within_df <- summary(within) %>%
    select(group, p.value) %>%
    rename(Within_group_p = p.value)
  
  # Merge with summary
  summary_df <- left_join(summary_df, within_df, by = "group")
  
  # Between-group comparisons of change
  between <- contrast(emm, interaction = "pairwise")
  between_df <- summary(between) %>%
    mutate(
      group_Comparison = .data$group_pairwise,
      Between_group_p = p.value
    ) %>%
    select(group_Comparison, Between_group_p)
  
  list(summary = summary_df, between = between_df)
}


# Extract results for each biomarker
results_stfr     <- extract_summary(mod_stfr, "stfr", "mg/L")
results_hepcidin <- extract_summary(mod_hepcidin, "hepcidin", "ng/mL")
results_ferritin <- extract_summary(mod_ferritin, "ferritin", "µg/L")
results_tsat     <- extract_summary(mod_tsat, "tsat", "%")

#Combine all Summaries Together
combined_summary <- bind_rows(
  results_stfr$summary,
  results_hepcidin$summary,
  results_ferritin$summary,
  results_tsat$summary
) %>%
  mutate(across(where(is.numeric), round, digits = 2))

print(combined_summary)

library(writexl)

format_ci <- function(mean, lower, upper) {
  paste0(mean, " (", lower, "–", upper, ")")
}

# Combine all summary tables
combined_summary <- bind_rows(
  results_stfr$summary,
  results_hepcidin$summary,
  results_ferritin$summary,
  results_tsat$summary
)

# Format for output
final_table <- combined_summary %>%
  mutate(
    `Baseline (Adjusted Mean [95% CI])` = format_ci(round(raw_mean_Baseline, 2), round(raw_lower_Baseline, 2), round(raw_upper_Baseline, 2)),
    `FollowUp (Adjusted Mean [95% CI])` = format_ci(round(raw_mean_FollowUp, 2), round(raw_lower_FollowUp, 2), round(raw_upper_FollowUp, 2)),
    `Absolute Change` = round(Absolute_Change, 2),
    `Percent Change (%)` = round(Percent_Change, 1),
    `p (Within-group)` = signif(Within_group_p, 2)
  ) %>%
  select(
    Biomarker, group,
    `Baseline (Adjusted Mean [95% CI])`,
    `FollowUp (Adjusted Mean [95% CI])`,
    `Absolute Change`, `Percent Change (%)`,
    `p (Within-group)`
  )

# Write to Excel
write_xlsx(final_table, "CRP_Adjusted_Biomarker_Summary.xlsx")
