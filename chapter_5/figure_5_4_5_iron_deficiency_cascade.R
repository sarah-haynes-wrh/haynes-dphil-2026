# Chapter 5: Prevalence and Aetiology of Postpartum Anaemia in Rural India (SHP2)
# Script: figure_5_4_5_iron_deficiency_cascade.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ============================================================================
# IRON DEFICIENCY CASCADE & SCATTER PLOT ANALYSIS
# Chapter 5: Postpartum Anaemia in Rural India - SMARThealth Pregnancy
#
# OUTPUT: 2 Publication Figures + 3 Results Tables
# ============================================================================

library(tidyverse)
library(ggplot2)
library(readxl)
library(knitr)

# ============================================================================
# STEP 1: DATA LOADING & PREPARATION
# ============================================================================

cat("\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\nLOADING DATA FROM EXCEL FILE\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

excel_file_path <- file.path("data", "master_results_april_2026_hepcidin_birthoutcomes.xlsx")
sheet_name <- "biomarkers_only"

df <- read_excel(excel_file_path, sheet = sheet_name)

cat(sprintf("✓ Loaded %d rows and %d columns\n", nrow(df), ncol(df)))
cat(sprintf("✓ Sheet: '%s'\n\n", sheet_name))

# ============================================================================
# STEP 2: INITIAL DATA PREPARATION
# ============================================================================

df <- df %>%
  mutate(
    # Keep anaemia status missing when venous Hb is missing
    anaemia_status = case_when(
      !is.na(venous_hb) & venous_hb < 120 ~ "anaemic",
      !is.na(venous_hb) & venous_hb >= 120 ~ "non-anaemic",
      TRUE ~ NA_character_
    ),
    trial_arm = case_when(
      tx_arm == "control" ~ "control",
      tx_arm == "intervention" ~ "intervention",
      TRUE ~ "unknown"
    ),
    
    # ── Ferritin index (consistent with master_analysis_script.R) ─────────
    # Definition: sTfR (mg/L) / log10(ferritin µg/L)  [Cook et al. 2003]
    # Threshold for iron deficiency: ≥1.03
    # ─────────────────────────────────────────────────────────────────────
    ferritin_index_calc = case_when(
      !is.na(stfr_mgL) & !is.na(aiims_ferr) & aiims_ferr > 0 ~ stfr_mgL / log10(aiims_ferr),
      TRUE ~ NA_real_
    ),
    ferritin_index = coalesce(ferritin_index, ferritin_index_calc)
  ) %>%
  filter(!is.na(trial_arm))

total_n <- nrow(df)
anaemic_n <- sum(df$anaemia_status == "anaemic", na.rm = TRUE)
non_anaemic_n <- sum(df$anaemia_status == "non-anaemic", na.rm = TRUE)
missing_hb_n <- sum(is.na(df$anaemia_status))

cat(sprintf("✓ Data prepared with %d participants\n", total_n))
cat(sprintf("  Anaemic: %d\n", anaemic_n))
cat(sprintf("  Non-anaemic: %d\n", non_anaemic_n))
cat(sprintf("  Missing venous Hb / anaemia status: %d\n\n", missing_hb_n))

# ============================================================================
# STEP 3: BRINDA CORRECTION (REGRESSION-BASED, LOG SCALE)
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nCALCULATING BRINDA-CORRECTED FERRITIN\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

# Reference CRP concentration
ref_crp <- 0.5

# Reference subset used to estimate regression coefficient
# (participants with CRP <= 5 mg/L, ferritin > 0, CRP > 0)
brinda_ref <- df %>%
  filter(
    !is.na(aiims_ferr),
    !is.na(aiims_crp),
    aiims_ferr > 0,
    aiims_crp > 0,
    aiims_crp <= 5
  )

cat(sprintf("Reference subset for BRINDA model: n = %d\n", nrow(brinda_ref)))

# Regression of log ferritin on log CRP
brinda_model <- lm(log(aiims_ferr) ~ log(aiims_crp), data = brinda_ref)
beta_crp <- coef(brinda_model)[["log(aiims_crp)"]]

cat(sprintf("Estimated BRINDA regression coefficient for log(CRP): %.4f\n\n", beta_crp))

# Apply correction
# Standard approach: only adjust when CRP > reference CRP
df <- df %>%
  mutate(
    ferritin_brinda = case_when(
      !is.na(aiims_ferr) & !is.na(aiims_crp) &
        aiims_ferr > 0 & aiims_crp > 0 & aiims_crp > ref_crp ~
        exp(log(aiims_ferr) - beta_crp * (log(aiims_crp) - log(ref_crp))),
      !is.na(aiims_ferr) ~ aiims_ferr,
      TRUE ~ NA_real_
    )
  )

# ============================================================================
# STEP 4: CREATE IRON DEFICIENCY MARKERS
# ============================================================================

df <- df %>%
  mutate(
    ferritin_lt15 = case_when(
      !is.na(aiims_ferr) ~ as.integer(aiims_ferr < 15),
      TRUE ~ NA_integer_
    ),
    ferritin_lt30_uncorr = case_when(
      !is.na(aiims_ferr) ~ as.integer(aiims_ferr < 30),
      TRUE ~ NA_integer_
    ),
    ferritin_lt15_brinda = case_when(
      !is.na(ferritin_brinda) ~ as.integer(ferritin_brinda < 15),
      TRUE ~ NA_integer_
    ),
    ferritin_lt30_brinda = case_when(
      !is.na(ferritin_brinda) ~ as.integer(ferritin_brinda < 30),
      TRUE ~ NA_integer_
    ),
    stfr_gte155 = case_when(
      !is.na(stfr_mgL) ~ as.integer(stfr_mgL >= 1.55),
      TRUE ~ NA_integer_
    ),
    ferritin_index_gte103 = case_when(
      !is.na(ferritin_index) ~ as.integer(ferritin_index >= 1.03),
      TRUE ~ NA_integer_
    )
  ) %>%
  mutate(
    any_marker = case_when(
      rowSums(
        cbind(
          ferritin_lt30_uncorr == 1,
          ferritin_lt30_brinda == 1,
          stfr_gte155 == 1,
          ferritin_index_gte103 == 1
        ),
        na.rm = TRUE
      ) > 0 ~ 1L,
      rowSums(
        cbind(
          !is.na(ferritin_lt30_uncorr),
          !is.na(ferritin_lt30_brinda),
          !is.na(stfr_gte155),
          !is.na(ferritin_index_gte103)
        )
      ) > 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    fi_status = factor(
      ferritin_index_gte103,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# ============================================================================
# HELPER FUNCTION FOR MARKER-SPECIFIC DENOMINATORS
# ============================================================================

get_counts <- function(data, var) {
  denom <- sum(!is.na(data[[var]]))
  numer <- sum(data[[var]] == 1, na.rm = TRUE)
  pct <- ifelse(denom > 0, round((numer / denom) * 100, 1), NA_real_)
  
  tibble(
    denominator = denom,
    numerator = numer,
    percentage = pct
  )
}

# ============================================================================
# FIGURE 1: IRON DEFICIENCY PREVALENCE BY BIOMARKER DEFINITION
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nFIGURE 1: IRON DEFICIENCY PREVALENCE BY BIOMARKER DEFINITION\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

cascade_definitions <- c(
  "ferritin_lt15",
  "ferritin_lt30_uncorr",
  "ferritin_lt30_brinda",
  "stfr_gte155",
  "ferritin_index_gte103",
  "any_marker"
)

cascade_labels <- c(
  "Ferritin <15 µg/L",
  "Ferritin <30 µg/L",
  "BRINDA Ferritin <30 µg/L",
  "sTfR ≥1.55 mg/L",
  "Ferritin Index ≥1.03",
  "Any Iron Marker"
)

cascade_all <- map2_dfr(cascade_definitions, cascade_labels, ~{
  all_stats <- get_counts(df, .x)
  anaemic_stats <- get_counts(df %>% filter(anaemia_status == "anaemic"), .x)
  
  tibble(
    marker = .y,
    n_all = all_stats$numerator,
    denom_all = all_stats$denominator,
    pct_all = all_stats$percentage,
    n_anaemic = anaemic_stats$numerator,
    denom_anaemic = anaemic_stats$denominator,
    pct_anaemic = anaemic_stats$percentage
  )
})

cat("IRON DEFICIENCY PREVALENCE BY DEFINITION:\n")
cat(paste(rep("-", 120), collapse = ""))
cat("\n")
cat(sprintf("%-32s | All Women             | Anaemic Women\n", "Definition"))
cat(sprintf("%-32s | n/N (%%)              | n/N (%%)\n", ""))
cat(paste(rep("-", 120), collapse = ""))
cat("\n")

for (i in 1:nrow(cascade_all)) {
  cat(sprintf(
    "%-32s | %3d/%-3d (%.1f%%)      | %3d/%-3d (%.1f%%)\n",
    cascade_all$marker[i],
    cascade_all$n_all[i], cascade_all$denom_all[i], cascade_all$pct_all[i],
    cascade_all$n_anaemic[i], cascade_all$denom_anaemic[i], cascade_all$pct_anaemic[i]
  ))
}

cat("\n\n")

cascade_plot_data <- bind_rows(
  cascade_all %>%
    transmute(
      marker,
      group = "All Women",
      count = n_all,
      denominator = denom_all,
      percentage = pct_all
    ),
  cascade_all %>%
    transmute(
      marker,
      group = "Anaemic Women",
      count = n_anaemic,
      denominator = denom_anaemic,
      percentage = pct_anaemic
    )
) %>%
  mutate(
    marker = factor(marker, levels = cascade_labels),
    group = factor(group, levels = c("All Women", "Anaemic Women"))
  )

cascade_plot <- ggplot(cascade_plot_data, aes(x = marker, y = percentage, fill = group)) +
  geom_col(
    position = position_dodge(width = 0.8),
    alpha = 0.85,
    color = "black",
    linewidth = 1
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%\n(n=%d)", percentage, as.integer(count))),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("All Women" = "#3498DB", "Anaemic Women" = "#E74C3C"),
    name = "Population"
  ) +
  labs(
    title = "Iron Deficiency Prevalence by Definition",
    subtitle = "Comparison of all women vs anaemic women only",
    y = "Prevalence (%)",
    x = "Iron Deficiency Definition"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#1F4E78"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#2E75B6"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("Figure_1_Iron_Deficiency_Cascade.png", cascade_plot, width = 14, height = 8, dpi = 300)
cat("✓ Saved: Figure_1_Iron_Deficiency_Cascade.png\n\n")

# ============================================================================
# FIGURE 2: SCATTER PLOT - IRON STATUS IN ANAEMIC WOMEN
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nFIGURE 2: IRON STATUS SCATTER PLOT (ANAEMIC WOMEN)\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

df_anaemic <- df %>%
  filter(anaemia_status == "anaemic")

ferritin_p99 <- quantile(df_anaemic$aiims_ferr, 0.99, na.rm = TRUE)
stfr_p99 <- quantile(df_anaemic$stfr_mgL, 0.99, na.rm = TRUE)

cat(sprintf("Ferritin 99th percentile: %.1f µg/L\n", ferritin_p99))
cat(sprintf("sTfR 99th percentile: %.2f mg/L\n", stfr_p99))
cat(sprintf(
  "Anaemic women with complete ferritin+sTfR data: %d\n\n",
  sum(!is.na(df_anaemic$aiims_ferr) & !is.na(df_anaemic$stfr_mgL))
))

plot_data <- df_anaemic %>%
  mutate(
    ferritin_plot = pmin(aiims_ferr, ferritin_p99),
    stfr_plot = pmin(stfr_mgL, stfr_p99)
  ) %>%
  filter(!is.na(ferritin_plot) & !is.na(stfr_plot))

scatter_plot <- ggplot(plot_data, aes(x = ferritin_plot, y = stfr_plot)) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "gray50", linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = 1.55, linetype = "dashed", color = "gray50", linewidth = 1, alpha = 0.7) +
  geom_point(
    aes(color = fi_status, size = fi_status),
    alpha = 0.6,
    position = position_jitter(width = 0.05, height = 0.02)
  ) +
  scale_color_manual(
    values = c("No" = "#3498DB", "Yes" = "#E74C3C"),
    labels = c("No" = "FI <1.03", "Yes" = "FI ≥1.03"),
    name = "Ferritin Index"
  ) +
  scale_size_manual(
    values = c("No" = 2, "Yes" = 3),
    guide = "none"
  ) +
  scale_x_log10(
    breaks = c(5, 10, 20, 30, 50, 100, 200),
    labels = c("5", "10", "20", "30", "50", "100", "200")
  ) +
  scale_y_log10(
    breaks = c(0.5, 1, 1.55, 2, 3, 5, 10),
    labels = c("0.5", "1.0", "1.55", "2.0", "3.0", "5.0", "10")
  ) +
  labs(
    title = "Iron Status in Anaemic Women: Ferritin vs sTfR",
    subtitle = "Red = Ferritin Index ≥1.03 (iron-restricted); Blue = Ferritin Index <1.03\nLog scales; axes truncated at 99th percentile",
    x = "Serum Ferritin (µg/L) - Log Scale",
    y = "Soluble Transferrin Receptor (mg/L) - Log Scale"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#1F4E78"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#2E75B6"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray95", linetype = "dotted", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("Figure_2_Iron_Status_Scatter_Anaemic.png", scatter_plot, width = 14, height = 9, dpi = 300)
cat("✓ Saved: Figure_2_Iron_Status_Scatter_Anaemic.png\n\n")

# ============================================================================
# RESULTS TABLE 1: COMPREHENSIVE IRON DEFICIENCY PREVALENCE
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nRESULTS TABLE 1: COMPREHENSIVE IRON DEFICIENCY PREVALENCE\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

iron_definitions <- c(
  "ferritin_lt15",
  "ferritin_lt30_uncorr",
  "ferritin_lt30_brinda",
  "stfr_gte155",
  "ferritin_index_gte103",
  "any_marker"
)

iron_labels <- c(
  "Ferritin <15 µg/L",
  "Ferritin <30 µg/L (uncorrected)",
  "Ferritin <30 µg/L (BRINDA-corrected)",
  "sTfR ≥1.55 mg/L",
  "Ferritin Index ≥1.03",
  "Any Iron Marker Positive"
)

results_table1 <- map2_dfr(iron_definitions, iron_labels, ~{
  all_stats <- get_counts(df, .x)
  anaemic_stats <- get_counts(df %>% filter(anaemia_status == "anaemic"), .x)
  non_anaemic_stats <- get_counts(df %>% filter(anaemia_status == "non-anaemic"), .x)
  
  tibble(
    Definition = .y,
    All_n = all_stats$numerator,
    All_denom = all_stats$denominator,
    All_pct = all_stats$percentage,
    Anaemic_n = anaemic_stats$numerator,
    Anaemic_denom = anaemic_stats$denominator,
    Anaemic_pct = anaemic_stats$percentage,
    NonAnaemic_n = non_anaemic_stats$numerator,
    NonAnaemic_denom = non_anaemic_stats$denominator,
    NonAnaemic_pct = non_anaemic_stats$percentage
  )
})

cat("TABLE 1: Iron Deficiency Prevalence by Definition\n")
cat(paste(rep("-", 170), collapse = ""))
cat("\n")

for (i in 1:nrow(results_table1)) {
  cat(sprintf(
    "%-40s | All: %3d/%-3d (%.1f%%) | Anaemic: %3d/%-3d (%.1f%%) | Non-anaemic: %3d/%-3d (%.1f%%)\n",
    results_table1$Definition[i],
    results_table1$All_n[i], results_table1$All_denom[i], results_table1$All_pct[i],
    results_table1$Anaemic_n[i], results_table1$Anaemic_denom[i], results_table1$Anaemic_pct[i],
    results_table1$NonAnaemic_n[i], results_table1$NonAnaemic_denom[i], results_table1$NonAnaemic_pct[i]
  ))
}

cat("\n\n")

# ============================================================================
# RESULTS TABLE 2: MARKER AGREEMENT STATISTICS
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nRESULTS TABLE 2: IRON MARKER AGREEMENT STATISTICS (ANAEMIC WOMEN)\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

df_an <- df %>%
  filter(anaemia_status == "anaemic")

results_table2 <- tibble(
  Marker_Pair = character(),
  Both_Positive = integer(),
  Marker1_Only = integer(),
  Marker2_Only = integer(),
  Neither = integer(),
  Pct_Agreement = numeric(),
  Pct_Marker1_Only = numeric(),
  Pct_Marker2_Only = numeric(),
  Pct_Neither = numeric(),
  Denominator = integer()
)

# Ferritin vs sTfR
tmp <- df_an %>%
  filter(!is.na(ferritin_lt30_uncorr), !is.na(stfr_gte155))
den <- nrow(tmp)
both <- sum(tmp$ferritin_lt30_uncorr == 1 & tmp$stfr_gte155 == 1)
f_only <- sum(tmp$ferritin_lt30_uncorr == 1 & tmp$stfr_gte155 == 0)
s_only <- sum(tmp$ferritin_lt30_uncorr == 0 & tmp$stfr_gte155 == 1)
neither <- sum(tmp$ferritin_lt30_uncorr == 0 & tmp$stfr_gte155 == 0)

results_table2 <- bind_rows(results_table2, tibble(
  Marker_Pair = "Ferritin <30 vs sTfR ≥1.55",
  Both_Positive = both,
  Marker1_Only = f_only,
  Marker2_Only = s_only,
  Neither = neither,
  Pct_Agreement = round((both + neither) / den * 100, 1),
  Pct_Marker1_Only = round(f_only / den * 100, 1),
  Pct_Marker2_Only = round(s_only / den * 100, 1),
  Pct_Neither = round(neither / den * 100, 1),
  Denominator = den
))

# Ferritin vs Ferritin Index
tmp <- df_an %>%
  filter(!is.na(ferritin_lt30_uncorr), !is.na(ferritin_index_gte103))
den <- nrow(tmp)
both <- sum(tmp$ferritin_lt30_uncorr == 1 & tmp$ferritin_index_gte103 == 1)
f_only <- sum(tmp$ferritin_lt30_uncorr == 1 & tmp$ferritin_index_gte103 == 0)
fi_only <- sum(tmp$ferritin_lt30_uncorr == 0 & tmp$ferritin_index_gte103 == 1)
neither <- sum(tmp$ferritin_lt30_uncorr == 0 & tmp$ferritin_index_gte103 == 0)

results_table2 <- bind_rows(results_table2, tibble(
  Marker_Pair = "Ferritin <30 vs Ferritin Index ≥1.03",
  Both_Positive = both,
  Marker1_Only = f_only,
  Marker2_Only = fi_only,
  Neither = neither,
  Pct_Agreement = round((both + neither) / den * 100, 1),
  Pct_Marker1_Only = round(f_only / den * 100, 1),
  Pct_Marker2_Only = round(fi_only / den * 100, 1),
  Pct_Neither = round(neither / den * 100, 1),
  Denominator = den
))

# sTfR vs Ferritin Index
tmp <- df_an %>%
  filter(!is.na(stfr_gte155), !is.na(ferritin_index_gte103))
den <- nrow(tmp)
both <- sum(tmp$stfr_gte155 == 1 & tmp$ferritin_index_gte103 == 1)
s_only <- sum(tmp$stfr_gte155 == 1 & tmp$ferritin_index_gte103 == 0)
fi_only <- sum(tmp$stfr_gte155 == 0 & tmp$ferritin_index_gte103 == 1)
neither <- sum(tmp$stfr_gte155 == 0 & tmp$ferritin_index_gte103 == 0)

results_table2 <- bind_rows(results_table2, tibble(
  Marker_Pair = "sTfR ≥1.55 vs Ferritin Index ≥1.03",
  Both_Positive = both,
  Marker1_Only = s_only,
  Marker2_Only = fi_only,
  Neither = neither,
  Pct_Agreement = round((both + neither) / den * 100, 1),
  Pct_Marker1_Only = round(s_only / den * 100, 1),
  Pct_Marker2_Only = round(fi_only / den * 100, 1),
  Pct_Neither = round(neither / den * 100, 1),
  Denominator = den
))

cat("TABLE 2: Iron Marker Agreement (Anaemic Women)\n")
cat(paste(rep("-", 150), collapse = ""))
cat("\n")
cat(sprintf("%-40s | Denom | Both+ | M1 Only | M2 Only | Neither | %% Agree\n", "Marker Pair"))
cat(paste(rep("-", 150), collapse = ""))
cat("\n")

for (i in 1:nrow(results_table2)) {
  cat(sprintf(
    "%-40s | %5d | %4d  | %4d (%.1f%%) | %4d (%.1f%%) | %4d (%.1f%%) | %.1f%%\n",
    results_table2$Marker_Pair[i],
    results_table2$Denominator[i],
    results_table2$Both_Positive[i],
    results_table2$Marker1_Only[i], results_table2$Pct_Marker1_Only[i],
    results_table2$Marker2_Only[i], results_table2$Pct_Marker2_Only[i],
    results_table2$Neither[i], results_table2$Pct_Neither[i],
    results_table2$Pct_Agreement[i]
  ))
}

cat("\n\n")

# ============================================================================
# RESULTS TABLE 3: COMBINATION ANALYSIS
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nRESULTS TABLE 3: IRON DEFICIENCY COMBINATION ANALYSIS (ANAEMIC WOMEN)\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")

combo_data <- df_an %>%
  filter(
    !is.na(ferritin_lt30_uncorr),
    !is.na(stfr_gte155),
    !is.na(ferritin_index_gte103)
  ) %>%
  mutate(
    combo = case_when(
      ferritin_lt30_uncorr == 1 & stfr_gte155 == 1 & ferritin_index_gte103 == 1 ~ "All Three Markers",
      ferritin_lt30_uncorr == 1 & stfr_gte155 == 1 & ferritin_index_gte103 == 0 ~ "Ferritin + sTfR",
      ferritin_lt30_uncorr == 1 & stfr_gte155 == 0 & ferritin_index_gte103 == 1 ~ "Ferritin + FI",
      ferritin_lt30_uncorr == 0 & stfr_gte155 == 1 & ferritin_index_gte103 == 1 ~ "sTfR + FI",
      ferritin_lt30_uncorr == 1 & stfr_gte155 == 0 & ferritin_index_gte103 == 0 ~ "Ferritin Only",
      ferritin_lt30_uncorr == 0 & stfr_gte155 == 1 & ferritin_index_gte103 == 0 ~ "sTfR Only",
      ferritin_lt30_uncorr == 0 & stfr_gte155 == 0 & ferritin_index_gte103 == 1 ~ "FI Only",
      ferritin_lt30_uncorr == 0 & stfr_gte155 == 0 & ferritin_index_gte103 == 0 ~ "None"
    )
  ) %>%
  count(combo, name = "n") %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    combo = factor(combo, levels = c(
      "All Three Markers", "Ferritin + sTfR", "Ferritin + FI", "sTfR + FI",
      "Ferritin Only", "sTfR Only", "FI Only", "None"
    ))
  ) %>%
  arrange(combo)

combo_den <- sum(combo_data$n)

cat("TABLE 3: Combinations of Iron Deficiency Markers (Anaemic Women)\n")
cat(paste(rep("-", 80), collapse = ""))
cat("\n")
cat(sprintf("Denominator with complete data for all 3 markers: %d\n", combo_den))
cat(sprintf("%-30s | Count | Percentage\n", "Combination"))
cat(paste(rep("-", 80), collapse = ""))
cat("\n")

for (i in 1:nrow(combo_data)) {
  cat(sprintf(
    "%-30s | %4d  | %.1f%%\n",
    as.character(combo_data$combo[i]),
    combo_data$n[i],
    combo_data$pct[i]
  ))
}

cat("\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat(paste(rep("=", 100), collapse = ""))
cat("\nANALYSIS COMPLETE!\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n")
cat("FIGURES CREATED:\n")
cat("  ✓ Figure_1_Iron_Deficiency_Cascade.png\n")
cat("  ✓ Figure_2_Iron_Status_Scatter_Anaemic.png\n\n")
cat("RESULTS TABLES PRINTED ABOVE:\n")
cat("  ✓ Table 1: Iron Deficiency Prevalence by Definition\n")
cat("  ✓ Table 2: Iron Marker Agreement Statistics\n")
cat("  ✓ Table 3: Iron Deficiency Combination Analysis\n\n")
cat(paste(rep("=", 100), collapse = ""))
cat("\n\n")


##STFR AND FERRITIN AGREEMENT STRATIFIED BY CRP 
## sTfR and ferritin agreement stratified by CRP

df_an_crp <- df %>%
  filter(anaemia_status == "anaemic") %>%
  mutate(
    crp_cat = case_when(
      !is.na(aiims_crp) & aiims_crp > 5 ~ ">5 mg/L",
      !is.na(aiims_crp) & aiims_crp <= 5 ~ "≤5 mg/L",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(crp_cat),
    !is.na(ferritin_lt30_uncorr),
    !is.na(stfr_gte155)
  )

agreement_table_crp <- df_an_crp %>%
  group_by(crp_cat) %>%
  summarise(
    both_pos = sum(ferritin_lt30_uncorr == 1 & stfr_gte155 == 1),
    ferritin_only = sum(ferritin_lt30_uncorr == 1 & stfr_gte155 == 0),
    stfr_only = sum(ferritin_lt30_uncorr == 0 & stfr_gte155 == 1),
    neither = sum(ferritin_lt30_uncorr == 0 & stfr_gte155 == 0),
    total = n(),
    pct_both_pos = round(both_pos / total * 100, 1),
    pct_ferritin_only = round(ferritin_only / total * 100, 1),
    pct_stfr_only = round(stfr_only / total * 100, 1),
    pct_neither = round(neither / total * 100, 1),
    pct_agreement = round((both_pos + neither) / total * 100, 1)
  )

agreement_table_crp