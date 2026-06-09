# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: analysis_lmm_crp_adjusted_all_biomarkers.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ============================================================
# CRP-adjusted LMMs for PANDA biomarkers
# - Adds: Hb, MCV, MCH, serum_iron, transferrin
# - Robust numeric coercion for Excel-imported text columns
# - Computes:
#     (1) Within-group change (FollowUp - Baseline)
#     (2) Between-group differences in change (difference-in-differences)
# - Removes: between-group differences at FollowUp (timepoint-only contrasts)
# - Keeps results consistent with prior analyses:
#     * Uses Kenward-Roger df for emmeans contrasts
#     * Uses NO multiplicity adjustment for between-group differences in change
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(performance)
  library(readxl)
})

# =========================
# Load Excel
# =========================
df <- read_excel(
  file.path("data", "CRP_Analysis_Master_Data.xlsx")
)

# =========================
# Robust numeric coercion (Excel often imports as character/factor)
# =========================
num_cols <- c(
  "CRP", "stfr", "hepcidin", "ferritin", "tsat",
  "serum_iron", "transferrin", "mcv", "mch",
  # Haemoglobin column (matched case-insensitively: "hb", "Hb", "haemoglobin", etc.)
  "hb", "Hb"
)
num_cols <- intersect(num_cols, names(df))

df <- df %>%
  mutate(
    across(all_of(num_cols), ~ {
      x <- as.character(.x)
      x <- gsub(",", "", x)            # remove thousands separators
      x <- gsub("[^0-9.\\-]", "", x)   # strip units/symbols; keep digits/dot/minus
      suppressWarnings(as.numeric(x))
    })
  )

# =========================
# Prep variables + transforms
# =========================
# Choose the Hb column name that exists
hb_col <- dplyr::case_when(
  "hb" %in% names(df) ~ "hb",
  "Hb" %in% names(df) ~ "Hb",
  TRUE ~ NA_character_
)
if (is.na(hb_col)) {
  stop("Could not find haemoglobin column. Expected 'hb' or 'Hb'.")
}

df <- df %>%
  mutate(
    id    = factor(id),
    group = factor(group),
    time  = factor(time, levels = c("Baseline", "FollowUp")),
    
    # CRP log10 with offset
    log10_CRP = log10(CRP + 0.1),
    
    # Log10 outcomes (offset for robustness)
    log10_stfr       = log10(stfr + 1e-6),
    log10_hepcidin   = log10(hepcidin + 1e-6),
    log10_ferritin   = log10(ferritin + 1e-6),
    log10_tsat       = log10(tsat + 1e-6),
    log10_serum_iron = log10(serum_iron + 1e-6)
    
    # NOTE: Hb is typically modelled untransformed (use run_linear_model)
  )

# =========================
# Ferritin index column name (robust to typo)
# =========================
fi_col <- dplyr::case_when(
  "ferritin_index" %in% names(df) ~ "ferritin_index",
  "ferrtin_index"  %in% names(df) ~ "ferrtin_index",
  TRUE ~ NA_character_
)
if (is.na(fi_col)) {
  stop("Could not find ferritin index column. Expected 'ferritin_index' or 'ferrtin_index'.")
}

# ── Model fitting options ──────────────────────────────────────────────────
# REML = TRUE: Restricted Maximum Likelihood used for parameter estimation.
#   REML is preferred over ML for estimating variance components when the
#   fixed effects structure is the same across compared models.
#   All models here use the same fixed effects, so REML is appropriate.
#
# Kenward-Roger degrees of freedom: used for all emmeans contrasts.
#   KR adjusts for small-sample bias in the F/t distribution for LMMs.
#   Appropriate for this dataset (n ≈ 130, two timepoints, random intercept).
#
# Random slope: not included (random intercept only: 1|id).
#   With only two timepoints per participant, a random slope for time
#   would be exactly confounded with the random intercept and is not
#   identifiable. Random intercept only is the correct structure here.
# ─────────────────────────────────────────────────────────────────────────
emmeans::emm_options(lmer.df = "kenward-roger")

# =========================
# Helper: fit model + print key contrasts (LOG outcomes)
# =========================
run_log_biomarker_model <- function(df, label, log_var) {
  cat("\n\n==============================\n")
  cat("MODEL:", label, "\n")
  cat("==============================\n")
  
  form <- as.formula(paste0(log_var, " ~ group * time + log10_CRP + (1 | id)"))
  
  model <- lmer(form, data = df, REML = TRUE)
  print(summary(model))
  
  emm <- emmeans(model, ~ group * time)
  
  cat("\n==== Within-group change (FollowUp - Baseline) ====\n")
  within_change <- contrast(
    emm,
    method = "revpairwise",
    by = "group",
    simple = "time"
  )
  print(summary(within_change, infer = TRUE))
  
  cat("\n==== Between-group differences in change (interaction contrasts) ====\n")
  diff_in_change <- pairs(within_change, by = NULL, adjust = "none")
  print(summary(diff_in_change, infer = TRUE))
  
  cat("\n==== Model diagnostics ====\n")
  print(check_collinearity(model))
  print(check_normality(model))
  print(check_heteroscedasticity(model))
  
  invisible(model)
}

# =========================
# Helper: fit model + print key contrasts (UNTRANSFORMED outcomes)
# =========================
run_linear_model <- function(df, label, var) {
  cat("\n\n==============================\n")
  cat("MODEL:", label, "\n")
  cat("==============================\n")
  
  form <- as.formula(paste0(var, " ~ group * time + log10_CRP + (1 | id)"))
  
  model <- lmer(form, data = df, REML = TRUE)
  print(summary(model))
  
  emm <- emmeans(model, ~ group * time)
  
  cat("\n==== Within-group change (FollowUp - Baseline) ====\n")
  within_change <- contrast(
    emm,
    method = "revpairwise",
    by = "group",
    simple = "time"
  )
  print(summary(within_change, infer = TRUE))
  
  cat("\n==== Between-group differences in change (interaction contrasts) ====\n")
  diff_in_change <- pairs(within_change, by = NULL, adjust = "none")
  print(summary(diff_in_change, infer = TRUE))
  
  cat("\n==== Model diagnostics ====\n")
  print(check_collinearity(model))
  print(check_normality(model))
  print(check_heteroscedasticity(model))
  
  invisible(model)
}

# =========================
# Run models: log10 outcomes
# =========================
mod_stfr       <- run_log_biomarker_model(df, "sTfR (log10)",       "log10_stfr")
mod_hepcidin   <- run_log_biomarker_model(df, "Hepcidin (log10)",   "log10_hepcidin")
mod_ferritin   <- run_log_biomarker_model(df, "Ferritin (log10)",   "log10_ferritin")
mod_tsat       <- run_log_biomarker_model(df, "TSAT (log10)",       "log10_tsat")
mod_serum_iron <- run_log_biomarker_model(df, "Serum iron (log10)", "log10_serum_iron")

# =========================
# Run models: untransformed outcomes
# =========================
mod_hb            <- run_linear_model(df, "Haemoglobin (untransformed)", hb_col)
mod_ferritin_index <- run_linear_model(df, "Ferritin Index (untransformed)", fi_col)

mod_transferrin    <- run_linear_model(df, "Transferrin (untransformed)", "transferrin")
mod_mcv            <- run_linear_model(df, "MCV (untransformed)", "mcv")
mod_mch            <- run_linear_model(df, "MCH (untransformed)", "mch")

# =========================
# Optional: extract CRP effect for any model (consistent)
# =========================
extract_crp_effect <- function(mod, biomarker){
  fe <- coef(summary(mod)) |> as.data.frame()
  fe$term <- rownames(fe)
  
  ci <- confint(mod, parm = "log10_CRP", method = "Wald")
  
  fe |>
    dplyr::filter(term == "log10_CRP") |>
    dplyr::mutate(
      Biomarker = biomarker,
      LCL = ci[1],
      UCL = ci[2]
    ) |>
    dplyr::select(
      Biomarker,
      Beta = Estimate,
      SE = `Std. Error`,
      LCL,
      UCL,
      p = `Pr(>|t|)`
    )
}

# Examples
extract_crp_effect(mod_serum_iron, "Serum iron")
extract_crp_effect(mod_transferrin, "Transferrin")
extract_crp_effect(mod_mcv, "MCV")
extract_crp_effect(mod_mch, "MCH")
extract_crp_effect(mod_hb, "Haemoglobin")

# EMM tables (examples)
emmeans(mod_serum_iron, ~ group * time, type = "response") |> summary(infer = TRUE)
emmeans(mod_transferrin, ~ group * time) |> summary(infer = TRUE)
emmeans(mod_mcv, ~ group * time) |> summary(infer = TRUE)
emmeans(mod_mch, ~ group * time) |> summary(infer = TRUE)
emmeans(mod_hb, ~ group * time) |> summary(infer = TRUE)
