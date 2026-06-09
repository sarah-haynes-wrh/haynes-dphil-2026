# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_4_7_forest_plot_within_group_changes.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# =========================
# PANDA: Forest plot of adjusted within-group change (original units)
# =========================

# ---- Packages ----
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(performance)
  library(ggplot2)
})

# ---- 1) Read Excel & map columns robustly ----
xls_path <- file.path("data", "CRP_Analysis_Master_Data.xlsx")
df_raw <- read_excel(xls_path)

find_col <- function(nms, pattern, label){
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop(sprintf("Column for '%s' not found.\nPattern: %s\nAvailable: %s",
                                 label, pattern, paste(nms, collapse = ", ")), call. = FALSE)
  hit[1]
}

nms <- names(df_raw)

col_id   <- find_col(nms, "^(patid|id)\\b",        "Participant ID")
col_grp  <- find_col(nms, "^group\\b",             "Group")
col_time <- find_col(nms, "^time\\b",              "Time")
col_crp  <- find_col(nms, "^crp\\b",               "CRP")
col_stfr <- find_col(nms, "^(stfr|s\\s*tfr)\\b",   "sTfR")
col_hep  <- find_col(nms, "^hepcidin\\b",          "Hepcidin")
col_fer  <- find_col(nms, "^ferritin\\b",          "Ferritin")
col_tsat <- find_col(nms, "^tsat\\b",              "TSAT")

df <- tibble::tibble(
  PATID    = df_raw[[col_id]],
  Group    = as.character(df_raw[[col_grp]]),
  Time     = as.character(df_raw[[col_time]]),
  CRP      = suppressWarnings(as.numeric(df_raw[[col_crp]])),
  stfr     = suppressWarnings(as.numeric(df_raw[[col_stfr]])),
  Hepcidin = suppressWarnings(as.numeric(df_raw[[col_hep]])),
  Ferritin = suppressWarnings(as.numeric(df_raw[[col_fer]])),
  TSAT     = suppressWarnings(as.numeric(df_raw[[col_tsat]]))
) %>%
  mutate(
    Group = factor(Group, levels = c("Daily","AltDaily","3xWeekly")),
    Time  = factor(Time,  levels = c("Baseline","FollowUp"))
  )

# ---- 2) Transforms used in models (log10) ----
df <- df %>%
  mutate(
    log10_CRP      = log10(CRP + 0.1),
    log10_stfr     = log10(stfr),
    log10_Hepcidin = log10(Hepcidin),
    log10_Ferritin = log10(Ferritin),
    log10_TSAT     = log10(TSAT)
  )

# ---- 3) Fit CRP-adjusted LMMs (log10 scale) ----
run_biomarker_model <- function(df, log_var){
  form <- as.formula(paste0(log_var, " ~ Group * Time + log10_CRP + (1|PATID)"))
  model <- lmer(form, data = df)
  # Optional diagnostics
  cat("\n==== Model diagnostics for", log_var, "====\n")
  print(check_collinearity(model))
  print(check_normality(model))
  print(check_heteroscedasticity(model))
  model
}

mod_stfr     <- run_biomarker_model(df, "log10_stfr")
mod_hepcidin <- run_biomarker_model(df, "log10_Hepcidin")
mod_ferritin <- run_biomarker_model(df, "log10_Ferritin")
mod_tsat     <- run_biomarker_model(df, "log10_TSAT")

# ---- 4) Extract within-group change on ORIGINAL scale (key) ----
get_within_change_bt <- function(fit, biomarker_label, data) {
  # EMMs on model scale (log10)
  emm <- emmeans(fit, ~ Time | Group, data = data, cov.reduce = mean)
  
  # Tell emmeans the transform so it knows the inverse (10^x) and SEs
  attr(emm, "misc")$tran <- make.tran("log10")
  
  # Regrid to response (original units)
  emm_resp <- regrid(emm, transform = "response")
  
  # Within-group contrast on response scale: FollowUp − Baseline
  con_resp <- contrast(emm_resp,
                       method = list("FollowUp - Baseline" = c(-1, 1)),
                       by = "Group")
  
  as.data.frame(summary(con_resp, infer = TRUE)) |>
    dplyr::transmute(
      Biomarker = biomarker_label,
      Group     = as.character(Group),
      Change    = estimate,    # ORIGINAL units (µg/L, %, mg/L, ng/mL)
      Lower_CI  = lower.CL,
      Upper_CI  = upper.CL,
      p_value   = p.value
    )
}

changes_df <- dplyr::bind_rows(
  get_within_change_bt(mod_ferritin, "Ferritin",  df),
  get_within_change_bt(mod_tsat,     "TSAT",      df),
  get_within_change_bt(mod_stfr,     "sTfR",      df),
  get_within_change_bt(mod_hepcidin, "Hepcidin",  df)
)

# ---- 5) FINAL order + colours ----
# Legend order you want:
changes_df <- changes_df %>%
  mutate(
    Group     = factor(Group, levels = c("Daily","AltDaily","3xWeekly")),
    Biomarker = factor(Biomarker, levels = c("Ferritin","TSAT","sTfR","Hepcidin"))
  )

# y-axis order to display (top->bottom). To have Daily at the BOTTOM, set limits as below:
y_limits <- c("3xWeekly","AltDaily","Daily")

# ---- 6) Forest plot (95% CI; original units) ----
pd <- position_dodge(width = 0.5)

unit_labels <- c(
  Ferritin = "Change in Ferritin (µg/L)",
  TSAT     = "Change in TSAT (%)",
  sTfR     = "Change in sTfR (mg/L)",
  Hepcidin = "Change in Hepcidin (ng/mL)"
)

p_forest <- ggplot(changes_df, aes(x = Change, y = Group, colour = Group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI),
                 height = 0.28, size = 0.9, position = pd) +
  geom_point(size = 3, position = pd) +
  # control y-axis display order (Daily at bottom)
  scale_y_discrete(limits = y_limits) +
  facet_wrap(~ Biomarker, ncol = 1, scales = "free_x",
             labeller = labeller(Biomarker = unit_labels)) +
  scale_colour_manual(
    values = c(
      "Daily"     = "#5E4A94",  # purple (Daily)
      "AltDaily"  = "#0096FF",  # bright electric blue (AltDaily)
      "3xWeekly"  = "#00A087"   # teal (3xWeekly)
    ),
    breaks = c("Daily","AltDaily","3xWeekly"),  # legend order
    limits = c("Daily","AltDaily","3xWeekly"),  # enforce legend order
    labels = c("Daily","Alt Daily","3× Weekly")
  ) +
  labs(
    title = "Adjusted within-group change (Follow-up − Baseline)",
    subtitle = "Back-transformed from log₁₀ LMM (make.tran('log10') + regrid('response')); CRP-adjusted",
    x = NULL, y = "Supplementation regimen", colour = "Regimen"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_forest)
ggsave("forest_within_change_original_units.png", p_forest, width = 7.0, height = 9.0, dpi = 300)
