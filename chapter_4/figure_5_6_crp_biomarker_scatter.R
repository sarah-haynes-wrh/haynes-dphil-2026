# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_5_6_crp_biomarker_scatter.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
#install.packages("patchwork")

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# File path
excel_file_path <- file.path("data", "CRP_Analysis_Master_Data.xlsx")

# Import data
df <- read_excel(excel_file_path)

# Check column names
colnames(df)

# Create log10-transformed variables
df <- df %>%
  mutate(
    log_crp = log10(CRP),
    log_ferritin = log10(ferritin),
    log_tsat = log10(tsat),
    log_stfr = log10(stfr)
  )

# Function to calculate Spearman correlation by timepoint
spearman_by_time <- function(data, x, y) {
  data %>%
    group_by(time) %>%
    summarise(
      n = sum(complete.cases(.data[[x]], .data[[y]])),
      rho = cor(.data[[x]], .data[[y]], method = "spearman", use = "complete.obs"),
      p_value = cor.test(.data[[x]], .data[[y]], method = "spearman")$p.value,
      .groups = "drop"
    )
}

# Run correlations
crp_ferritin <- spearman_by_time(df, "log_crp", "log_ferritin") %>%
  mutate(biomarker = "Ferritin")

crp_tsat <- spearman_by_time(df, "log_crp", "log_tsat") %>%
  mutate(biomarker = "TSAT")

crp_stfr <- spearman_by_time(df, "log_crp", "log_stfr") %>%
  mutate(biomarker = "sTfR")

# Combine into one results table
crp_biomarker_results <- bind_rows(crp_ferritin, crp_tsat, crp_stfr) %>%
  select(biomarker, time, n, rho, p_value)

print(crp_biomarker_results)

# Optional formatted results table
crp_biomarker_results %>%
  mutate(
    rho = round(rho, 3),
    p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 3))
    )
  )

# Common plot theme
common_theme <- theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

# Ferritin plot
p_ferritin <- ggplot(df, aes(x = CRP, y = ferritin)) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~time, ncol = 2) +
  scale_x_log10(name = "CRP (mg/L)") +
  scale_y_log10(name = "Ferritin (µg/L)") +
  ggtitle("Ferritin") +
  common_theme

# sTfR plot
p_stfr <- ggplot(df, aes(x = CRP, y = stfr)) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~time, ncol = 2) +
  scale_x_log10(name = "CRP (mg/L)") +
  scale_y_log10(name = "sTfR (mg/L)") +
  ggtitle("sTfR") +
  common_theme

# TSAT plot
p_tsat <- ggplot(df, aes(x = CRP, y = tsat)) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~time, ncol = 2) +
  scale_x_log10(name = "CRP (mg/L)") +
  scale_y_log10(name = "TSAT (%)") +
  ggtitle("TSAT") +
  common_theme

# Stack vertically on one page
combined_plot <- p_ferritin / p_stfr / p_tsat +
  plot_annotation(
    title = "Associations between CRP and iron biomarkers by timepoint"
  )

# Display in RStudio
combined_plot

# Save as A4 PDF (portrait)
ggsave(
  filename = "CRP_Biomarker_Associations_A4.pdf",
  plot = combined_plot,
  width = 8.27,
  height = 11.69,
  units = "in"
)

# Save as A4 PNG (portrait)
ggsave(
  filename = "CRP_Biomarker_Associations_A4.png",
  plot = combined_plot,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 300
)