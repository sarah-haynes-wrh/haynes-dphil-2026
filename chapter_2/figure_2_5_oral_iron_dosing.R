# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_5_oral_iron_dosing.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)  # Load viridis for colorblind-friendly colors

# Create dataset
iron_dose_data <- data.frame(
  Dose = c("65mg", "40-80mg", "35-65mg", "30-100mg", "100mg", "100-200mg", "Not Mentioned"),
  Daily = c(24, 8, 3, 1, 1, 7, 2),
  Alternate_Daily = c(0, 1, 0, 0, 0, 0, 0)
)

# Reshape data for stacked bar chart
iron_dose_long <- pivot_longer(iron_dose_data, cols = -Dose, names_to = "Frequency", values_to = "Guidelines")

# Create stacked bar chart with custom colors
ggplot(iron_dose_long, aes(x = Dose, y = Guidelines, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Daily" = "#0D0887FF",# Blue 
                               "Alternate_Daily" ="#CC4678FF")) +   # Orange   
  labs(
    title = "Iron Dosing Recommendations in NHS Guidelines",
    x = "Elemental Iron Dose",
    y = "Number of Guidelines",
    fill = "Dosing Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )