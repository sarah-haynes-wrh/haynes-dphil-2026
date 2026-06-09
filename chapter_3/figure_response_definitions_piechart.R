# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_response_definitions_piechart.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)

# Create dataset
response_data <- data.frame(
  Definition = c("Crude Hb Increase", "Hb increase by 10-20g/L over 4 weeks", 
                 "Hb Increase by 10ug/L per week", "Hb Increase by 10ug/L over 2-4 weeks",
                 "Hb increase of 20g/L over 4-6 weeks", "Increase in RDW", "Not Mentioned"),
  Guidelines = c(17, 1, 3, 4, 3, 1, 18)
)

# Define colors (Plasma Viridis for data, Gray for "Not Mentioned")
custom_colors <- setNames(c(viridis(6, option = "plasma"), "gray50"), response_data$Definition)

# Create pie chart with labels
ggplot(response_data, aes(x = "", y = Guidelines, fill = Definition)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart in polar form
  coord_polar(theta = "y") +  # Converts to a pie chart
  geom_text(aes(label = Guidelines), position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Adds labels inside slices
  scale_fill_manual(values = custom_colors, name = "Definition of Response") +  
  labs(
    title = "Proportions of Response Definitions to Oral Iron"
  ) +
  theme_void() +  # Removes background, axes, and grid lines
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )

# Save the pie chart
ggsave("Response_Definition_PieChart_WithLabels.png", width = 8, height = 8, dpi = 300)

  )
