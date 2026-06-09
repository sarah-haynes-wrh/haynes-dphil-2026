# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_definitions_oral_iron_response.R
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

# **Sort bars from most to least frequent**
response_data <- response_data %>% arrange(desc(Guidelines))

# Define custom colors (Plasma Viridis for data, Gray for "Not Mentioned")
custom_colors <- setNames(c(viridis(7, option = "plasma"), "gray50"), response_data$Definition)

# Create horizontal bar chart
ggplot(response_data, aes(x = reorder(Definition, Guidelines), y = Guidelines, fill = Definition)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # **Flips the chart to horizontal**
  scale_fill_manual(values = custom_colors, name = "Definition of Response") +  
  labs(
    title = "Definitions of Response to Oral Iron",
    x = "Definition",
    y = "Number of Guidelines"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend since labels are clear
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
