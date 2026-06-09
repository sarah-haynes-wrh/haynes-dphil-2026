# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_6_iv_iron_indications.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)  # Use Viridis colorblind-friendly palette

# Create dataset with categories
oral_iron_data <- data.frame(
  Indication = c("Hb <70g/L", "Hb <80g/L", "Hb <85g/L", "Hb <90g/L", "Hb <100g/L", "Hb <105g/L", "Hb <110g/L",
                 "Serum Ferritin <10ug/L", "Serum Ferritin <15ug/L", "Serum Ferritin <30ug/L", "Serum Ferritin <50ug/L",
                 "Hb <90g/L at >34 weeks", "Urgent Correction", "Anaemic at >34 weeks", "Hb <100g/L in 3rd tri", "Hb <95g/L approaching term",
                 "Non-Compliance", "Intolerance", "Failure to Respond", "Malabsorption", "Significantly Symptomatic"),
  Guidelines = c(4, 10, 3, 4, 1, 3, 2, 1, 1, 3, 1, 3, 17, 4, 4, 1, 19, 39, 27, 24, 3),
  Category = c("Haemoglobin Threshold", "Haemoglobin Threshold", "Haemoglobin Threshold", "Haemoglobin Threshold", 
               "Haemoglobin Threshold", "Haemoglobin Threshold", "Haemoglobin Threshold",
               "Ferritin Threshold", "Ferritin Threshold", "Ferritin Threshold", "Ferritin Threshold",
               "Gestational Threshold", "Gestational Threshold", "Gestational Threshold", "Gestational Threshold", "Gestational Threshold",
               "Other", "Other", "Other", "Other", "Other")
)

# Generate Plasma color palette dynamically based on the number of categories
num_categories <- length(unique(oral_iron_data$Category))
plasma_colors <- viridis(num_categories, option = "plasma")

# Create a named vector for category colors
category_colors <- setNames(plasma_colors, unique(oral_iron_data$Category))

# Create horizontal bar chart with Plasma color-coded categories and correct sorting
ggplot(oral_iron_data, aes(x = Guidelines, y = reorder(Indication, Guidelines), fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = category_colors, name = "Indication Category") +  
  labs(
    title = "Indications for IV Iron Across Guidelines",
    x = "Number of Guidelines",
    y = "Indication"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

