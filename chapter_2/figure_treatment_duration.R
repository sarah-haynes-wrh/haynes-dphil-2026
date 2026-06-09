# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_treatment_duration.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)       # Colorblind-friendly palette
library(RColorBrewer)  # Scientific color schemes

# Create dataset
treatment_data <- data.frame(
  Category = c("Duration Only", "Duration Only", 
               "Duration Once Hb Normalised", "Duration Once Hb Normalised", "Duration Once Hb Normalised", "Duration Once Hb Normalised",
               "Duration in Relation to Postpartum", "Duration in Relation to Postpartum", "Duration in Relation to Postpartum", "Duration in Relation to Postpartum"),
  Treatment_Duration = c("3 Months", "1 Month",
                         "3 Months AND 6 Weeks Postpartum", "3 Months", "3-6 Months", "1 Month",
                         "3 Months AND 6 Weeks Postpartum", "3 Months OR 6 Weeks Postpartum", "3 Months Postpartum", "6 Weeks Postpartum"),
  Guidelines = c(2, 1, 12, 3, 1, 1, 4, 4, 1, 2)
)

# CHOOSE ONE OF THE COLOR OPTIONS BELOW:

# 1️⃣ Viridis Palette (Best for colorblind users)
color_palette <- viridis::viridis(3, option = "plasma")  # Try "viridis", "magma", "inferno", "cividis"

# 2️⃣ Soft Pastel Palette (Manually chosen)
 #color_palette <- c("Duration Only" = "#F4A582",   # Soft Orange
                #"Duration Once Hb Normalised" = "#92C5DE",  # Soft Blue
                 #"Duration in Relation to Postpartum" = "#B2182B")  # Deep Red

# 3️⃣ RColorBrewer Palette (Scientific color schemes)
#color_palette <- brewer.pal(3, "Set2")  # Try "Set1", "Set2", "Pastel1", "Dark2"

# Create stacked bar chart with the chosen color palette
ggplot(treatment_data, aes(x = Treatment_Duration, y = Guidelines, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette) +  
  labs(
    title = "Iron Treatment Duration Recommendations Across Guidelines",
    x = "Treatment Duration",
    y = "Number of Guidelines",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = c(0.95, 0.95),        # Legend near top right
    legend.justification = c("right", "top"), # Anchor legend's top right corner
    axis.text = element_text(size = 10),  # Increase font size for axis text
    axis.title = element_text(size = 16, face = "bold"),  # Increase font size for axis labels
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.background = element_blank()   # Remove background
  )

