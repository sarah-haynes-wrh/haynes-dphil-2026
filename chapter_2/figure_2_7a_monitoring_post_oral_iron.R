# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_7a_monitoring_post_oral_iron.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Create dataset for post-oral iron monitoring
monitoring_oral <- data.frame(
  Test = c("Haemoglobin", "Haemoglobin + Ferritin", "Hb + Ferritin + Folate + B12", "Haemoglobin + RDW", "Not Mentioned"),
  Total = c(33, 10, 1, 1, 2),
  X2wks = c(4, 4, 0, 0, 0),
  X2_3wks = c(7, 1, 0, 1, 0),
  X2_4wks = c(14, 2, 0, 0, 0),
  X3_4wks = c(2, 1, 1, 0, 0),
  X4wks = c(6, 1, 0, 0, 0),
  X6wks = c(0, 1, 0, 0, 0),
  Not_Mentioned = c(0, 0, 0, 0, 2)  # Ensuring correct name
)

# **Manually rename columns to prevent "X2.3" issue**
colnames(monitoring_oral) <- c("Test", "Total", "2 Weeks", "2-3 Weeks", "2-4 Weeks", "3-4 Weeks", "4 Weeks", "6 Weeks", "Not Mentioned")

# Reshape data for ggplot
monitoring_oral_long <- pivot_longer(monitoring_oral, cols = -c(Test, Total), 
                                     names_to = "Timing", values_to = "Guidelines")

# **Manually define correct x-axis labels**
correct_labels <- c("2 Weeks" = "2 Weeks", "2-3 Weeks" = "2-3 Weeks", "2-4 Weeks" = "2-4 Weeks",
                    "3-4 Weeks" = "3-4 Weeks", "4 Weeks" = "4 Weeks", "6 Weeks" = "6 Weeks")

# Define colors (Viridis Plasma + Gray for "Not Mentioned")
custom_colors <- c(viridis(4, option = "plasma"), "gray50")  # Gray for "Not Mentioned"

# Create stacked bar chart
ggplot(monitoring_oral_long, aes(x = Timing, y = Guidelines, fill = Test)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Test Used") +
  scale_x_discrete(labels = correct_labels) +  # **Fix x-axis label formatting**
  labs(
    title = "Monitoring Post-Oral Iron by Test Type and Timing",
    x = "Time Interval (Weeks)",
    y = "Number of Guidelines"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )

# Save the plot
ggsave("Monitoring_Post_Oral_Iron_StackedBar_Fixed.png", width = 10, height = 6, dpi = 300)

# Print the count of guidelines that did not mention monitoring (for reference)
print(paste("Number of Guidelines that did not mention monitoring:", monitoring_oral$`Not Mentioned`[5]))
