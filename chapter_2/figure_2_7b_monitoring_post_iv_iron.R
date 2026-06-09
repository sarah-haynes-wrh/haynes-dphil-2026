# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_7b_monitoring_post_iv_iron.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Create dataset for IV iron monitoring
monitoring_iv <- data.frame(
  Test = c("Haemoglobin", "Haemoglobin + Ferritin", "Hb + Ferritin + Reticulocyte Count", "Not Mentioned"),
  Total = c(16, 8, 2, 21),
  X10_Days = c(0, 1, 0, 0),
  X2_Weeks = c(5, 2, 0, 0),
  X2_3_Weeks = c(2, 0, 0, 0),
  X3_Weeks = c(2, 1, 0, 0),
  X2_4_Weeks = c(2, 1, 0, 0),
  X4_Weeks = c(4, 3, 2, 0),
  X4_6_Weeks = c(1, 0, 0, 0),
  Not_Mentioned = c(0, 0, 0, 21)  # Including "Not Mentioned"
)

# **Manually rename the columns to ensure proper x-axis formatting**
colnames(monitoring_iv) <- c("Test", "Total", "10 Days", "2 Weeks", "2-3 Weeks", "3 Weeks", 
                             "2-4 Weeks", "4 Weeks", "4-6 Weeks", "Not Mentioned")

# Reshape data for ggplot (including "Not Mentioned")
monitoring_iv_long <- pivot_longer(monitoring_iv, cols = -c(Test, Total), 
                                   names_to = "Timing", values_to = "Guidelines")

# **Manually define correct x-axis labels**
correct_labels <- c("10 Days" = "10 Days", "2 Weeks" = "2 Weeks", "2-3 Weeks" = "2-3 Weeks",
                    "3 Weeks" = "3 Weeks", "2-4 Weeks" = "2-4 Weeks", "4 Weeks" = "4 Weeks",
                    "4-6 Weeks" = "4-6 Weeks")

# Define custom colors (Viridis Plasma + Gray for "Not Mentioned")
custom_colors <- c(viridis(3, option = "plasma"), "gray50")  # Gray for "Not Mentioned"

# Create stacked bar chart
ggplot(monitoring_iv_long, aes(x = Timing, y = Guidelines, fill = Test)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Test Used") +  
  scale_x_discrete(labels = correct_labels) +  # **Fix x-axis label formatting**
  labs(
    title = "Monitoring Post-IV Iron by Test Type and Timing",
    x = "Time Interval (Weeks)",
    y = "Number of Guidelines"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )

# Save the plot
ggsave("Monitoring_Post_IV_Iron_StackedBar_Fixed.png", width = 10, height = 6, dpi = 300)

# Print the count of guidelines that did not mention monitoring (for reference)
print(paste("Number of Guidelines that did not mention monitoring:", monitoring_iv$`Not Mentioned`[4]))