# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_4_screening_heatmap.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)  
library(dplyr)

# Create a dataframe with UK regions and screening categories
screening_data <- data.frame(
  Region = c("Greater London", "South East", "North East", "West Midlands", "East",
             "North West", "Yorkshire and the Humber", "East Midlands", "South West",
             "Northern Ireland", "Eastern", "East Wales", "West Wales and the Valleys", "North Eastern"),
  Hb_32_36 = c(1, 0, 1, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0),  # Additional Hb Screening
  Ferritin_Both = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0),   # Additional Ferritin Screening 
  Hb_Ferritin_Combo = c(0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1)  # Additional Hb and Ferritin
)

# Load UK map data
uk_map <- ne_states(country = "United Kingdom", returnclass = "sf")

# Verify region names in map data
print(unique(uk_map$region))

# Merge screening data with UK map
uk_map <- left_join(uk_map, screening_data, by = c("region" = "Region"))

# Assign missing screening values (NAs) to a new category "No Guidelines Collected"
uk_map <- uk_map %>%
  mutate(Screening_Type = case_when(
    !is.na(Hb_32_36) & Hb_32_36 > 0 ~ "Additional Hb",
    !is.na(Ferritin_Both) & Ferritin_Both > 0 ~ "Additional Ferritin",
    !is.na(Hb_Ferritin_Combo) & Hb_Ferritin_Combo > 0 ~ "Additional Hb and Ferritin",
    is.na(Hb_32_36) & is.na(Ferritin_Both) & is.na(Hb_Ferritin_Combo) ~ "No Guidelines Collected",
    TRUE ~ "No Additional Screening"
  ))

# Define colorblind-friendly palette (fixing incorrect key)
palette_colors <- c(
  "Additional Hb" = "#F0F921FF",   # Yellow/Green
  "Additional Ferritin" = "#CC4678FF",  # Pink/Red
  "Additional Hb and Ferritin" = "#0D0887FF",  # Blue/Purple
  "No Additional Screening" = "#000000",  # Black for missing data regions
  "No Guidelines Collected" = "#bdbdbd"  # Light Gray 
)

# Convert Screening_Type to a factor with correct levels
uk_map$Screening_Type <- factor(uk_map$Screening_Type, 
                                levels = c("Additional Hb", "Additional Ferritin", 
                                           "Additional Hb and Ferritin", "No Additional Screening", 
                                           "No Guidelines Collected"))

# Plot the heatmap
ggplot(data = uk_map) +
  geom_sf(aes(fill = Screening_Type), color = "white") +
  scale_fill_manual(values = palette_colors, name = "Screening Type") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    legend.key.height = unit(2, "cm"),
    legend.position = "left",
    plot.title = element_text(hjust = 1, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0)
  ) +
  labs(title = "Additional Routine Anaemia Screening in Pregnancy across UK Regions")

# Save the heatmap
ggsave("UK_Screening_Heatmap_Black_NoData.png", width = 10, height = 6, dpi = 300)
