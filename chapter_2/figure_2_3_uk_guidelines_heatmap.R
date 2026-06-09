# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Script: figure_2_3_uk_guidelines_heatmap.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Create a dataframe with UK regions and guideline counts
guideline_data <- data.frame(
  Region = c("Greater London", "South East", "North East", "West Midlands", "East",
             "North West", "Yorkshire and the Humber", "East Midlands", "South West",
             "Northern Ireland", "Eastern","East Wales", "West Wales and the Valleys"),
  Guidelines = c(4, 8, 4, 3, 2, 4, 6, 3, 2, 1, 3, 2, 2)
)

# Load UK map data
uk_map <- ne_states(country = "United Kingdom", returnclass = "sf")

# Check available region names in the UK map (to verify correct names)
print(unique(uk_map$region))

# Merge guideline data with the UK map
uk_map <- merge(uk_map, guideline_data, by.x = "region", by.y = "Region", all.x = TRUE)

# Check if the merge was successful
print(colnames(uk_map))  # "Guidelines" should appear
print(table(is.na(uk_map$Guidelines)))  # Should return mostly FALSE

# Replace NA values with 0 for missing regions
uk_map$Guidelines[is.na(uk_map$Guidelines)] <- 0


# Create a heatmap using ggplot2
ggplot(data = uk_map) +
  geom_sf(aes(fill = Guidelines), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Guidelines") +
  theme_minimal() +
  labs(title = "Heatmap of Guidelines per UK Region",
       subtitle = "Based on NHS and Health Trust Data",
       caption = "Source: NHS Guidelines Audit")


# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)  # New for improved color scales


# Create the heatmap using clearer colors (without lat/lon grid lines)
ggplot(data = uk_map) +
  geom_sf(aes(fill = Guidelines), color = "white") +
  scale_fill_viridis(option = "plasma", name = "Guidelines") +  # Colorblind-friendly color scheme
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_blank(),         # Remove axis labels
    axis.ticks = element_blank(),        # Remove axis ticks
    axis.title = element_blank(),        # Remove axis titles
    panel.background = element_blank()   # Ensure no background lines
  ) +
  labs(title = "Heatmap of Guidelines per UK Region",
       subtitle = "Based on NHS and Health Trust Data",
       caption = "Source: NHS Guidelines Audit")
