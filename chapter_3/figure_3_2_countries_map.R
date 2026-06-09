# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_3_2_countries_map.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Load required packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Study count data
study_counts <- data.frame(
  country = c("India", "Italy","Singapore","Turkey", "Egypt", "South Korea", "United States of America", 
              "Jamaica", "Colombia", "Argentina", "Australia", "Russia", "Sweden", "Switzerland", 
            "Thailand", "Greece", "Bangladesh", "Pakistan", "Malawi"),
  count = c(15, 2, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# Merge study counts into the world map data
world_data <- left_join(world, study_counts, by = c("name" = "country"))

# Plot with uniform fill color
ggplot(data = world_data) +
  geom_sf(fill = ifelse(!is.na(world_data$count), "lightblue", "gray80"), color = "white") +
  geom_sf_text(data = subset(world_data, !is.na(count)), aes(label = count), size = 5, color = "black") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Countries Included in the Review",
       subtitle = "Each labeled with number of studies",
       caption = "Note some studies were conducted across multipe countries")
# Plot
#ggplot(data = world_data) +
  #geom_sf(aes(fill = count), color = "white") +
  #scale_fill_gradient(low = "#f0f9e8", high = "#0868ac", na.value = "gray90", name = "Study Count") +
  #geom_sf_text(data = subset(world_data, !is.na(count)), aes(label = count), size = 3, color = "black") +
  #theme_minimal() +
  #labs(title = "Heatmap of Study Distribution by Country",
       #caption = "Note: some studies were conducted across multiple countries")
