# =============================================================================
# Figure 2.3: Distribution of NHS Antenatal Anaemia Guidelines Across UK Regions
# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Outputs:
#   figure_2_3a_map.png      -- Bubble map of guideline counts by region
#   figure_2_3b_barchart.png -- Horizontal bar chart of annual births by region
# =============================================================================

library(ggplot2)
library(maps)
library(dplyr)
library(scales)

# --- Settings -----------------------------------------------------------------

label_size  <- 4.3
main_colour <- "#0072B2"

# --- Data ---------------------------------------------------------------------

guideline_data <- data.frame(
  Region = c(
    "London", "South East", "North East", "West Midlands",
    "East of England", "North West", "Yorkshire & Humber",
    "East Midlands", "South West", "Northern Ireland",
    "Scotland", "Wales"
  ),
  Guidelines = c(4, 8, 4, 3, 5, 4, 6, 3, 2, 1, 3, 4),
  Annual_Births = c(
    88000, 62000, 23000, 31000, 35000, 51000,
    43000, 29000, 28000, 24000, 51000, 30000
  ),
  lon = c(
     0.10, -0.50, -1.60, -2.00,  0.90, -2.60,
    -1.20, -1.00, -3.60, -6.65, -4.20, -3.80
  ),
  lat = c(
    51.50, 51.10, 54.90, 52.48, 52.30, 53.80,
    53.80, 52.80, 50.80, 54.60, 57.00, 52.30
  ),
  lon_offset = c(
     1.75,  1.75,  1.95, -2.25,
     1.95, -2.10,  2.20,  2.15,
    -2.10, -2.25,  2.00, -2.40
  ),
  lat_offset = c(
    -0.15, -0.55,  0.00,  0.20,
    -0.15,  0.00,  0.45, -0.10,
    -0.35,  0.25,  0.00, -0.25
  ),
  label_hjust = c(
    0, 0, 0, 1,
    0, 1, 0, 0,
    1, 1, 0, 1
  )
)

guideline_data$fontface <- ifelse(
  guideline_data$Region %in% c("Scotland", "Wales", "Northern Ireland"),
  "bold",
  "plain"
)

# --- Map data -----------------------------------------------------------------

uk_map      <- map_data("world", region = "UK")
ireland_map <- map_data("world", region = "Ireland")

# --- Figure 2.3a: Bubble map --------------------------------------------------

p_map <- ggplot() +
  geom_polygon(
    data = ireland_map,
    aes(x = long, y = lat, group = group),
    fill = "#e8e8e8", colour = "#cccccc", linewidth = 0.3
  ) +
  geom_polygon(
    data = uk_map,
    aes(x = long, y = lat, group = group),
    fill = "#f0f0f0", colour = "#999999", linewidth = 0.4
  ) +
  geom_segment(
    data = guideline_data,
    aes(
      x = lon, y = lat,
      xend = lon + lon_offset * 0.80,
      yend = lat + lat_offset
    ),
    colour = "grey60", linewidth = 0.3
  ) +
  geom_point(
    data = guideline_data,
    aes(x = lon, y = lat, size = Guidelines),
    shape = 21, fill = main_colour, colour = "white",
    stroke = 0.5, alpha = 0.92
  ) +
  geom_text(
    data = guideline_data,
    aes(x = lon, y = lat, label = Guidelines),
    size = 4.2, fontface = "bold", colour = "white"
  ) +
  geom_text(
    data = guideline_data,
    aes(
      x = lon + lon_offset,
      y = lat + lat_offset,
      label = Region,
      hjust = label_hjust,
      fontface = fontface
    ),
    size = label_size, colour = "grey15", lineheight = 0.9
  ) +
  scale_size_continuous(range = c(5, 14), guide = "none") +
  coord_fixed(
    ratio = 1.6,
    xlim  = c(-13.5, 6.5),
    ylim  = c(49.5, 61.0),
    clip  = "off"
  ) +
  theme_void() +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 40))

print(p_map)

ggsave(
  "figure_2_3a_map.png",
  plot = p_map, width = 9, height = 9, dpi = 300, bg = "white"
)

# --- Figure 2.3b: Annual births bar chart ------------------------------------

bar_data <- guideline_data %>%
  arrange(desc(Annual_Births)) %>%
  mutate(Region = factor(Region, levels = rev(Region)))

p_bar <- ggplot(bar_data, aes(x = Annual_Births, y = Region)) +
  geom_col(fill = main_colour, width = 0.7) +
  geom_text(
    aes(label = comma(Annual_Births)),
    hjust = -0.15, size = 5, colour = "black"
  ) +
  scale_x_continuous(
    labels = comma,
    breaks = seq(0, 100000, 25000),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    x = "Annual births represented by included NHS sites",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 12),
    axis.text.x        = element_text(size = 11),
    axis.title.x       = element_text(size = 12, face = "bold"),
    plot.margin        = margin(10, 20, 10, 10)
  )

print(p_bar)

ggsave(
  "figure_2_3b_barchart.png",
  plot = p_bar, width = 8, height = 6, dpi = 300, bg = "white"
)
