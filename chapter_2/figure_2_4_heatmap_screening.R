# =============================================================================
# Figure 2.4: Regional Variation in Additional Anaemia Screening Across NHS Sites
# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Output:
#   figure_2_4_screening_map.png
# =============================================================================

library(ggplot2)
library(maps)
library(dplyr)

# --- Data ---------------------------------------------------------------------

screening_data <- data.frame(
  Region = c(
    "Greater London", "South East England", "North East England",
    "West Midlands", "East of England", "North West England",
    "Yorkshire and the Humber", "East Midlands", "South West England",
    "Northern Ireland", "Scotland", "Wales"
  ),
  Screening_Type = c(
    "No additional screening",
    "No additional screening",
    "Additional Hb",
    "Additional Hb",
    "No additional screening",
    "Additional ferritin",
    "Additional Hb and ferritin",
    "Additional Hb",
    "Additional ferritin",
    "No additional screening",
    "Additional ferritin",
    "Additional ferritin"
  ),
  lon = c(
     0.10, -0.50, -1.60, -2.00,  0.90, -2.60,
    -1.20, -1.00, -3.60, -6.65, -4.20, -3.80
  ),
  lat = c(
    51.50, 51.10, 54.90, 52.48, 52.30, 53.80,
    53.80, 52.80, 50.80, 54.60, 57.00, 52.30
  ),
  lon_offset  = c( 1.8, -1.8,  1.8, -1.8,  1.8, -1.8,
                   1.8,  1.8, -1.8, -1.8,  1.8, -1.8),
  lat_offset  = c( 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,
                   0.4,  0.0,  0.0,  0.0,  0.0,  0.0),
  label_hjust = c(   0,    1,    0,    1,    0,    1,
                     0,    0,    1,    1,    0,    1)
)

screening_data$Screening_Type <- factor(
  screening_data$Screening_Type,
  levels = c(
    "Additional Hb",
    "Additional ferritin",
    "Additional Hb and ferritin",
    "No additional screening"
  )
)

# --- Colour palette -----------------------------------------------------------
# Distinct in both colour and greyscale

palette_colours <- c(
  "Additional Hb"               = "#E69F00",  # Amber
  "Additional ferritin"         = "#009E73",  # Teal
  "Additional Hb and ferritin"  = "#0072B2",  # Dark blue
  "No additional screening"     = "#cccccc"   # Light grey
)

# --- Map data -----------------------------------------------------------------

uk_map      <- map_data("world", region = "UK")
ireland_map <- map_data("world", region = "Ireland")

# --- Figure 2.4: Screening map ------------------------------------------------

p_2_4 <- ggplot() +
  geom_polygon(
    data = ireland_map,
    aes(x = long, y = lat, group = group),
    fill = "#f0f0f0", colour = "#cccccc", linewidth = 0.3
  ) +
  geom_polygon(
    data = uk_map,
    aes(x = long, y = lat, group = group),
    fill = "#f8f8f8", colour = "#999999", linewidth = 0.4
  ) +
  geom_segment(
    data = screening_data,
    aes(
      x = lon, y = lat,
      xend = lon + lon_offset * 0.80,
      yend = lat + lat_offset
    ),
    colour = "grey50", linewidth = 0.3
  ) +
  geom_point(
    data = screening_data,
    aes(x = lon, y = lat, fill = Screening_Type),
    shape = 21, size = 9, colour = "white",
    stroke = 0.6, alpha = 0.95
  ) +
  geom_text(
    data = screening_data,
    aes(
      x     = lon + lon_offset,
      y     = lat + lat_offset,
      label = Region,
      hjust = label_hjust
    ),
    size = 3.6, colour = "grey15", lineheight = 0.9
  ) +
  scale_fill_manual(
    values = palette_colours,
    name   = "Additional routine\nscreening recommended",
    drop   = FALSE
  ) +
  coord_fixed(
    ratio = 1.6,
    xlim  = c(-10.5, 6.5),
    ylim  = c(49.5, 61.0)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.key.size = unit(0.9, "cm"),
    plot.margin     = margin(10, 10, 10, 10)
  )

print(p_2_4)

ggsave(
  "figure_2_4_screening_map.png",
  plot = p_2_4, width = 10, height = 9, dpi = 300, bg = "white"
)
