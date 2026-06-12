# =============================================================================
# Figure 2.6: Haemoglobin Thresholds for IV Iron Initiation Across NHS Guidelines
# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Note: Requires hb_data and col_blue to be defined prior to running this script,
#       or prepend the relevant data preparation code here.
#
# Output:
#   figure_2_6_iv_iron_hb_thresholds.png
# =============================================================================

library(ggplot2)

# --- Colour palette -----------------------------------------------------------

col_blue <- "#0072B2"

# --- Figure 2.6: IV iron Hb threshold bar chart ------------------------------

p_2_6 <- ggplot(hb_data, aes(x = Guidelines, y = Threshold)) +
  geom_bar(stat = "identity", fill = col_blue, width = 0.65) +
  geom_tile(
    data = subset(hb_data, Threshold == "Hb <70 g/L"),
    aes(x = 6, y = Threshold, width = 12.5, height = 1),
    fill = "darkgreen", alpha = 0.08, inherit.aes = FALSE
  ) +
  geom_text(
    data = subset(hb_data, Threshold == "Hb <70 g/L"),
    aes(x = 6, y = Threshold, label = "BSH recommendation"),
    vjust = -1.2, size = 4.0, colour = "darkgreen",
    fontface = "italic", inherit.aes = FALSE
  ) +
  geom_text(
    aes(label = Guidelines),
    hjust = -0.3, size = 4.0, colour = "black"
  ) +
  scale_x_continuous(
    breaks = seq(0, 12, 2),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(x = "Number of guidelines", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text          = element_text(size = 12),
    axis.title.x       = element_text(size = 13, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(15, 20, 10, 10)
  )

print(p_2_6)

ggsave(
  "figure_2_6_iv_iron_hb_thresholds.png",
  plot = p_2_6, width = 8, height = 5, dpi = 300, bg = "white"
)
