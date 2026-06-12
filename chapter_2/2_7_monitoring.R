# =============================================================================
# Figure 2.7: Timing and Biomarkers for Post-Treatment Monitoring Across Guidelines
# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Note: Requires mon_oral_long, col_blue, col_light, and col_amber to be defined
#       prior to running this script, or prepend the relevant data preparation code.
#
# Outputs:
#   figure_2_7a_monitoring_oral_iron.png
#   figure_2_7b_monitoring_iv_iron.png
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# --- Colour palette -----------------------------------------------------------

col_blue  <- "#0072B2"
col_light <- "#56B4E9"
col_amber <- "#E69F00"

# --- Figure 2.7a: Monitoring after oral iron ----------------------------------

test_colours_oral <- c(
  "Haemoglobin only"   = col_blue,
  "Haemoglobin + iron studies" = col_light
)

p_2_7a <- ggplot(mon_oral_long, aes(x = Timing, y = Guidelines, fill = Test)) +
  geom_bar(stat = "identity", position = "stack", width = 0.65) +
  geom_vline(
    xintercept = 2.5,
    linetype = "dashed", colour = "darkgreen", linewidth = 0.7
  ) +
  annotate(
    "text", x = 2.6, y = 16.5,
    label    = "BSH: 2-3 weeks",
    colour   = "darkgreen", size = 4.0,
    fontface = "italic", hjust = 0
  ) +
  scale_fill_manual(values = test_colours_oral, name = "Biomarker(s) assessed") +
  scale_y_continuous(
    breaks = seq(0, 16, 4),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(x = "Timing of reassessment", y = "Number of guidelines") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(size = 11, angle = 30, hjust = 1),
    axis.text.y        = element_text(size = 12),
    axis.title         = element_text(size = 13, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text        = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(15, 15, 10, 10)
  )

print(p_2_7a)

ggsave(
  "figure_2_7a_monitoring_oral_iron.png",
  plot = p_2_7a, width = 10, height = 6, dpi = 300, bg = "white"
)

# --- Figure 2.7b: Monitoring after IV iron ------------------------------------

monitoring_iv <- data.frame(
  Test = c(
    "Haemoglobin only",
    "Haemoglobin + ferritin",
    "Hb + ferritin + reticulocyte count"
  ),
  d10  = c(0, 1, 0),
  w2   = c(5, 2, 0),
  w2_3 = c(2, 0, 0),
  w3   = c(2, 1, 0),
  w2_4 = c(2, 1, 0),
  w4   = c(4, 3, 2),
  w4_6 = c(1, 0, 0)
)

mon_iv_long <- pivot_longer(
  monitoring_iv,
  cols      = -Test,
  names_to  = "Timing",
  values_to = "Guidelines"
) %>%
  filter(Guidelines > 0) %>%
  mutate(Timing = recode(Timing,
    "d10" = "10 days", "w2" = "2 weeks", "w2_3" = "2-3 weeks",
    "w3"  = "3 weeks", "w2_4" = "2-4 weeks", "w4" = "4 weeks",
    "w4_6" = "4-6 weeks"
  ))

mon_iv_long$Timing <- factor(
  mon_iv_long$Timing,
  levels = c(
    "10 days", "2 weeks", "2-3 weeks",
    "3 weeks", "2-4 weeks", "4 weeks", "4-6 weeks"
  )
)

test_colours_iv <- c(
  "Haemoglobin only"                   = col_blue,
  "Haemoglobin + ferritin"             = col_light,
  "Hb + ferritin + reticulocyte count" = col_amber
)

p_2_7b <- ggplot(mon_iv_long, aes(x = Timing, y = Guidelines, fill = Test)) +
  geom_bar(stat = "identity", position = "stack", width = 0.65) +
  scale_fill_manual(values = test_colours_iv, name = "Biomarker(s) assessed") +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(x = "Timing of reassessment", y = "Number of guidelines") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(size = 11, angle = 30, hjust = 1),
    axis.text.y        = element_text(size = 12),
    axis.title         = element_text(size = 13, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text        = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(15, 15, 10, 10)
  )

print(p_2_7b)

ggsave(
  "figure_2_7b_monitoring_iv_iron.png",
  plot = p_2_7b, width = 10, height = 6, dpi = 300, bg = "white"
)
