# =============================================================================
# Figure 2.5: Oral Iron Dosing Recommendations Across NHS Antenatal Guidelines
# Chapter 2: Variation in Antenatal Anaemia Guidelines Across NHS Sites in the UK
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Output:
#   figure_2_5a_oral_iron_dose.png
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# --- Colour palette -----------------------------------------------------------

col_blue <- "#0072B2"
col_pink <- "#CC4678"

# --- Data ---------------------------------------------------------------------

dose_data <- data.frame(
  Dose = c(
    "30-100 mg", "35-65 mg", "40-80 mg", "65 mg",
    "100 mg", "100-200 mg", "Not stated"
  ),
  Daily           = c(1, 3, 8, 24, 1, 7, 2),
  Alternate_daily = c(0, 0, 1,  0, 0, 0, 0)
)

dose_data$Dose <- factor(
  dose_data$Dose,
  levels = c(
    "30-100 mg", "35-65 mg", "40-80 mg", "65 mg",
    "100 mg", "100-200 mg", "Not stated"
  )
)

dose_long <- pivot_longer(
  dose_data,
  cols      = -Dose,
  names_to  = "Frequency",
  values_to = "Guidelines"
) %>%
  filter(Guidelines > 0) %>%
  mutate(Frequency = ifelse(Frequency == "Daily", "Daily", "Alternate-day"))

# --- Figure 2.5a: Oral iron dose stacked bar chart ---------------------------

p_2_5a <- ggplot(dose_long, aes(x = Dose, y = Guidelines, fill = Frequency)) +
  geom_bar(stat = "identity", position = "stack", width = 0.65) +
  geom_text(
    aes(label = ifelse(Guidelines > 0, Guidelines, "")),
    position = position_stack(vjust = 0.5),
    size = 3.5, colour = "white", fontface = "bold"
  ) +
  annotate(
    "rect",
    xmin = 2.5, xmax = 4.5, ymin = 0, ymax = 26,
    alpha = 0.08, fill = "darkgreen"
  ) +
  annotate(
    "text", x = 3.5, y = 25.5,
    label    = "BSH recommended range",
    size     = 4.0, colour = "darkgreen",
    fontface = "italic", hjust = 0.5
  ) +
  scale_fill_manual(
    values = c("Daily" = col_blue, "Alternate-day" = col_pink),
    name   = "Dosing frequency"
  ) +
  scale_y_continuous(
    breaks = seq(0, 26, 4),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = "Elemental iron dose", y = "Number of guidelines") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text          = element_text(size = 12),
    axis.title         = element_text(size = 13, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text        = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(15, 15, 10, 10)
  )

print(p_2_5a)

ggsave(
  "figure_2_5a_oral_iron_dose.png",
  plot = p_2_5a, width = 9, height = 6, dpi = 300, bg = "white"
)
