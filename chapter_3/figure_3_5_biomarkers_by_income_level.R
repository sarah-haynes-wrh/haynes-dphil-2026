# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_3_5_biomarkers_by_income_level.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
library(tidyverse)
library(readxl)

# 1) Read your sheet
df_wide <- read_excel(
  file.path("data", "SR_Results_Graphs.xlsx"),
  sheet = "Q3 Barchart Biomarker",
  trim_ws = TRUE
)

# 2) Pivot to long format
income_cols <- c("High", "Upper-Middle", "Lower-Middle", "Low")

long <- df_wide |>
  pivot_longer(all_of(income_cols), names_to = "Income", values_to = "n")

# 3) Reorder biomarkers by total (descending)
order_tbl <- long |>
  group_by(Biomarker) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total))

long <- long |>
  mutate(
    Biomarker = factor(Biomarker, levels = order_tbl$Biomarker),
    Income = factor(Income, levels = c("High", "Upper-Middle", "Lower-Middle", "Low"))
  )



ggplot(long, aes(x = Biomarker, y = Income, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Number of\nStudies"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 11,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 11,
      colour = "black"
    ),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    panel.grid = element_blank(),
    legend.key.height = unit(3, "cm")
    
  )
