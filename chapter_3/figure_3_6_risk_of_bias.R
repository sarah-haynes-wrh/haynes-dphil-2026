# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_3_6_risk_of_bias.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Ensure plots go to RStudio's Plots pane
options(device = "RStudioGD")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

df <- read_csv(file.path("data", "rob2_table.csv"))

df_long <- df |>
  pivot_longer(
    cols = c(`Randomisation Process`,
             `Deviations from Intended Interventions`,
             `Missing Outcome Data`,
             `Measurement of the Outcome`,
             `Selection of the Reported Result`,
             Overall),
    names_to = "Domain", values_to = "Judgement"
  )

# ---- ORDER THE COLUMNS (left-to-right) ----
domain_order <- c("Randomisation Process",
                  "Deviations from Intended Interventions",
                  "Missing Outcome Data",
                  "Measurement of the Outcome",
                  "Selection of the Reported Result",
                  "Overall")
df_long$Domain <- factor(df_long$Domain, levels = domain_order)

levs <- c("Low Risk","Some Concerns","High Risk")
df_long$Judgement <- factor(df_long$Judgement, levels = levs)

# Palette
pal <- c("Low Risk" = "#2ca25f", "Some Concerns" = "#ffbf00", "High Risk" = "#de2d26")

# ============== Traffic-light (tile) plot ==============
p_tile <- ggplot(df_long, aes(x = Domain, y = reorder(Study, desc(Study)), fill = Judgement)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Risk of bias (RoB 2): study × domain") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 12),
    legend.position = "right",
    legend.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 13, face = "bold")
  )
print(p_tile)

# ============== Summary bar plot (proportions per domain) ==============
p_bar <- df_long |>
  count(Domain, Judgement) |>
  group_by(Domain) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = Domain, y = prop, fill = Judgement)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL, y = "Proportion of studies", fill = NULL,
    title = "Risk of bias by domain (proportions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 13),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    legend.position = "right",
    legend.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 13, face = "bold")
  )
print(p_bar)

# Save (optional)
ggsave("rob2_traffic_light_custom.png", p_tile, width = 10, height = 8, dpi = 300)
ggsave("rob2_summary_bar_custom.png", p_bar, width = 8, height = 5, dpi = 300)
