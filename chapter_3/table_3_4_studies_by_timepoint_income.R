# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: table_3_4_studies_by_timepoint_income.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
library(tidyverse)

# 1) Enter your data ----
df <- tribble(
  ~Timepoint, ~Number_Studies, ~High, ~Upper_Middle, ~Lower_Middle_Excl_India, ~India, ~Low,
  "1",        4,               0,     0,             1,                         3,      0,
  "2",        7,               1,     1,             0,                         5,      0,
  "3",        4,               1,     1,             0,                         2,      0,
  "4",        21,              3,     2,             4,                         12,     0,
  "6",        7,               2,     2,             0,                         3,      0,
  "8",        10,              0,     1,             0,                         9,      0,
  "9",        3,               1,     1,             1,                         0,      0,
  "10",       2,               0,     0,             0,                         2,      0,
  "12",       10,              1,     3,             3,                         3,      0,
  "GA",       4,               1,     1,             0,                         1,      1,
  "D",        5,               2,     2,             0,                         1,      1
)

# 2) Pivot to long format ----
df_long <- df |>
  pivot_longer(
    cols = c(High, Upper_Middle, Lower_Middle_Excl_India, India, Low),
    names_to = "Income",
    values_to = "n"
  )

# 3) Factor order for timepoints + income ----
df_long <- df_long |>
  mutate(
    Timepoint = factor(Timepoint,
                       levels = c("1","2","3","4","6","8","9","10","12","GA","D")),
    Income = factor(
      Income,
      levels = c("High", "Upper_Middle", "Lower_Middle_Excl_India", "India", "Low")
    )
  )

# 4) Plot ----
ggplot(df_long, aes(x = Timepoint, y = n, fill = Income)) +
  geom_col(position = "stack") +
  labs(
    title = "Number of Studies by Timepoint and Country Income Level (India Separated)",
    x = "Timepoint (weeks / GA / Delivery)",
    y = "Number of Studies",
    fill = "Country Income Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 12,
      colour = "black"
    ),
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      colour = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "High"                      = "#08519C",  # darkest blue
      "Upper_Middle"              = "#2171B5",
      "Lower_Middle_Excl_India"   = "#6BAED6",
      "Low"                       = "#BDD7E7",  # lightest blue
      "India"                     = "black"     # highlight India
    )
  )
