# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_crp_categories_stacked_bar.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# --- data as before ---
crp_tbl <- tribble(
  ~Group,           ~Time, ~Cat,          ~n, ~Ntot,
  "Daily",          "BL",  "<5",           32, 45,
  "Daily",          "FU",  "<5",           29, 45,
  "Daily",          "BL",  "5–10",          9,  45,
  "Daily",          "FU",  "5–10",          9,  45,
  "Daily",          "BL",  "10–20",         1,  45,
  "Daily",          "FU",  "10–20",         6,  45,
  "Daily",          "BL",  ">20",           3,  45,
  "Daily",          "FU",  ">20",           1,  45,
  
  "Alternate",      "BL",  "<5",           23, 47,
  "Alternate",      "FU",  "<5",           29, 47,
  "Alternate",      "BL",  "5–10",         13, 47,
  "Alternate",      "FU",  "5–10",         11, 47,
  "Alternate",      "BL",  "10–20",         6, 47,
  "Alternate",      "FU",  "10–20",         6, 47,
  "Alternate",      "BL",  ">20",           5, 47,
  "Alternate",      "FU",  ">20",           1, 47,
  
  "3× weekly",      "BL",  "<5",           24, 43,
  "3× weekly",      "FU",  "<5",           24, 43,
  "3× weekly",      "BL",  "5–10",         11, 43,
  "3× weekly",      "FU",  "5–10",         10, 43,
  "3× weekly",      "BL",  "10–20",         4, 43,
  "3× weekly",      "FU",  "10–20",         4, 43,
  "3× weekly",      "BL",  ">20",           4, 43,
  "3× weekly",      "FU",  ">20",           5, 43
) %>%
  mutate(
    Cat  = factor(Cat, levels = c("<5","5–10","10–20",">20")),
    Group = factor(Group, levels = c("Daily","Alternate","3× weekly")),
    Time  = factor(Time,  levels = c("BL","FU")),
    pct  = 100 * n / Ntot
  )

# --- make a tidy label for x-axis (Daily BL, Daily FU, etc.) ---
crp_tbl <- crp_tbl %>%
  mutate(GroupTime = interaction(Group, Time, sep = "_"))

# Ensure combined factor follows the correct order:
crp_tbl$GroupTime <- factor(
  crp_tbl$GroupTime,
  levels = c("Daily_BL","Daily_FU",
             "Alternate_BL","Alternate_FU",
             "3× weekly_BL","3× weekly_FU")
)

# --- plot ---
ggplot(crp_tbl, aes(x = GroupTime, y = pct, fill = Cat)) +
  geom_col(width = 0.75, color = "grey20", linewidth = 0.2) +
  geom_text(aes(label = ifelse(pct >= 6, n, "")),
            position = position_stack(vjust = 0.5), size = 6) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "YlGnBu", direction = 1) +
  labs(
    x = NULL, y = "CRP category composition (%)", 
    fill = "CRP (mg/L)",
    title = "CRP category composition by group at baseline (BL) and follow-up (FU)"
  ) +
  theme_minimal(base_size = 25) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = c("Daily\n(BL)", "Daily\n(FU)",
               "Alternate-day\n(BL)", "Alternate-day\n(FU)",
               "3× weekly\n(BL)", "3× weekly\n(FU)")
  )
