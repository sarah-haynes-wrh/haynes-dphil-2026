# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_3_4_biomarker_use_by_timepoint.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# PACKAGES
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)   # not strictly needed now, but harmless if installed

# ---- READ + TIDY ----
# Read data
df_wide <- read_excel(file.path("data", "SR_Results_Graphs.xlsx"),
                      sheet = "Q2 Biomarker by Timepoint")

  # --- 1) READ SHEET (adjust sheet name if needed) ---
  df_wide <- read_excel(
    file.path("data", "SR_Results_Graphs.xlsx"),
    sheet = "Q2 Biomarker by Timepoint"
  )
  
  # Clean any stray whitespace in headers / biomarker names
  names(df_wide) <- str_squish(names(df_wide))
  df_wide <- df_wide %>% mutate(biomarker = str_squish(biomarker))
  
  # --- 2) TIDY LONG + CLEAN NUMBERS ---
  # Define the intended timepoint order
  time_levels <- c(
    "1 Week","2 Weeks","3 Weeks","4 Weeks",
    "6 Weeks","8 Weeks","9 Weeks","10 Weeks","12 Weeks",
    "Gestational Age","Delivery"
  )
  
  long <- df_wide %>%
    pivot_longer(
      cols = -biomarker,
      names_to = "timepoint",
      values_to = "n_raw"
    ) %>%
    mutate(
      timepoint = str_squish(timepoint),
      # keep only the timepoints that actually exist in the sheet
      timepoint = factor(timepoint, levels = intersect(time_levels, unique(timepoint))),
      # coerce counts, treat blanks/NA as 0
      n = suppressWarnings(as.integer(n_raw)),
      n = replace_na(n, 0L)
    )
  
  # --- 3) ORDER BIOMARKERS BY TOTAL COUNT (DESC) ---
  order_tbl <- long %>%
    group_by(biomarker) %>%
    summarise(total_use = sum(n), .groups = "drop") %>%
    arrange(desc(total_use))
  
  lvl_y <- order_tbl$biomarker
  
  long <- long %>%
    mutate(biomarker_f = factor(biomarker, levels = rev(lvl_y)))

  
  # --- 4) PLOT (ABSOLUTE COUNTS HEATMAP) ---
  ggplot(long, aes(x = timepoint, y = biomarker_f, fill = n)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_distiller(
      palette = "Blues", direction = 1,
      breaks = pretty(c(0, max(long$n, na.rm = TRUE)), 4),
      name = "Studies"
    ) +
    labs(
      title = "Biomarker Use by Timepoint",
      x = "Timepoint",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      
      # X-axis text: bigger + black + angled
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 12,        # increase font size
        colour = "black"  # darker font
      ),
      
      # Y-axis text: bigger + black
      axis.text.y = element_text(
        size = 12,
        colour = "black"
      ),
      
      # Title: centered, optionally dark
      plot.title = element_text(
        hjust = 0.5,
        colour = "black",
        size = 14          # optional
      ),
      
      # Keep your long legend
      legend.key.height = unit(3, "cm")
    )
  
  