# Chapter 3: Assessing Response to Oral Iron in Pregnancy - Systematic Review
# Script: figure_3_3_biomarker_use_by_year.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
#install.packages("ggtext")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)   # for bold label on y-axis

# Read your sheet
df_wide <- read_excel(file.path("data", "SR_Results_Graphs.xlsx"),
                 sheet = "Q1 Biomarker by Year")

# --- 2) TIDY LONG + CLEAN NUMBERS ---
long <- df_wide %>%
  pivot_longer(-biomarker, names_to = "year", values_to = "n_raw") %>%
  mutate(
    year = as.integer(str_extract(year, "\\d+")),
    n    = suppressWarnings(as.integer(n_raw)),
    n    = replace_na(n, 0L)
  )

# --- 3) SEPARATE 'Total Studies' AND ORDER BIOMARKERS BY TOTAL COUNT ---
totals <- long %>% filter(str_to_lower(biomarker) == "total studies") %>%
  mutate(biomarker_f = "Total Studies")

biomarkers_only <- long %>% filter(str_to_lower(biomarker) != "total studies")

# (optional) normalise the indices label if present
#biomarkers_only <- biomarkers_only %>%
  #mutate(biomarker = ifelse(str_to_lower(biomarker) == "red cell indicies",
              #              "Red cell indices (MCV/MCH/MCHC)", biomarker))

# Order biomarkers by overall use (descending)
order_tbl <- biomarkers_only %>%
  group_by(biomarker) %>%
  summarise(total_use = sum(n), .groups = "drop") %>%
  arrange(desc(total_use))

# Put all biomarkers (ordered) + 'Total Studies' at the bottom
lvl_y <- c(order_tbl$biomarker, "Total Studies")

biomarkers_only <- biomarkers_only %>%
  mutate(biomarker_f = factor(biomarker, levels = rev(lvl_y)))

totals <- totals %>%
  mutate(biomarker_f = factor(biomarker_f, levels = rev(lvl_y)))


# Build a label vector that bolds 'Total Studies'
#y_lab <- setNames(as.list(lvl_y), lvl_y)
#y_lab[["Total Studies"]] <- "Total Studies"

# Build a label vector that bolds 'Total Studies'
y_lab <- setNames(as.list(lvl_y), lvl_y)
y_lab[["Total Studies"]] <- "Total Studies"


# --- 4) PLOT (ABSOLUTE COUNTS HEATMAP) ---
ggplot() +
  # biomarkers (white grid lines)
  geom_tile(data = biomarkers_only,
            aes(x = year, y = biomarker_f, fill = n),
            color = "white", linewidth = 0.2) +
  # totals row overlay with a black border so it stands out
  geom_tile(data = totals,
            aes(x = year, y = biomarker_f, fill = n),
            color = "black", linewidth = 0.5, height = 0.98) +
 
   # darker blue scale
 scale_fill_distiller(
  palette = "Blues", direction = 1,
  breaks = pretty(c(0, max(long$n)), 4),  # 4 steps based on your counts
  name = "Studies"
) +
  scale_y_discrete(labels = y_lab) +         # bold 'Total Studies'
  scale_x_continuous(breaks = sort(unique(long$year))) +
  labs(
    #title = "Biomarker Use by Year",
    x = "Publication Year", y = NULL
    
    
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    
    # X-axis text (bigger + darker)
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 12,        # increase font size
      colour = "black"  # darker font
    ),
    
    # Y-axis text (markdown-enabled, bigger + darker)
    axis.text.y = ggtext::element_markdown(
      size = 12,
      colour = "black"
    ),
    
    # Title stays centred, can also enlarge if wanted
    plot.title = element_text(
      hjust = 0.5,
      size = 14,        # optional: make title bigger
      colour = "black"
    ),
    
    # Long legend
    legend.key.height = unit(2, "cm")
  )

    