# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_4_6_violin_plots_all_biomarkers.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ================== Packages ==================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

use_half_violins <- requireNamespace("gghalves", quietly = TRUE)
if (use_half_violins) library(gghalves)

use_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (use_patchwork) library(patchwork)

# ================== Read Excel ==================
dat <- read_excel(file.path("data", "CRP_Analysis_Master_Data.xlsx"))
nms <- names(dat)

find_col <- function(nms, pattern, label) {
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop(sprintf("Column for '%s' not found.", label), call. = FALSE)
  hit[1]
}
find_col_opt <- function(nms, pattern, label) {
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop(sprintf("Optional column for '%s' not found.", label), call. = FALSE)
  hit[1]
}

# ================== Map columns ==================
col_id       <- find_col(nms, "^id$|^patid$", "ID")
col_group    <- find_col(nms, "^group$", "Group")
col_time     <- find_col(nms, "^time$", "Time")

col_ferritin <- find_col(nms, "^ferritin$", "Ferritin")
col_tsat     <- find_col(nms, "^tsat$", "TSAT")
col_hb       <- find_col(nms, "^hb$|^ha?emoglobin$", "Haemoglobin")
col_stfr     <- find_col(nms, "^stfr$", "sTfR")

col_crp      <- find_col_opt(nms, "^crp$", "CRP")
col_hepc     <- find_col_opt(nms, "^hepcidin$", "Hepcidin")
col_mcv      <- find_col_opt(nms, "^mcv$", "MCV")
col_mch      <- find_col_opt(nms, "^mch$", "MCH")

# ================== Standardised working df ==================
df <- tibble(
  id          = dat[[col_id]],
  group       = factor(dat[[col_group]], levels = c("Daily","AltDaily","3xWeekly")),
  time        = factor(dat[[col_time]],  levels = c("Baseline","FollowUp")),
  Ferritin    = as.numeric(dat[[col_ferritin]]),
  TSAT        = as.numeric(dat[[col_tsat]]),
  Haemoglobin = as.numeric(dat[[col_hb]]),
  sTfR        = as.numeric(dat[[col_stfr]]),
  CRP         = as.numeric(dat[[col_crp]]),
  Hepcidin    = as.numeric(dat[[col_hepc]]),
  MCV         = as.numeric(dat[[col_mcv]]),
  MCH         = as.numeric(dat[[col_mch]])
)

# ================== Long format ==================
biomarkers_to_plot <- c(
  "Haemoglobin","MCV","MCH","CRP",
  "Ferritin","TSAT","sTfR","Hepcidin"
)

long <- df %>%
  pivot_longer(all_of(biomarkers_to_plot),
               names_to = "biomarker",
               values_to = "value") %>%
  filter(!is.na(value))

log_biomarkers <- c("Ferritin","sTfR","CRP","Hepcidin")

# ================== Plot function ==================
plot_one <- function(bm, show_legend = FALSE) {
  
  df_bm <- long %>% filter(biomarker == bm)
  
  if (bm %in% log_biomarkers)
    df_bm <- df_bm %>% filter(value > 0)
  
  pd <- position_dodge(width = 0.5)
  
  p <- ggplot(df_bm, aes(group, value, fill = time, color = time)) +
    geom_violin(position = pd, width = 0.8, alpha = 0.35, colour = NA) +
    geom_point(position = pd, alpha = 0.6, size = 1.7)
  
  # ---------- Thresholds / reference ranges ----------
  if (bm == "Ferritin") p <- p + geom_hline(yintercept = 30, linetype = "dashed", colour = "red")
  if (bm == "TSAT")     p <- p + geom_hline(yintercept = 20, linetype = "dashed", colour = "red")
  if (bm == "CRP")      p <- p + geom_hline(yintercept = 5,  linetype = "dashed", colour = "red")
  
  if (bm == "Haemoglobin")
    p <- p +
    geom_hline(yintercept = 110, linetype = "dashed", colour = "red") +
    geom_hline(yintercept = 105, linetype = "dotted", colour = "red")
  
  if (bm == "MCV")
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = 100,
                      fill = "grey40", alpha = 0.08)
  
  if (bm == "MCH")
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 27, ymax = 32,
                      fill = "grey40", alpha = 0.08)
  
  # ---------- Units ----------
  units <- c(
    Haemoglobin = "g/L",
    MCV         = "fL",
    MCH         = "pg",
    CRP         = "mg/L",
    Ferritin    = "µg/L",
    TSAT        = "%",
    sTfR        = "mg/L",
    Hepcidin    = "ng/mL"
  )
  
  # ---------- Axis + title labels ----------
  y_lab <- if (bm %in% log_biomarkers)
    paste0(bm, " (", units[bm], ") (log10 axis)")
  else
    paste0(bm, " (", units[bm], ")")
  
  title_lab <- paste0(bm, " (", units[bm], ")")
  
  subtitle_txt <- case_when(
    bm == "Ferritin"    ~ "Dashed: <30 µg/L",
    bm == "TSAT"        ~ "Dashed: <20%",
    bm == "CRP"         ~ "Dashed: >5 mg/L",
    bm == "Haemoglobin" ~ "Dashed: <110 g/L (Baseline); Dotted: <105 g/L (FollowUp)",
    bm == "MCV"         ~ "Reference range: 80–100 fL",
    bm == "MCH"         ~ "Reference range: 27–32 pg",
    bm == "sTfR"        ~ "No threshold",
    bm == "Hepcidin"    ~ "No threshold",
    TRUE ~ NA_character_
  )
  
  p +
    (if (bm %in% log_biomarkers) scale_y_continuous(trans = "log10") else scale_y_continuous()) +
    labs(
      title = title_lab,
      subtitle = subtitle_txt,
      x = NULL,
      y = y_lab,
      fill = "Time",
      color = "Time"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

# ================== Build plots ==================
p_hb   <- plot_one("Haemoglobin")
p_mcv  <- plot_one("MCV")
p_mch  <- plot_one("MCH")
p_crp  <- plot_one("CRP")

p_ferr <- plot_one("Ferritin")
p_tsat <- plot_one("TSAT")
p_stfr <- plot_one("sTfR")
p_hepc <- plot_one("Hepcidin", show_legend = TRUE)

# ================== Two-column layout ==================
combined <- (p_hb / p_mcv / p_mch / p_crp) |
  (p_ferr / p_tsat / p_stfr / p_hepc) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Biomarker distributions by group and timepoint",
    subtitle = "Ferritin, sTfR, CRP and Hepcidin shown on log10 scale; thresholds/ranges shown where specified"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    legend.box = "horizontal"
  )

# ================== Preview ==================
print(combined)

# ================== Save ==================
ggsave("biomarkers_A4_landscape_two_column_logCRP_logHepcidin.pdf",
       combined, width = 11.69, height = 8.27, dpi = 300)
ggsave("biomarkers_A4_landscape_two_column_logCRP_logHepcidin.png",
       combined, width = 11.69, height = 8.27, dpi = 300)
