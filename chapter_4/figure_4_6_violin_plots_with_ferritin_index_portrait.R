# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_4_6_violin_plots_with_ferritin_index_portrait.R
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

# NEW: ferritin index, transferrin, TIBC
col_fi       <- find_col_opt(nms, "^ferritin[_ ]?index$|^ferrtin[_ ]?index$", "Ferritin index")
col_transf   <- find_col_opt(nms, "^transferrin$", "Transferrin")
col_tibc     <- find_col_opt(nms, "^tibc$", "TIBC")

# ================== Standardised working df ==================
df <- tibble(
  id            = dat[[col_id]],
  group         = factor(dat[[col_group]], levels = c("Daily","AltDaily","3xWeekly")),
  time          = factor(dat[[col_time]],  levels = c("Baseline","FollowUp")),
  
  Ferritin      = as.numeric(dat[[col_ferritin]]),
  TSAT          = as.numeric(dat[[col_tsat]]),
  Haemoglobin   = as.numeric(dat[[col_hb]]),
  sTfR          = as.numeric(dat[[col_stfr]]),
  
  CRP           = as.numeric(dat[[col_crp]]),
  Hepcidin      = as.numeric(dat[[col_hepc]]),
  MCV           = as.numeric(dat[[col_mcv]]),
  MCH           = as.numeric(dat[[col_mch]]),
  
  FerritinIndex = as.numeric(dat[[col_fi]]),
  Transferrin   = as.numeric(dat[[col_transf]]),
  TIBC          = as.numeric(dat[[col_tibc]])
)

# ================== Long format ==================
biomarkers_to_plot <- c(
  "Haemoglobin","MCV","MCH",
  "Ferritin","sTfR","FerritinIndex",
  "TIBC","Transferrin","TSAT",
  "Hepcidin","CRP"
)

long <- df %>%
  pivot_longer(all_of(biomarkers_to_plot),
               names_to = "biomarker",
               values_to = "value") %>%
  filter(!is.na(value))

# Log-scale markers (edit if you do NOT want FerritinIndex on log scale)
log_biomarkers <- c("Ferritin","sTfR","CRP","Hepcidin","FerritinIndex")

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
  
  # Adult general range 250–300 mg/dL == 2.5–3.0 g/L (if your Transferrin is g/L)
  if (bm == "Transferrin") {
    p <- p +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.0,
               fill = "grey40", alpha = 0.08)
  }
  
  # TIBC reference range: 45–80 µmol/L (WHO/standard adult reference range)
  # This shading is illustrative; verify against the assay-specific reference
  # interval used in the PANDA laboratory before publication.
  if (bm == "TIBC") {
    p <- p +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 45, ymax = 80,
               fill = "grey40", alpha = 0.08)
  }
  
  # ---------- Units ----------
  units <- c(
    Haemoglobin   = "g/L",
    MCV           = "fL",
    MCH           = "pg",
    CRP           = "mg/L",
    Ferritin      = "\u00B5g/L",
    TSAT          = "%",
    sTfR          = "mg/L",
    Hepcidin      = "ng/mL",
    FerritinIndex = "a.u.",
    Transferrin   = "g/L",
    TIBC          = "\u00B5mol/L"
  )
  
  # ---------- Axis + title labels ----------
  y_lab <- if (bm %in% log_biomarkers)
    paste0(bm, " (", units[bm], ") (log10 axis)")
  else
    paste0(bm, " (", units[bm], ")")
  
  title_lab <- paste0(bm, " (", units[bm], ")")
  
  subtitle_txt <- case_when(
    bm == "Ferritin"      ~ "Dashed: <30 \u00B5g/L",
    bm == "TSAT"          ~ "Dashed: <20%",
    bm == "CRP"           ~ "Dashed: >5 mg/L",
    bm == "Haemoglobin"   ~ "Dashed: <110 g/L (Baseline); Dotted: <105 g/L (FollowUp)",
    bm == "MCV"           ~ "Reference band: 80\u2013100 fL",
    bm == "MCH"           ~ "Reference band: 27\u201332 pg",
    bm == "Transferrin"   ~ "Reference band: 2.5\u20133.0 g/L (general adult range)",
    bm == "TIBC"          ~ "Reference band: 45\u201380 \u00B5mol/L (generic adult range; assay-dependent)",
    bm == "sTfR"          ~ "No threshold",
    bm == "Hepcidin"      ~ "No threshold",
    bm == "FerritinIndex" ~ "No threshold",
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
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 9),
      plot.subtitle = element_text(size = 7),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 8),
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

# ================== Build plots in requested 3x4 layout ==================
p_hb   <- plot_one("Haemoglobin")
p_mcv  <- plot_one("MCV")
p_mch  <- plot_one("MCH")

p_ferr <- plot_one("Ferritin")
p_stfr <- plot_one("sTfR")
p_fi   <- plot_one("FerritinIndex")

p_tibc <- plot_one("TIBC")
p_tran <- plot_one("Transferrin")
p_tsat <- plot_one("TSAT")

p_hepc <- plot_one("Hepcidin")
p_crp  <- plot_one("CRP", show_legend = TRUE)

blank_panel <- plot_spacer()

# ================== 3 columns x 4 rows (portrait A4) ==================
combined <- (p_hb   | p_mcv  | p_mch) /
  (p_ferr | p_stfr | p_fi) /
  (p_tibc | p_tran | p_tsat) /
  (p_hepc | p_crp  | blank_panel) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Biomarker distributions by group and timepoint",
    subtitle = "Ferritin, sTfR, Ferritin index, CRP and Hepcidin shown on log10 scale; thresholds/ranges shown where specified"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    legend.box = "horizontal"
  )

# ================== Preview ==================
print(combined)

# ================== Save (A4 portrait) ==================
# A4 portrait: 8.27 x 11.69 inches
ggsave("biomarkers_A4_portrait_3x4_with_FI_Tf_TIBC.pdf",
       combined, width = 8.27, height = 11.69, dpi = 300)
ggsave("biomarkers_A4_portrait_3x4_with_FI_Tf_TIBC.png",
       combined, width = 8.27, height = 11.69, dpi = 300)
