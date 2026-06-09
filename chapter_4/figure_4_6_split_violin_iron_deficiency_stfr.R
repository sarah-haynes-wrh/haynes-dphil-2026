# Chapter 4: Impact of Iron Supplementation Frequency on Iron Biomarkers (PANDA Trial)
# Script: figure_4_6_split_violin_iron_deficiency_stfr.R
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# ================== Packages ==================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

use_half_violins <- requireNamespace("gghalves", quietly = TRUE)
if (use_half_violins) library(gghalves)

# ================== Read Excel ==================

dat <- read_excel(file.path("data", "CRP_Analysis_Master_Data.xlsx"))
nms <- names(dat)

# Helper: find a column by regex (case-insensitive)
find_col <- function(nms, pattern, label) {
  hit <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop(sprintf("Column for '%s' not found. Looked for: %s\nAvailable: %s",
                                 label, pattern, paste(nms, collapse = ", ")), call. = FALSE)
  if (length(hit) > 1) message(sprintf("Multiple '%s' matches: %s → using '%s'",
                                       label, paste(hit, collapse = ", "), hit[1]))
  hit[1]
}

# Map columns (case-insensitive)
col_id       <- find_col(nms, "^id$|^patid$",               "ID")
col_group    <- find_col(nms, "^group$",                    "Group")
col_time     <- find_col(nms, "^time$",                     "Time")
col_ferritin <- find_col(nms, "^ferritin$",                 "Ferritin")
col_tsat     <- find_col(nms, "^tsat$",                     "TSAT")
col_hb       <- find_col(nms, "^hb$|^ha?emoglobin$",        "Hb")
col_stfr     <- find_col(nms, "^stfr$|^s\\s*tfr$",          "sTfR")

col_crp      <- grep("^crp$", nms, ignore.case = TRUE, value = TRUE)[1]
col_hepc     <- grep("^hepcidin$", nms, ignore.case = TRUE, value = TRUE)[1]

# ================== Standardised working df ==================
df <- tibble::tibble(
  id       = dat[[col_id]],
  group    = dat[[col_group]],
  time     = dat[[col_time]],
  ferritin = suppressWarnings(as.numeric(dat[[col_ferritin]])),
  tsat     = suppressWarnings(as.numeric(dat[[col_tsat]])),
  hb       = suppressWarnings(as.numeric(dat[[col_hb]])),
  stfr     = suppressWarnings(as.numeric(dat[[col_stfr]])),
  CRP      = if (!is.na(col_crp))  suppressWarnings(as.numeric(dat[[col_crp]]))  else NA_real_,
  hepcidin = if (!is.na(col_hepc)) suppressWarnings(as.numeric(dat[[col_hepc]])) else NA_real_
)

# Order factor levels
df <- df %>%
  mutate(
    group = factor(group, levels = c("Daily","AltDaily","3xWeekly")),
    time  = factor(time,  levels = c("Baseline","FollowUp"))
  )

# ================== Long format ==================
long <- df %>%
  select(id, group, time, ferritin, tsat, hb, stfr) %>%
  rename(Ferritin = ferritin, TSAT = tsat, Hb = hb, sTfR = stfr) %>%
  pivot_longer(cols = c(Ferritin, TSAT, Hb, sTfR),
               names_to = "biomarker", values_to = "value") %>%
  filter(!is.na(value))

# ================== Thresholds (none for sTfR) ==================
thresh <- tibble::tibble(
  biomarker = c("Ferritin","TSAT","Hb","Hb"),
  time      = c("Baseline","Baseline","Baseline","FollowUp"),
  y         = c(30, 20, 110, 105)
)

# ================== Plot function ==================
plot_one <- function(bm, show_legend = FALSE) {
  df_bm <- dplyr::filter(long, biomarker == bm)
  p <- ggplot(df_bm, aes(x = group, y = value, fill = time, color = time))
  
  if (use_half_violins) {
    p <- p +
      gghalves::geom_half_violin(
        data = ~ dplyr::filter(.x, time == "Baseline"),
        side = "l", width = 0.8, alpha = 0.5, color = NA
      ) +
      gghalves::geom_half_violin(
        data = ~ dplyr::filter(.x, time == "FollowUp"),
        side = "r", width = 0.8, alpha = 0.5, color = NA
      ) +
      gghalves::geom_half_point(
        data = ~ dplyr::filter(.x, time == "Baseline"),
        side = "l", range_scale = .4, alpha = 0.6, size = 1.9,
        transform = ggplot2::position_nudge(x = -0.08)
      ) +
      gghalves::geom_half_point(
        data = ~ dplyr::filter(.x, time == "FollowUp"),
        side = "r", range_scale = .4, alpha = 0.6, size = 1.9,
        transform = ggplot2::position_nudge(x = +0.08)
      )
  } else {
    pd <- position_dodge(width = 0.5)
    p <- p +
      geom_violin(position = pd, width = 0.8, alpha = 0.35, color = NA) +
      geom_point(position = pd, alpha = 0.6, size = 1.9)
  }
  
  # Thresholds for Ferritin, TSAT, Hb only (none for sTfR)
  if (bm %in% c("Ferritin","TSAT","Hb")) {
    if (bm %in% c("Ferritin","TSAT")) {
      th_y <- if (bm == "Ferritin") 30 else 20
      p <- p + geom_hline(yintercept = th_y, linetype = "dashed", color = "red")
    } else if (bm == "Hb") {
      xvals <- sort(unique(as.numeric(df_bm$group)))
      if (length(xvals) > 0) {
        seg_df <- tibble::tibble(x = xvals - 0.33, xend = xvals + 0.33)
        p <- p +
          geom_segment(data = transform(seg_df, y = 110, yend = 110),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       inherit.aes = FALSE, linetype = "dashed", color = "red") +
          geom_segment(data = transform(seg_df, y = 105, yend = 105),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       inherit.aes = FALSE, linetype = "dotted", color = "red")
      }
    }
  }
  
  # Labels
  y_lab <- dplyr::case_when(
    bm == "Ferritin" ~ "Ferritin (µg/L, log₁₀ scale)",
    bm == "TSAT"     ~ "TSAT (%)",
    bm == "Hb"       ~ "Haemoglobin (g/L)",
    bm == "sTfR"     ~ "sTfR (mg/L, log₁₀ scale)",
    TRUE             ~ "Value"
  )
  
  subtitle_txt <- dplyr::case_when(
    bm == "Ferritin" ~ "Dashed line: Ferritin <30 µg/L (log₁₀ axis)",
    bm == "TSAT"     ~ "Dashed line: TSAT <20%",
    bm == "Hb"       ~ "Dashed: Hb<110 (BL); Dotted: Hb<105 (FU)",
    bm == "sTfR"     ~ "No threshold displayed (log₁₀ axis)",
    TRUE             ~ NA_character_
  )
  
  # Scales — log10 for Ferritin and sTfR
  p <- p + (
    if (bm %in% c("Ferritin", "sTfR"))
      scale_y_continuous(trans = "log10")
    else
      scale_y_continuous()
  ) +
    labs(x = NULL, y = y_lab, title = bm, fill = "Time", color = "Time", subtitle = subtitle_txt) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
  
  return(p)
}

# ================== Build panels (Ferritin → TSAT → sTfR → Hb) ==================
p_ferr <- plot_one("Ferritin", show_legend = FALSE)
p_tsat <- plot_one("TSAT", show_legend = FALSE)
p_stfr <- plot_one("sTfR", show_legend = FALSE)
p_hb   <- plot_one("Hb", show_legend = TRUE)  # only bottom plot shows legend

# ================== Combine & Save ==================
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- p_ferr / p_tsat / p_stfr / p_hb +
    plot_layout(heights = c(1,1,1,1)) +
    plot_annotation(
      title = "Biomarker distributions by group and timepoint",
      subtitle = "Ferritin and sTfR shown on log₁₀ scale; thresholds drawn for Ferritin/TSAT/Hb only"
    )
  ggsave("biomarkers_split_violin_ordered_onelegend.png", combined, width = 8, height = 13, dpi = 300)
} else {
  ggsave("ferritin_split_violin.png", p_ferr, width = 8, height = 4, dpi = 300)
  ggsave("tsat_split_violin.png",     p_tsat, width = 8, height = 4, dpi = 300)
  ggsave("stfr_split_violin.png",     p_stfr, width = 8, height = 4, dpi = 300)
  ggsave("hb_split_violin.png",       p_hb,   width = 8, height = 4, dpi = 300)
}
