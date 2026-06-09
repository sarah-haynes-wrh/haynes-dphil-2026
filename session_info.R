# Session Information and Package Dependencies
# Thesis: Haynes S (2026) DPhil, University of Oxford
#
# Run this script to record your R session information for reproducibility.
# Output should be saved alongside analysis outputs.

# ── Required packages across all chapters ─────────────────────────────────────

required_packages <- c(
  # Core
  "tidyverse", "readxl",
  # Mixed-effects models (Chapters 4 & 5)
  "lme4", "lmerTest", "emmeans", "broom.mixed", "performance", "pbkrtest",
  # Visualisation
  "ggplot2", "gghalves", "patchwork", "viridis", "RColorBrewer", "scales",
  # Spatial/maps (Chapters 2 & 3)
  "sf", "rnaturalearth", "rnaturalearthdata",
  # Tables and Word export (Chapter 4)
  "flextable", "officer",
  # Statistics (Chapter 5)
  "psych", "corrplot"
)

# ── Check and install missing packages ────────────────────────────────────────

missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All required packages are installed.")
}

# ── Print session info ─────────────────────────────────────────────────────────

sessionInfo()
