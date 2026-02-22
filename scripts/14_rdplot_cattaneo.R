# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 14_rdplot_cattaneo.R
# Purpose:
#   Create official rdplot (Cattaneo et al.) for outcome vs signed distance.
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/figures/figure_rdplot_cattaneo.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(rdrobust)
})

in_file <- "data/processed/merge/gustav_line_dataset.rds"
results_dir <- "results"
figures_dir <- file.path(results_dir, "figures")
out_png <- file.path(figures_dir, "figure_rdplot_cattaneo.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

df <- readRDS(in_file)

# outcome auto-detect
nm <- names(df)
cands <- nm[grepl("ref", nm, ignore.case = TRUE) & grepl("46|1946", nm, ignore.case = TRUE)]
cands <- unique(c(
  cands[grepl("repub", cands, ignore.case = TRUE) & grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands[grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands
))
if (length(cands) == 0) stop("Could not auto-detect outcome.")
outcome_var <- cands[1]
message("Using outcome variable: ", outcome_var)

df <- df %>%
  mutate(gagliarducci_gustav = suppressWarnings(as.integer(as.character(gagliarducci_gustav)))) %>%
  filter(!is.na(dist_gustav_km), dist_gustav_km > 0, !is.na(gagliarducci_gustav)) %>%
  mutate(
    x = ifelse(gagliarducci_gustav == 1L, dist_gustav_km, -dist_gustav_km),
    y = .data[[outcome_var]]
  ) %>%
  filter(!is.na(x), !is.na(y))

# scale to pp if 0-1
mx <- suppressWarnings(max(df$y, na.rm = TRUE))
if (is.finite(mx) && mx <= 1.5) df$y <- df$y * 100

# Save rdplot to PNG
png(filename = out_png, width = 1200, height = 800, res = 150)
rdplot(y = df$y, x = df$x, c = 0, x.lim = c(-75, 75),
       x.label = "Distance from Gustav Line (km)",
       y.label = "Demand for Institutional Change (%)")
dev.off()

message("Saved: ", out_png)