# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 22_figure_4_rdrobust_plot.R
# Purpose:
#   Create the official rdrobust plot for the outcome vs distance
#   to the Gustav Line (cutoff at 0).
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/figures/figure4_rdrobust_plot.png
# Notes:
#   - Running variable: distance_gustav_km (already signed: North +, South -), cutoff c = 0.
#   - Excludes municipalities exactly on the line (distance_gustav_km == 0).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)     # data manipulation
  library(rdrobust)  # RD tools by Calonico-Cattaneo-Titiunik
  library(here)      # robust project-root paths for replication
})

# Paths
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
figures_dir <- here("results", "figures")
out_png     <- here("results", "figures", "figure4_rdrobust_plot.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# Load data
df <- readRDS(in_file)

# Outcome (auto-detect: 1946 Republic vote share)
nm <- names(df)
cands <- nm[grepl("ref", nm, ignore.case = TRUE) & grepl("46|1946", nm, ignore.case = TRUE)]
cands <- unique(c(
  cands[grepl("repub", cands, ignore.case = TRUE) & grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands[grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands
))
if (length(cands) == 0) stop("Could not auto-detect the 1946 Republic vote share outcome variable.")
outcome_var <- cands[1]
message("Using outcome variable: ", outcome_var)

# Required variables (updated names)
req_vars <- c("distance_gustav_km", "gustav", outcome_var)
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Build running variable x and outcome y
df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav)))) %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%  # exclude exactly-on-the-line
  filter(!is.na(gustav)) %>%
  mutate(
    x = distance_gustav_km,          # already signed (North +, South -)
    y = .data[[outcome_var]]
  ) %>%
  filter(!is.na(x), !is.na(y))

# Scale outcome to percentage points if stored as 0–1 share
if (is.numeric(df$y)) {
  mx <- suppressWarnings(max(df$y, na.rm = TRUE))
  if (is.finite(mx) && mx <= 1.5) {
    df$y <- df$y * 100
    message("Scaled outcome to percentage points (x100).")
  }
}

# Support check
if (sum(df$x < 0, na.rm = TRUE) == 0 || sum(df$x > 0, na.rm = TRUE) == 0) {
  stop("No support on one side of the cutoff for x. Check distance_gustav_km coding.")
}

# Save rdplot to png
png(filename = out_png, width = 1200, height = 800, res = 150)

rdplot(
  y = df$y,
  x = df$x,
  c = 0,
  x.lim = c(-75, 75),
  x.label = "Distance from Gustav Line (km)",
  y.label = "Demand for Institutional Change (%)"
)

dev.off()

message("Saved: ", out_png)