# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 22_figure_5_mccrary_plot.R
# Purpose:
#   Plot McCrary density test at cutoff using rddensity.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/figures/figure5_mccrary_plot.png
# Notes:
#   - Running variable: distance_gustav_km (already signed), cutoff = 0
#   - Excludes municipalities exactly on the line (distance_gustav_km == 0)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)      # data manipulation
  library(rddensity)  # McCrary-style density test
  library(here)       # project-root paths for replication
})

# Paths
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
figures_dir <- here("results", "figures")
out_png     <- here("results", "figures", "figure5_mccrary_plot.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# Load data
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Build running variable
df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav)))) %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%
  filter(!is.na(gustav)) %>%
  mutate(x = distance_gustav_km) %>%
  filter(!is.na(x))

if (sum(df$x < 0) == 0 || sum(df$x > 0) == 0) {
  stop("No support on one side of the cutoff for x.")
}

# Run rddensity
den <- rddensity(X = df$x, c = 0)

# Save density plot
png(filename = out_png, width = 1200, height = 800, res = 150)

ok <- TRUE
tryCatch(
  {
    rddensity::rdplotdensity(den, X = df$x, c = 0)
  },
  error = function(e) {
    ok <<- FALSE
  }
)

if (!ok) {
  rddensity::rdplotdensity(den, X = df$x)
}

dev.off()

message("Saved: ", out_png)