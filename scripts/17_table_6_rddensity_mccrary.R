# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 17_table_6_rddensity_mccrary.R
# Purpose:
#   Run density test at cutoff (McCrary-style) using rddensity and export results.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Outputs: results/tables/test_rddensity_mccrary.csv
#          results/figures/figure_rddensity.png
# Notes:
#   - Running variable: signed distance to Gustav Line (km), cutoff = 0
#   - Excludes municipalities exactly on the line (dist_gustav_km == 0)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rddensity)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_file <- "data/processed/merge/gustav_line_dataset.rds"

results_dir <- "results"
tables_dir  <- file.path(results_dir, "tables")
figures_dir <- file.path(results_dir, "figures")

out_csv <- file.path(tables_dir, "test_rddensity_mccrary.csv")
out_png <- file.path(figures_dir, "figure_rddensity.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load + build signed running variable
# ------------------------------------------------------------------------------
df <- readRDS(in_file)

df <- df %>%
  mutate(gagliarducci_gustav = suppressWarnings(as.integer(as.character(gagliarducci_gustav)))) %>%
  filter(!is.na(dist_gustav_km), dist_gustav_km > 0) %>%  # exclude dist==0
  filter(!is.na(gagliarducci_gustav)) %>%
  mutate(x = ifelse(gagliarducci_gustav == 1L, dist_gustav_km, -dist_gustav_km)) %>%
  filter(!is.na(x))

if (sum(df$x < 0) == 0 || sum(df$x > 0) == 0) {
  stop("No support on one side of the cutoff for x. Check construction of x.")
}

# ------------------------------------------------------------------------------
# Density test at cutoff 0
# ------------------------------------------------------------------------------
den <- rddensity(X = df$x, c = 0)
sum_den <- summary(den)

# ------------------------------------------------------------------------------
# Extract robust p-value (version-robust)
#   Many rddensity versions do NOT expose sum_den$test in a consistent way.
#   We therefore:
#   1) Try sum_den$test with flexible column matching, then
#   2) Fallback to parsing printed summary output (stable across versions).
# ------------------------------------------------------------------------------
p_val <- NA_real_

# 1) Try structured extraction if available
if (!is.null(sum_den$test)) {
  test_mat <- as.data.frame(sum_den$test)
  colnames_lower <- tolower(colnames(test_mat))
  
  # Prefer columns that look like a p-value column
  p_candidates <- which(grepl("^p", colnames_lower) | grepl("p\\s*>\\s*\\|t\\|", colnames_lower) | grepl("p\\|", colnames_lower))
  
  if (length(p_candidates) > 0) {
    p_val_try <- suppressWarnings(as.numeric(test_mat[1, p_candidates[1]]))
    if (is.finite(p_val_try)) p_val <- p_val_try
  }
}

# 2) Fallback: parse the printed summary (very robust across versions)
if (!is.finite(p_val)) {
  txt <- capture.output(summary(den))
  
  # Find the line containing "Robust" and extract the last numeric on the line (the p-value)
  robust_line <- txt[grepl("^\\s*Robust\\s+", txt)]
  
  if (length(robust_line) > 0) {
    nums <- regmatches(robust_line[1], gregexpr("[-+]?[0-9]*\\.?[0-9]+", robust_line[1]))[[1]]
    # expected: T then p-value -> take the last one
    if (length(nums) >= 2) {
      p_val_try <- suppressWarnings(as.numeric(tail(nums, 1)))
      if (is.finite(p_val_try)) p_val <- p_val_try
    }
  }
}

# ------------------------------------------------------------------------------
# Save CSV
# ------------------------------------------------------------------------------
out <- tibble::tibble(
  test = "rddensity (McCrary-style)",
  cutoff = 0,
  N = length(df$x),
  N_left = sum(df$x < 0),
  N_right = sum(df$x > 0),
  p_value_robust = p_val
)

readr::write_csv(out, out_csv, na = "", eol = "\n")
message("Saved: ", out_csv)

# ------------------------------------------------------------------------------
# Plot density around cutoff
#   NOTE: rdplotdensity() signature differs across versions; some do NOT accept c=
# ------------------------------------------------------------------------------
png(filename = out_png, width = 1200, height = 800, res = 150)

ok <- TRUE
tryCatch(
  {
    # Try with c (newer versions)
    rddensity::rdplotdensity(den, X = df$x, c = 0)
  },
  error = function(e) {
    ok <<- FALSE
  }
)

if (!ok) {
  # Retry without c (older versions)
  rddensity::rdplotdensity(den, X = df$x)
}

dev.off()
message("Saved: ", out_png)

print(out)