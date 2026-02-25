# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 17_table_6_rddensity_mccrary.R
# Purpose:
#   Run density test at cutoff (McCrary) using rddensity and export results.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Outputs: results/tables/table6_rddensity_mccrary.csv
# Notes:
#   - Running variable: distance to Gustav Line (km), cutoff = 0
#   - Excludes municipalities exactly on the line (distance_gustav_km == 0)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)      # data manipulation
  library(readr)      # CSV export
  library(rddensity)  # McCrary density test for RD running variables
  library(here)       # robust project-root paths for replication
})

# Paths (project-root relative via here())
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")

out_csv <- here("results", "tables", "table6_rddensity_mccrary.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# Load + build running variable (distance from Gustav Line in km)
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav)))) %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%  # exclude exactly-on-the-line
  filter(!is.na(gustav)) %>%
  mutate(x = distance_gustav_km) %>%  # already signed (North +, South -)
  filter(!is.na(x))

if (sum(df$x < 0) == 0 || sum(df$x > 0) == 0) {
  stop("No support on one side of the cutoff for x. Check distance_gustav_km sign coding.")
}

# Density test at cutoff 0
den <- rddensity(X = df$x, c = 0)
sum_den <- summary(den)

# Extract robust p-value in a version-robust way
p_val <- NA_real_

# 1) Try structured extraction if available
if (!is.null(sum_den$test)) {
  test_mat <- as.data.frame(sum_den$test)
  colnames_lower <- tolower(colnames(test_mat))
  
  p_candidates <- which(
    grepl("^p", colnames_lower) |
      grepl("p\\s*>\\s*\\|t\\|", colnames_lower) |
      grepl("p\\|", colnames_lower) |
      grepl("p-value", colnames_lower)
  )
  
  if (length(p_candidates) > 0) {
    p_val_try <- suppressWarnings(as.numeric(test_mat[1, p_candidates[1]]))
    if (is.finite(p_val_try)) p_val <- p_val_try
  }
}

# 2) Fallback: parse printed summary (robust across versions)
if (!is.finite(p_val)) {
  txt <- capture.output(summary(den))
  robust_line <- txt[grepl("^\\s*Robust\\s+", txt)]
  
  if (length(robust_line) > 0) {
    nums <- regmatches(robust_line[1], gregexpr("[-+]?[0-9]*\\.?[0-9]+", robust_line[1]))[[1]]
    # Usually: "Robust   T-stat   P-value" -> take the last numeric as p-value
    if (length(nums) >= 2) {
      p_val_try <- suppressWarnings(as.numeric(tail(nums, 1)))
      if (is.finite(p_val_try)) p_val <- p_val_try
    }
  }
}

# Save CSV
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