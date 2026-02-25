# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 18_table_7_support_symmetry.R
# Purpose:
#   Check support symmetry around the cutoff by counting observations in distance bins.
# Input:   data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/support_symmetry_bins.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

in_file <- "data/processed/merge/gustav_line_dataset.rds"
results_dir <- "results"
tables_dir  <- file.path(results_dir, "tables")
out_csv <- file.path(tables_dir, "support_symmetry_bins.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

df <- readRDS(in_file)

df <- df %>%
  mutate(gagliarducci_gustav = suppressWarnings(as.integer(as.character(gagliarducci_gustav)))) %>%
  filter(!is.na(dist_gustav_km), dist_gustav_km > 0, !is.na(gagliarducci_gustav)) %>%
  mutate(x = ifelse(gagliarducci_gustav == 1L, dist_gustav_km, -dist_gustav_km)) %>%
  filter(!is.na(x))

# bins (km)
bin_w <- 5
x_lim <- 100

tab <- df %>%
  filter(x >= -x_lim, x <= x_lim) %>%
  mutate(
    bin = floor(x / bin_w) * bin_w,
    side = ifelse(x < 0, "South (x<0)", "North (x>0)")
  ) %>%
  group_by(bin, side) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = side, values_from = n, values_fill = 0) %>%
  arrange(bin) %>%
  mutate(
    total = `South (x<0)` + `North (x>0)`,
    north_share = ifelse(total > 0, `North (x>0)` / total, NA_real_)
  )

readr::write_csv(tab, out_csv, na = "", eol = "\n")
message("Saved: ", out_csv)