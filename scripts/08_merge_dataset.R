# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 08_merge_dataset.R
# Purpose:
# Inputs: data/processed/select/gis_gustav_distance_selected.rds
#         data/processed/select/fontana_et_al_selected.rds
#         data/processed/select/gagliarducci_et_al_selected.rds
#         data/processed/select/referendum_1946_selected.rds
# Output: data/processed/merge/gustav_line_dataset.rds
# Notes:
#   - Master dataset: gis_gustav_distance_selected.rds (8101 obs.)
#   - Merge key: cod_istat103 (ISTAT municipality code)
#   - Non-key columns from the other datasets are prefixed to avoid name collisions
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_dir  <- "data/processed/select"
out_dir <- "data/processed/merge"
out_rds <- file.path(out_dir, "gustav_line_dataset.rds")

# Ensure output directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------
prefix_non_key <- function(df, prefix, key = "cod_istat103") {
  df %>%
    rename_with(
      .fn = ~ ifelse(.x == key, .x, paste0(prefix, .x)),
      .cols = everything()
    )
}

assert_no_duplicates <- function(df, key = "cod_istat103", df_name = "dataset") {
  dup <- df %>% count(.data[[key]]) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop(sprintf("Duplicated keys found in %s on '%s'. Please resolve before merging.", df_name, key))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# 1) Read MASTER (GIS): 8101 obs
# ------------------------------------------------------------------------------
gis <- readRDS(file.path(in_dir, "gis_gustav_distance_selected.rds")) %>%
  mutate(
    cod_istat103 = as.integer(pro_com)  # pro_com is ISTAT municipality code in GIS
  )

assert_no_duplicates(gis, key = "cod_istat103", df_name = "gis_gustav_distance_selected.rds")

# ------------------------------------------------------------------------------
# 2) Read Fontana et al.
# ------------------------------------------------------------------------------
fontana <- readRDS(file.path(in_dir, "fontana_et_al_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(fontana, key = "cod_istat103", df_name = "fontana_et_al_selected.rds")
fontana_p <- prefix_non_key(fontana, "fontana_")

# ------------------------------------------------------------------------------
# 3) Read Gagliarducci et al.
# ------------------------------------------------------------------------------
gagliarducci <- readRDS(file.path(in_dir, "gagliarducci_et_al_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(gagliarducci, key = "cod_istat103", df_name = "gagliarducci_et_al_selected.rds")
gagliarducci_p <- prefix_non_key(gagliarducci, "gagliarducci_")

# ------------------------------------------------------------------------------
# 4) Read Referendum 1946
# ------------------------------------------------------------------------------
referendum <- readRDS(file.path(in_dir, "referendum_1946_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(referendum, key = "cod_istat103", df_name = "referendum_1946_selected.rds")
referendum_p <- prefix_non_key(referendum, "ref46_")

# ------------------------------------------------------------------------------
# Merge (left joins on master)
# ------------------------------------------------------------------------------
merged <- gis %>%
  left_join(fontana_p,      by = "cod_istat103") %>%
  left_join(gagliarducci_p, by = "cod_istat103") %>%
  left_join(referendum_p,   by = "cod_istat103")

# ------------------------------------------------------------------------------
# Diagnostics (coverage)
# ------------------------------------------------------------------------------
coverage <- tibble::tibble(
  master_n = nrow(gis),
  matched_fontana = sum(!is.na(merged$fontana_cod_istat103)),
  matched_gagliarducci = sum(!is.na(merged$gagliarducci_cod_istat103)),
  matched_ref46 = sum(!is.na(merged$ref46_cod_istat103))
)

print(coverage)

# ------------------------------------------------------------------------------
# Save output
# ------------------------------------------------------------------------------
saveRDS(merged, out_rds)
message("Saved merged dataset to: ", out_rds)