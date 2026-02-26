# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 10_merge_dataset.R
# Purpose: Merge municipality-level datasets using the ISTAT municipality identifier.
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
  library(dplyr)  # basic data inspection and manipulation
  library(here)   # robust file paths relative to project root
  library(fs)     # filesystem utilities: directory creation and file existence checks
  library(tibble) # convenient tibble construction for manual fixes
})

# Paths
in_dir  <- here("data", "processed", "select")
out_dir <- here("data", "processed", "merge")
out_rds <- here("data", "processed", "merge", "gustav_line_dataset.rds")

# Ensure output directory exists
dir_create(out_dir)

# Utility functions to ensure a safe merge:
# - prefix_non_key(): prefixes all non-key variables to avoid name collisions.
# - assert_no_duplicates(): checks that cod_istat103 uniquely identifies
#   municipalities in each dataset before merging.
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

# 1) Read master file (2001 gis): 8101 obs
gis <- readRDS(file.path(in_dir, "gis_gustav_distance_selected.rds")) %>%
  mutate(
    cod_istat103 = as.integer(pro_com)  # pro_com is ISTAT municipality code in GIS
  )

assert_no_duplicates(gis, key = "cod_istat103", df_name = "gis_gustav_distance_selected.rds")

# 2) Read Fontana et al.
fontana <- readRDS(file.path(in_dir, "fontana_et_al_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(fontana, key = "cod_istat103", df_name = "fontana_et_al_selected.rds")
fontana_p <- prefix_non_key(fontana, "fontana_")

# 3) Read Gagliarducci et al.
gagliarducci <- readRDS(file.path(in_dir, "gagliarducci_et_al_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(gagliarducci, key = "cod_istat103", df_name = "gagliarducci_et_al_selected.rds")
gagliarducci_p <- prefix_non_key(gagliarducci, "gagliarducci_")

# 4) Read Referendum 1946 dataset
referendum <- readRDS(file.path(in_dir, "referendum_1946_selected.rds")) %>%
  mutate(cod_istat103 = as.integer(cod_istat103))

assert_no_duplicates(referendum, key = "cod_istat103", df_name = "referendum_1946_selected.rds")
referendum_p <- prefix_non_key(referendum, "ref46_")

# Merge (left joins on master dataset)
merged <- gis %>%
  left_join(fontana_p,      by = "cod_istat103") %>%
  left_join(gagliarducci_p, by = "cod_istat103") %>%
  left_join(referendum_p,   by = "cod_istat103")

# Derived variables (used in empirical specifications)
merged <- merged %>%
  mutate(
    # % Female population in 1921
    perc_f_popres_1921 = case_when(
      is.na(fontana_popres_1921_f) | 
        is.na(fontana_popres_1921_tot) |
        fontana_popres_1921_tot == 0 ~ NA_real_,
      TRUE ~ (fontana_popres_1921_f / fontana_popres_1921_tot) * 100
    ),
    
    # Squared coordinates
    longitude2 = gagliarducci_longitude^2,
    latitude2  = gagliarducci_latitude^2,
    
    # Squared distance in km 
    distance_gustav2 = distance_gustav_km^2
  )

# We report basic merge diagnostics to assess coverage relative to the master
# GIS dataset (municipalities with distance to the Gustav Line).
#
# - master_n: total number of municipalities in the master dataset.
# - matched_fontana: number of municipalities successfully matched with
#   Fontana et al. data.
# - matched_gagliarducci: number of municipalities successfully matched with
#   Gagliarducci et al. data.
# - matched_ref46: number of municipalities successfully matched with
#   the 1946 referendum dataset.
#
# A matched observation is defined as a non-missing key variable after the
# left join. These diagnostics help verify merge completeness and detect
# unexpected data loss.
coverage <- tibble(
  master_n = nrow(gis),
  matched_fontana = sum(!is.na(merged$fontana_cod_istat103)),
  matched_gagliarducci = sum(!is.na(merged$gagliarducci_cod_istat103)),
  matched_ref46 = sum(!is.na(merged$ref46_cod_istat103))
)

print(coverage)

# Save output
saveRDS(merged, out_rds)
message("Saved merged dataset to: ", out_rds)