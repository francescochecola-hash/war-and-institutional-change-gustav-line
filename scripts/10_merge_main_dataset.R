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
#         data/processed/select/gustav_line.rds   (LINESTRING, EPSG:32632)
# Output: data/processed/merge/gustav_line_dataset.rds
# Notes:
#   - Master dataset: gis_gustav_distance_selected.rds (8101 obs.)
#   - Merge key: cod_istat103 (ISTAT municipality code)
#   - Computes Euclidean distance from municipality point to Gustav Line
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)   # basic data inspection and manipulation
  library(here)    # robust file paths relative to project root
  library(fs)      # filesystem utilities: directory creation and file existence checks
  library(tibble)  # convenient tibble construction for manual fixes
  library(sf)      # spatial objects & distances
})

# Paths
in_dir  <- here("data", "processed", "select")
out_dir <- here("data", "processed", "merge")
out_rds <- here("data", "processed", "merge", "gustav_line_dataset.rds")

# Ensure output directory exists
dir_create(out_dir)

# Utility functions
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

# 1) Read master file (GIS): 8101 obs
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

# Compute Euclidean distance to Gustav Line (EPSG:32632)
gustav_line_file <- here("data", "processed", "import", "gustav_line.rds")

if (!file_exists(gustav_line_file)) {
  stop("gustav_line.rds not found at: ", gustav_line_file)
}

if (!all(c("gagliarducci_longitude", "gagliarducci_latitude") %in% names(merged))) {
  stop("Missing gagliarducci_longitude and/or gagliarducci_latitude in merged dataset.")
}

# Quick diagnostics: lon/lat ranges (should look like degrees)
message("Longitude range (raw): ", paste(range(merged$gagliarducci_longitude, na.rm = TRUE), collapse = " , "))
message("Latitude range  (raw): ", paste(range(merged$gagliarducci_latitude,  na.rm = TRUE), collapse = " , "))

# Municipalities as POINT sf:
# lon/lat are degrees -> treat as EPSG:4326, then transform to 32632
mun_sf <- merged %>%
  filter(!is.na(gagliarducci_longitude), !is.na(gagliarducci_latitude)) %>%
  st_as_sf(
    coords = c("gagliarducci_longitude", "gagliarducci_latitude"),
    crs = 4326,
    remove = FALSE
  )

# Load Gustav line in EPSG:32632
gustav_line <- readRDS(gustav_line_file)

# If stored as sfc, wrap into sf for consistency
if (inherits(gustav_line, "sfc")) {
  gustav_line <- st_sf(geometry = gustav_line)
}

# Ensure Gustav line CRS is EPSG:32632
if (is.na(st_crs(gustav_line))) {
  st_crs(gustav_line) <- 32632
  message("gustav_line had no CRS -> set to EPSG:32632.")
} else {
  message("gustav_line CRS: ", st_crs(gustav_line)$epsg, " / ", st_crs(gustav_line)$input)
}

# Transform municipality points to EPSG:32632
mun_utm <- st_transform(mun_sf, 32632)

# Euclidean distance point-to-line
dist_m  <- st_distance(mun_utm, gustav_line)
dist_km <- as.numeric(dist_m) / 1000

dist_df <- mun_sf %>%
  st_drop_geometry() %>%
  transmute(
    cod_istat103,
    gustav_euclidean_dist = dist_km
  )

merged <- merged %>%
  left_join(dist_df, by = "cod_istat103")

message("gustav_euclidean_dist (km) summary:")
print(summary(merged$gustav_euclidean_dist))

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
    
    # Quadratic terms for lon/lat polynomials
    longitude2 = gagliarducci_longitude^2,
    latitude2  = gagliarducci_latitude^2,
    long_lat   = gagliarducci_longitude * gagliarducci_latitude,
    
    # Euclidean distance to line (km) squared
    gustav_euclidean_dist2 = gustav_euclidean_dist^2
  )

# Merge diagnostics
coverage <- tibble(
  master_n = nrow(gis),
  matched_fontana = sum(!is.na(merged$fontana_cod_istat103)),
  matched_gagliarducci = sum(!is.na(merged$gagliarducci_cod_istat103)),
  matched_ref46 = sum(!is.na(merged$ref46_cod_istat103)),
  nonmissing_euclid_dist = sum(!is.na(merged$gustav_euclidean_dist))
)

print(coverage)

# Save output
saveRDS(merged, out_rds)
message("Saved merged dataset to: ", out_rds)