# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 03_import_gis_distance_gustav.R
# Purpose: Import GIS output (QGIS) with municipality-level distance to the
#          Gustav Line and save an R version (.rds) in the folder "data/processed/"
# Output:  data/processed/gustav_distance.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(sf)       # handling and import of spatial vector data (GeoPackage)
  library(dplyr)    # data manipulation and variable construction
  library(stringr)  # string processing and variable name standardization
})

# Create output directory if it does not already exist
dir_create("data/processed")

# Raw input file (QGIS output)
raw_file <- "data/raw/gis/comuni_2001_dist_gustav.gpkg"

# Check that the input file exists
if (!file_exists(raw_file)) {
  stop(paste("Missing input file:", raw_file))
}

# Read GIS dataset
message("Importing: ", raw_file)

gdf <- st_read(raw_file, quiet = TRUE)

# Drop geometry (it is not useful for econometric analysis)
df <- st_drop_geometry(gdf)

# Clean and standardize variable names
names(df) <- names(df) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_") |>
  str_replace_all("^_|_$", "")

# Rename and construct analysis variables (distances from m to km)
df <- df |>
  rename(
    nearest_gustav_municipality = hubname,
    dist_gustav_m = hubdist
  ) |>
  mutate(
    dist_gustav_km = dist_gustav_m / 1000
  )

# Basic sanity checks: dataset size and presence of key variables
message("Rows: ", nrow(df), " | Columns: ", ncol(df))
message("Key variables: ",
        "nearest_gustav_municipality, dist_gustav_m, dist_gustav_km")

# Save processed dataset
out_path <- file.path("data/processed", "gustav_distance_2001.rds")
saveRDS(df, out_path)
message("Saved to:  ", out_path)

message("Gustav Line distance dataset imported successfully.")