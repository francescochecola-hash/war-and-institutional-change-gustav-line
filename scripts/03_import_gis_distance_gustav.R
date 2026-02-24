# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 03_import_gis_distance_gustav.R
# Purpose: Import GIS output (QGIS) with municipality-level distance to
#          Gustav Line and save an R version (.rds) in the folder
#          "data/processed/import/"
# Output:  data/processed/import/gustav_distance_sf.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(sf)       # handling and import of spatial vector data (GeoPackage)
  library(dplyr)    # data manipulation and variable construction
  library(stringr)  # string processing and variable name standardization
  library(here)     # robust file paths relative to project root
})

# Create output directories if they do not already exist
dir_create(here("data", "processed"))
dir_create(here("data", "processed", "import"))

# Raw input file
raw_file <- here("data", "raw", "gis", "comuni_2001_dist_gustav.gpkg")

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

# Create distance in km
if (!"distance" %in% names(df)) {
  stop(
    "Variable 'distance' is missing from the GIS dataset. ",
    "Available variables are: ",
    paste(names(df), collapse = ", ")
  )
}

df <- df |>
  mutate(distance_km = distance / 1000)

# Create signed running variable
if (!"gustav" %in% names(df)) {
  stop(
    "Variable 'gustav' is missing from the GIS dataset. ",
    "Available variables are: ",
    paste(names(df), collapse = ", ")
  )
}

df <- df |>
  mutate(
    distance_gustav_km = if_else(
      gustav == 0,
      -distance_km,
      distance_km
    )
  )

# Save processed dataset
out_path <- here("data", "processed", "import", "gustav_distance.rds")
saveRDS(df, out_path)

message("Saved to:  ", out_path)
message("comuni_2001_dist_gustav.gpkg imported successfully.")