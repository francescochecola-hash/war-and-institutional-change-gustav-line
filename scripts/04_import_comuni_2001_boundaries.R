# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 04_import_comuni_2001_boundaries.R
# Purpose: Import ISTAT 2001 municipal boundaries (GeoPackage, polygons)
#          and save an R version (.rds) in the folder "data/processed/import/"
# Input:   data/raw/gis/comuni_2001_boundaries.gpkg
# Output:  data/processed/import/comuni_2001_boundaries.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)      # filesystem utilities
  library(sf)      # spatial data handling
  library(here)    # project-root file paths
})

# Create output directories if they do not already exist
dir_create(here("data", "processed"))
dir_create(here("data", "processed", "import"))

# Raw input file
raw_file <- here("data", "raw", "gis", "comuni_2001_boundaries.gpkg")

# Check that the input file exists
if (!file_exists(raw_file)) {
  stop("Missing input file: ", raw_file)
}

message("Importing: ", raw_file)

# Optional: inspect layers (useful if gpkg contains multiple layers)
layers_info <- st_layers(raw_file)
print(layers_info)

# If only one layer exists, this is sufficient:
gdf_comuni <- st_read(raw_file, quiet = TRUE)

# Check geometry type
geom_type <- unique(st_geometry_type(gdf_comuni))
message("Geometry type detected: ", paste(geom_type, collapse = ", "))

if (!all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("Geometry is not POLYGON/MULTIPOLYGON. Check the input file.")
}

# Save processed dataset
out_path <- here("data", "processed", "import", "comuni_2001_boundaries.rds")
saveRDS(gdf_comuni, out_path)

message("Saved to: ", out_path)
message("ISTAT 2001 municipal boundaries imported successfully.")