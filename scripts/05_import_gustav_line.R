# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 05_import_gustav_line.R
# Purpose: Import Gustav Line geometry (GeoPackage, line layer)
#          and save an R version (.rds) in the folder "data/processed/import/"
# Input:   data/raw/gis/gustav_line.gpkg
# Output:  data/processed/import/gustav_line.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)      # filesystem utilities: directory creation and file existence checks
  library(sf)      # handling and import of spatial vector data (GeoPackage)
  library(here)    # robust file paths relative to project root
})

# Create output directories if they do not already exist
dir_create(here("data", "processed"))
dir_create(here("data", "processed", "import"))

# Raw input file
raw_file <- here("data", "raw", "gis", "gustav_line.gpkg")

# Check that the input file exists
if (!file_exists(raw_file)) {
  stop("Missing input file: ", raw_file)
}

message("Importing: ", raw_file)

# Optional: inspect available layers (useful if gpkg contains multiple layers)
layers_info <- st_layers(raw_file)
print(layers_info)

# If only one layer exists, this is sufficient:
gustav_line <- st_read(raw_file, quiet = TRUE)

# Check geometry type
geom_type <- unique(st_geometry_type(gustav_line))
message("Geometry type detected: ", paste(geom_type, collapse = ", "))

if (!all(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
  stop("Geometry is not LINESTRING/MULTILINESTRING. Check the input file.")
}

# Save processed dataset
out_path <- here("data", "processed", "import", "gustav_line.rds")
saveRDS(gustav_line, out_path)

message("Saved to: ", out_path)
message("Gustav Line imported successfully.")