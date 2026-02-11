# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 07_select_gis_distance_gustav_data.R
# Purpose: Select relevant variables from dataset with municipality-level
#          observations on distance from the Gustav Line for subsequent analysis
# Input:  data/processed/import/gustav_distance.rds
# Output: data/processed/select/gis_gustav_distance_selected.rds
# Notes:  Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)     # filesystem utilities: directory creation and file existence checks
  library(dplyr)  # data manipulation and variable construction
})

# Create output directories if they do not already exist
dir_create("data/processed")
dir_create("data/processed/select")

# Load dataset from "data/processed/import/"
file_gustav_dist <- "data/processed/import/gustav_distance.rds"

# Check that the input file exists before proceeding
if (!file_exists(file_gustav_dist)) {
  stop(paste("Missing input file:", file_gustav_dist))
}

message("Loading: ", file_gustav_dist)
df_gustav_dist <- readRDS(file_gustav_dist)

# Select relevant variables (municipality-level)
df_selected <- df_gustav_dist |>
  select(
    cod_rip,
    cod_reg,
    cod_prov,
    pro_com,
    comune,
    shape_leng,
    shape_area,
    area_new,
    nearest_gustav_municipality,
    dist_gustav_m,
    dist_gustav_km
  )

message(
  "Selected variables — Rows: ", nrow(df_selected),
  " | Columns: ", ncol(df_selected)
)

# Save selected dataset
out_path <- "data/processed/select/gis_gustav_distance_selected.rds"
saveRDS(df_selected, out_path)

message("Saved to:  ", out_path)
message("GIS Gustav distance variable selection completed successfully.")