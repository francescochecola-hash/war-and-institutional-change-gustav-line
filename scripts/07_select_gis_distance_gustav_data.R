# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 07_select_gis_distance_gustav_data.R
# Purpose: Select relevant variables from dataset with municipality-level
#          observations distance from Gustav Line replication datasets
#          for subsequent analysis 
# Input: data/processed/gustav_distance.rds
# Output: data/processed/gis_gustav_distance_selected.rds
# Notes: Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)     # filesystem utilities: directory creation and file existence checks
  library(dplyr)  # data manipulation and variable construction
})

# Load dataset from "data/processed/"
file_gustav_dist <- "data/processed/gustav_distance.rds"

# Check that the input file exists before proceeding
if (!file_exists(file_gustav_dist)) {
  stop(paste("Missing input file:", file_gustav_dist))
}

message("Loading: ", file_gustav_dist)
df_gustav_dist <- readRDS(file_gustav_dist)

# Select relevant variables (municipality-level)
df_selected <- df_gustav_dist |>
  select(
    pro_com_t,
    comune_a,
    pop_2001,
    cc_p
  )

message("Selected variables — Rows: ", nrow(df_selected),
        " | Columns: ", ncol(df_selected))

# Save selected dataset
out_path <- "data/processed/gis_gustav_distance_selected.rds"
saveRDS(df_selected, out_path)

message("Saved to:  ", out_path)
message("GIS Gustav distance variable selection completed successfully.")