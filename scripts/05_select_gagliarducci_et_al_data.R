# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 05_select_gagliarducci_et_al_data.R
# Purpose: Select relevant variables from Gagliarducci et al. dataset
#          for subsequent analysis
# Input:  data/processed/import/data_final_wow.rds
# Output: data/processed/select/gagliarducci_et_al_selected.rds
# Notes:  Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)     # filesystem utilities: directory creation and file existence checks
  library(dplyr)  # data manipulation and variable construction
})

# Create output directories if they do not already exist
dir_create("data/processed")
dir_create("data/processed/select")

# Load replication dataset from "data/processed/import/"
file_wow <- "data/processed/import/data_final_wow.rds"

# Check that the input file exists before proceeding
if (!file_exists(file_wow)) {
  stop(paste("Missing input file:", file_wow))
}

message("Loading: ", file_wow)
df_wow <- readRDS(file_wow)

# Keep one row per municipality:
# data_final_wow is a panel (monthly observations), so we collapse it to cross-section
# by taking the first available row for each cod_istat103.
df_one_row <- df_wow |>
  distinct(cod_istat103, .keep_all = TRUE)

message(
  "Collapsed to one row per cod_istat103 — Rows: ", nrow(df_one_row),
  " | Columns: ", ncol(df_one_row)
)

# Select variables needed for the analysis
df_selected <- df_one_row |>
  select(
    cod_istat103, cod_prov103, cod_reg103,
    gustav, occupation_NAZI, occupation, tot_bande,
    longitude, latitude, mun_elev,
    analfshare_1951_tot, female_share_1951, popres_1951_tot,
    p_voti2_liberali1919, p_voti2_cattolici1919, p_voti2_socialisti1919,
    p_voti2_fascisti1919, p_voti2_liberali1921, p_voti2_cattolici1921,
    p_voti2_socialisti1921, p_voti2_fascisti1921,
    p_voti2_comunisti1921, p_voti2_liberali1924, p_voti2_cattolici1924,
    p_voti2_socialisti1924, p_voti2_fascisti1924, p_voti2_comunisti1924
  )

message(
  "Selected variables — Rows: ", nrow(df_selected),
  " | Columns: ", ncol(df_selected)
)

# Save the selected dataset
out_path <- "data/processed/select/gagliarducci_et_al_selected.rds"
saveRDS(df_selected, out_path)
message("Saved to:  ", out_path)

message("Gagliarducci et al. variable selection prepared successfully.")