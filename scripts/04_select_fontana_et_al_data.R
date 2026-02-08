# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 04_select_fontana_et_al_data.R
# Purpose: Select relevant variables from Fontana, Nannicini and Tabellini (2023)
#          replication datasets for subsequent analysis
# Input: data/processed/replication_data_sample_analysis.rds
#        data/processed/replication_data_additional_data.rds
# Output: data/processed/fontana_et_al_selected.rds
# Notes: Variable selection reflects empirical specifications used in the thesis 
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(dplyr)    # data manipulation and variable construction
})

# Load replication datasets from "data/processed/"
file_sample <- "data/processed/replication_data_sample_analysis.rds"
file_additional <- "data/processed/replication_data_additional_data.rds"

# Check that the input files exist
if (!file_exists(file_sample)) {
  stop(paste("Missing input file:", file_sample))
}
if (!file_exists(file_additional)) {
  stop(paste("Missing input file:", file_additional))
}

message("Loading: ", file_sample)
df_sample <- readRDS(file_sample)

message("Loading: ", file_additional)
df_additional <- readRDS(file_additional)

# Select variables from replication_data_sample_analysis
df_sample_sel <- df_sample |>
  select(
    name,
    province103,
    region103,
    cod_istat103,
    popres_1921_tot,
    popres_1921_f,
    analfshare_1921_tot,
    popres_1951_tot,
    popres_1951_f,
    analfshare_1951_tot,
    longitude,
    latitude,
    alt_max,
    mun_elev,
    latitude2,
    longitude2,
    occupation,
    plant_pop_1951,
    dum15_hgss,
    shP_comsocantifasc_En_25_42,
    shP_Ncomsocantifasc_En_25_42,
    pop_agr_princ_1929_shpop,
    addetti1927_shpop,
    imprese1927_shpop,
    bestiame_1929_shpop,
    az_agricole_1929_shpop
  )

message("Sample analysis data — Rows: ", nrow(df_sample_sel),
        " | Columns: ", ncol(df_sample_sel))

# Select variables from replication_data_additional_data
df_additional_sel <- df_additional |>
  select(
    name,
    province103,
    region103,
    cod_istat103,
    occupation_HGSS,
    n_episode_t_AT,
    occupation
  )

message("Additional data — Rows: ", nrow(df_additional_sel),
        " | Columns: ", ncol(df_additional_sel))

# Combine selected variables (by ISTAT code of municipalities as of 2001)
df_selected <- df_additional_sel |>
  left_join(
    df_sample_sel,
    by = c("cod_istat103", "name", "province103", "region103")
  )

message("Merged Fontana et al. dataset — Rows: ", nrow(df_selected),
        " | Columns: ", ncol(df_selected))

# Save selected dataset
out_path <- "data/processed/fontana_et_al_selected.rds"
saveRDS(df_selected, out_path)

message("Saved to: ", out_path)
message("Fontana et al. variable selection completed successfully.")