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
# Notes: Variable selection refelcts empirical specifications used in the thesis 
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(dplyr)    # data manipulation and variable construction
})

#-------------------------------------------------------------------------------
# Load replication datasets from "data/processed/"
#-------------------------------------------------------------------------------

file_sample <-  
