# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 02_import_referendum_1946_data.R
# Purpose: Import raw Italian institutional referendum data and save them
#          as R versions (.rds) in the folder "data/processed/"
# Output:  data/processed/referendum_1946.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # file system utilities (file existence checks, directory creation)
  library(readr)    # import of delimited text files
  library(dplyr)    # basic data inspection and manipulation
  library(stringr)  # string handling and variable name cleaning
})

