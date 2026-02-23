# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 01_import_replication_data.R
# Purpose: Import raw Stata datasets (.dta) and save them as R versions (.rds)
#          in the folder "data/processed/import/"
# Output:  data/processed/import/*.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(haven)   # import Stata datasets (.dta)
  library(fs)      # filesystem utilities: directory creation and file existence checks
  library(tools)   # utilities for handling file paths and extensions
  library(here)    # robust file paths relative to project root
})

# Create output directories if they do not already exist
dir_create(here("data", "processed"))
dir_create(here("data", "processed", "import"))

# Raw input datasets (stored locally; excluded from version control)
raw_files <- c(
  here("data", "raw", "replication_data_additional_data.dta"),
  here("data", "raw", "replication_data_sample_analysis.dta"),
  here("data", "raw", "data_final_wow.dta")
)

import_and_save <- function(path) {
  
  # Check that the input file exists before proceeding
  if (!file_exists(path)) {
    stop(paste("Missing input file:", path))
  }
  
  # Read raw Stata dataset
  message("Importing: ", path)
  df <- read_dta(path)
  
  # Construct output filename by replacing .dta with .rds
  out_name <- paste0(file_path_sans_ext(basename(path)), ".rds")
  
  # Define output path in the processed/import directory
  out_path <- here("data", "processed", "import", out_name)
  
  # Save dataset in .rds format and confirm successful save
  saveRDS(df, out_path)
  message("Saved to:  ", out_path)
}

# Import each raw dataset and write a processed .rds version
invisible(lapply(raw_files, import_and_save))
message("All datasets imported successfully.")