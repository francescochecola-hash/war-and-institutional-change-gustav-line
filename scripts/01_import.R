# scripts/01_import.R
# Import raw Stata datasets (.dta) and save them as .rds in data/processed/

suppressPackageStartupMessages({
  library(haven)   # read_dta
  library(fs)      # dir_create, file_exists
  library(tools)   # file_path_sans_ext
})

# Ensure output directory exists
dir_create("data/processed")

# Raw datasets (kept locally)
raw_files <- c(
  "data/raw/replication_data_additional_data.dta",
  "data/raw/replication_data_sample_analysis.dta",
  "data/raw/data_final_wow.dta"
)

import_and_save <- function(path) {
  if (!file_exists(path)) {
    stop(paste("Missing file:", path))
  }

  message("Reading: ", path)
  df <- read_dta(path)

  out_name <- paste0(file_path_sans_ext(basename(path)), ".rds")
  out_path <- file.path("data/processed", out_name)

  saveRDS(df, out_path)
  message("Saved:  ", out_path)
}

invisible(lapply(raw_files, import_and_save))
message("All datasets imported successfully.")
