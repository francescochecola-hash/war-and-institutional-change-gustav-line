# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 02_import_referendum_1946_data.R
# Purpose: Import raw Italian institutional referendum data (.txt) and save them
#          as R versions (.rds) in the folder "data/processed/"
# Output:  data/processed/referendum_1946.rds
# Notes:   Raw data are intentionally excluded from the repository (see .gitignore)
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(readr)    # import of delimited text files
  library(dplyr)    # basic data inspection and manipulation
  library(stringr)  # string handling and variable name cleaning
})

# Create output directory if it does not already exist
dir_create("data/processed")

# Raw input file
raw_file <- "data/raw/referendum-19460602.txt"

# Check that the input file exists before proceeding
if (!file_exists(raw_file)) {
  stop(paste("Missing input file:", raw_file))
}

# Read raw referendum dataset
message("Importing: ", raw_file)

df <- read_delim(
  file = raw_file,
  delim = ";",
  quote = "\"",
  trim_ws = TRUE,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Clean (and standardize) variables' names
names(df) <- names(df) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_") |>
  str_replace_all("^_|_$", "")

# Save dataset in .rds format and confirm successful save
out_path <- file.path("data/processed", "referendum_1946.rds")
saveRDS(df, out_path)
message("Saved to:  ", out_path)

message("referendum-19460602.txt imported successfully.")