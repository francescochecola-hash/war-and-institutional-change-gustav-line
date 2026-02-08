# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 06_select_referendum_1946_data.R
# Purpose: Prepare municipality-level referendum outcomes by:
#          (i) attaching cod_istat103 (needed for merges),
#          (ii) selecting key variables and computing others.
# Output: data/processed/referendum_1946_selected.rds
# Notes: Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)       # filesystem utilities: directory creation and file existence checks
  library(dplyr)    # data manipulation and variable construction
  library(stringr)  # string processing and variable name standardization
})

# Create output directory if it does not already exist
dir_create("data/processed")

# ------------------------------------------------------------------------------
# Inputs
# ------------------------------------------------------------------------------

# Referendum dataset (created by 02_import_referendum_1946_data.R)
ref_path <- "data/processed/referendum_1946.rds"

# Fontana et al. dataset used only as a "crosswalk" to recover cod_istat103
# (created by 01_import_replication_data.R)
fontana_crosswalk_path <- "data/processed/replication_data_additional_data.rds"

if (!file_exists(ref_path)) stop(paste("Missing input file:", ref_path))
if (!file_exists(fontana_crosswalk_path)) stop(paste("Missing input file:", fontana_crosswalk_path))

message("Loading: ", ref_path)
ref_raw <- readRDS(ref_path)

message("Loading (crosswalk): ", fontana_crosswalk_path)
fontana_raw <- readRDS(fontana_crosswalk_path)

# ------------------------------------------------------------------------------
# Helper: standardize names for merges (accents/apostrophes/spaces/case)
# ------------------------------------------------------------------------------

clean_key <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    toupper() |>
    # convert accented chars (e.g., È -> E) to ASCII where possible
    (\(z) iconv(z, from = "UTF-8", to = "ASCII//TRANSLIT"))() |>
    # unify apostrophes and remove punctuation that often breaks exact matches
    str_replace_all("[’`]", "'") |>
    str_replace_all("'", "") |>
    str_replace_all("[^A-Z0-9 ]+", " ") |>
    str_squish()
}

# ------------------------------------------------------------------------------
# Select variables from referendum data + create merge keys
# ------------------------------------------------------------------------------

ref_sel <- ref_raw |>
  select(
    circoscrizione,
    provincia,
    comune,
    elettori,
    votanti,
    numvotisi,
    numvotino
  ) |>
  mutate(
    comune_key    = clean_key(comune),
    provincia_key = clean_key(provincia)
  )

# ------------------------------------------------------------------------------
# Build crosswalk from Fontana additional data: (province, municipality) -> cod_istat103
# ------------------------------------------------------------------------------

fontana_crosswalk <- fontana_raw |>
  select(cod_istat103, name, province103) |>
  mutate(
    comune_key    = clean_key(name),
    provincia_key = clean_key(province103)
  ) |>
  distinct(provincia_key, comune_key, .keep_all = TRUE) |>
  select(cod_istat103, provincia_key, comune_key)

# Attach cod_istat103 to referendum data
ref_with_id <- ref_sel |>
  left_join(fontana_crosswalk, by = c("provincia_key", "comune_key"))

# Quick diagnostic: how many municipalities did not match?
n_unmatched <- sum(is.na(ref_with_id$cod_istat103))
message("Referendum 1946 — unmatched municipalities (missing cod_istat103): ", n_unmatched)

# Compute main variables
ref_final <- ref_with_id |>
  mutate(
    participation_rate_perc = (votanti / elettori) * 100,
    perc_republic           = (numvotisi / (numvotisi + numvotino)) * 100,
    perc_monarchy           = (numvotino / (numvotisi + numvotino)) * 100
  ) |>
  # keep only what we want in the final file
  select(
    cod_istat103,
    circoscrizione, provincia, comune,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy
  )

message("Prepared referendum dataset — Rows: ", nrow(ref_final),
        " | Columns: ", ncol(ref_final))

# Save the selected dataset
out_path <- "data/processed/referendum_1946_selected.rds"
saveRDS(ref_final, out_path)
message("Saved to:  ", out_path)

message("Referendum dataset variable selection prepared successfully.")