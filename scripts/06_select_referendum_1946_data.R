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
  library(fs)       # filesystem utilities
  library(dplyr)    # data manipulation
  library(stringr)  # string processing
})

# Load replication datasets from "data/processed/"
file_referendum <- "data/processed/referendum_1946.rds"
file_fontana    <- "data/processed/replication_data_additional_data.rds"

# Check that the input files exist before proceeding
if (!file_exists(file_referendum)) stop(paste("Missing input file:", file_referendum))
if (!file_exists(file_fontana))    stop(paste("Missing input file:", file_fontana))

message("Loading: ", file_referendum)
df_sample <- readRDS(file_referendum)

message("Loading: ", file_fontana)
df_additional <- readRDS(file_fontana)

# Standardize names for merges (accents/apostrophes/spaces/case)
clean_key <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    toupper() |>
    (\(z) iconv(z, from = "UTF-8", to = "ASCII//TRANSLIT"))() |>
    str_replace_all("[’`]", "'") |>
    str_replace_all("'", "") |>
    str_replace_all("[^A-Z0-9 ]+", " ") |>
    str_squish()
}

# Referendum dataset: keep variables and compute others
ref_sel <- df_sample |>
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
    provincia_key = clean_key(provincia),
    participation_rate_perc = (votanti / elettori) * 100,
    perc_republic           = (numvotisi / (numvotisi + numvotino)) * 100,
    perc_monarchy           = (numvotino / (numvotisi + numvotino)) * 100
  ) |>
  distinct(provincia_key, comune_key, .keep_all = TRUE)

# This dataset does not include ISTAT municipality identifiers (cod_istat103),
# which are required for consistent merging across historical datasets.

# We therefore rely on the master municipality file from Fontana et al. (2023),
# which provides a complete list of Italian municipalities along with
# harmonized ISTAT codes and administrative names as of 2001.
fontana_master <- df_additional |>
  select(cod_istat103, name, province103, region103) |>
  mutate(
    comune_key    = clean_key(name),
    provincia_key = clean_key(province103)
  ) |>
  distinct(cod_istat103, .keep_all = TRUE)

message("Fontana master — Rows: ", nrow(fontana_master))

# Merge strategy:
# Use Fontana et al. (2023) dataset as the master reference (left-hand side),
# retaining the full universe of municipalities and attaching referendum
# outcomes where a valid name-based match is available.
df_final <- fontana_master |>
  left_join(
    ref_sel |>
      select(
        provincia_key, comune_key,
        circoscrizione, provincia, comune,
        elettori, votanti, numvotisi, numvotino,
        participation_rate_perc, perc_republic, perc_monarchy
      ),
    by = c("provincia_key", "comune_key")
  ) |>
  select(
    cod_istat103,
    name, province103, region103,
    circoscrizione, provincia, comune,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy
  )

message("Prepared referendum dataset (master = Fontana additional) — Rows: ", nrow(df_final),
        " | Columns: ", ncol(df_final))

n_unmatched <- sum(is.na(df_final$elettori))
message("Municipalities with missing referendum info (NA): ", n_unmatched)

# Save the selected dataset
out_path <- "data/processed/referendum_1946_selected.rds"
dir_create(path_dir(out_path))

saveRDS(df_final, out_path)
message("Saved to:  ", out_path)

message("Referendum dataset variable selection prepared successfully.")