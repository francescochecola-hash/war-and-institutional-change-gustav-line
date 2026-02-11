# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 06_select_referendum_1946_data.R
# Purpose: Prepare municipality-level outcomes from the 1946 Italian referendum by:
#          (1) harmonizing municipality names to a reference administrative year (2001);
#          (2) generating robust text keys for merging (name/province cleaning);
#          (3) attaching harmonized ISTAT municipality identifiers (cod_istat103)
#              via the dataset by Fontana et al. (2023);
#          (4) selecting core variables and computing derived measures used in the thesis.
# Inputs: data/processed/import/referendum_1946.rds
#         data/processed/import/replication_data_additional_data.rds
# Output: data/processed/select/referendum_1946_selected.rds
# Notes: Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
file_referendum <- "data/processed/import/referendum_1946.rds"
file_fontana    <- "data/processed/import/replication_data_additional_data.rds"

if (!file_exists(file_referendum)) stop(paste("Missing input file:", file_referendum))
if (!file_exists(file_fontana))    stop(paste("Missing input file:", file_fontana))

message("Loading: ", file_referendum)
df_sample <- readRDS(file_referendum)

message("Loading: ", file_fontana)
df_additional <- readRDS(file_fontana)

# ------------------------------------------------------------------------------
# Clean key helper
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Manual admin adjustments
# ------------------------------------------------------------------------------
admin_adjustments <- tribble(
  ~type,              ~comune_from,                 ~comune_to,
  
  "name_change",      "BOIANO",                     "BOJANO",
  "name_change",      "BORGOCOLLEFEGATO",           "BORGOROSE",
  "name_change",      "BROCCO",                     "BROCCOSTELLA",
  "name_change",      "CASAMICCIOLA",               "CASAMICCIOLA TERME",
  "name_change",      "CASTELLO D ALIFE",           "CASTELLO DEL MATESE",
  "name_change",      "CONTURSI",                   "CONTURSI TERME",
  "name_change",      "RESINA",                     "ERCOLANO",
  "name_change",      "GALLO",                      "GALLO MATESE",
  "name_change",      "PIANA DI CAIAZZO",           "PIANA DI MONTE VERNA",
  "name_change",      "PIEDIMONTE D ALIFE",         "PIEDIMONTE MATESE",
  "name_change",      "PETRURO",                    "PETRURO IRPINO",
  "name_change",      "RIPALIMOSANO",               "RIPALIMOSANI",
  "name_change",      "SAN GREGORIO",               "SAN GREGORIO MATESE",
  "name_change",      "SAN NAZZARO CALVI",          "SAN NAZZARO",
  "name_change",      "SANT ANDREA",                "SANT ANDREA DEL GARIGLIANO",
  "name_change",      "SESSANO",                    "SESSANO DEL MOLISE",
  "name_change",      "TRENTOLA",                   "TRENTOLA DUCENTA",
  "name_change",      "TRENTOLA DUCENTA",           "TRENTOLA DUCENTA",
  
  "disaggregation",   "ARIENZO SAN FELICE",         "ARIENZO",
  "disaggregation",   "FERTILIA",                   "CASALUCE"
) |>
  mutate(
    comune_from_key = clean_key(comune_from),
    comune_to_key   = clean_key(comune_to)
  ) |>
  select(type, comune_from, comune_to, comune_from_key, comune_to_key) |>
  distinct(comune_from_key, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 2) 1946 Referendum dataset
# ------------------------------------------------------------------------------
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
    comune_raw     = comune,
    provincia_raw  = provincia,
    comune_key_raw = clean_key(comune_raw),
    provincia_key  = clean_key(provincia_raw)
  ) |>
  left_join(
    admin_adjustments |>
      select(comune_from_key, comune_to, comune_to_key),
    by = c("comune_key_raw" = "comune_from_key")
  ) |>
  mutate(
    comune_harmonized = coalesce(comune_to, comune_raw),
    comune_key        = coalesce(comune_to_key, comune_key_raw),
    
    participation_rate_perc = (votanti / elettori) * 100,
    perc_republic           = (numvotisi / (numvotisi + numvotino)) * 100,
    perc_monarchy           = (numvotino / (numvotisi + numvotino)) * 100
  ) |>
  select(
    circoscrizione,
    provincia = provincia_raw,
    comune    = comune_harmonized,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy,
    comune_key, provincia_key,
    comune_raw, comune_key_raw
  ) |>
  distinct(provincia_key, comune_key, .keep_all = TRUE)

message("Referendum selected — Rows: ", nrow(ref_sel),
        " | Columns: ", ncol(ref_sel))

# ------------------------------------------------------------------------------
# Fontana master (reference year 2001)
# ------------------------------------------------------------------------------
fontana_master <- df_additional |>
  select(cod_istat103, name, province103, region103) |>
  mutate(
    comune_key    = clean_key(name),
    provincia_key = clean_key(province103)
  ) |>
  distinct(cod_istat103, .keep_all = TRUE)

message("Fontana master — Rows: ", nrow(fontana_master))

# ------------------------------------------------------------------------------
# Exact merge
# ------------------------------------------------------------------------------
out <- ref_sel |>
  left_join(
    fontana_master |>
      select(cod_istat103, province103, region103,
             comune_key, provincia_key),
    by = c("comune_key", "provincia_key")
  )

# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------
message("Prepared referendum dataset — Rows: ", nrow(out),
        " | Columns: ", ncol(out))

n_unmatched <- sum(is.na(out$cod_istat103))
message("Municipalities with missing cod_istat103 (NA): ", n_unmatched)

remaining_unmatched <- out |>
  filter(is.na(cod_istat103)) |>
  select(provincia, comune) |>
  distinct()

message("Remaining unmatched municipalities (showing up to 50):")
print(remaining_unmatched, n = min(50, nrow(remaining_unmatched)))

# ------------------------------------------------------------------------------
# Save output
# ------------------------------------------------------------------------------
out_path <- "data/processed/select/referendum_1946_selected.rds"

dir_create(path_dir(out_path))
saveRDS(out, out_path)

message("Saved to:  ", out_path)
message("Referendum dataset merge completed successfully.")