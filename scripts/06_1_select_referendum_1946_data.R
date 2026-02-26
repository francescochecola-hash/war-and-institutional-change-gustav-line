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
  library(fs)        # filesystem utilities
  library(dplyr)     # data manipulation
  library(stringr)   # string processing
  library(fuzzyjoin) # controlled fuzzy matching
  library(stringdist)# string distances used by fuzzyjoin
  library(tibble)    # for manual_fix tibble
})

# Load replication datasets from "data/processed/"
file_referendum <- "data/processed/import/referendum_1946.rds"
file_fontana    <- "data/processed/import/replication_data_additional_data.rds"

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

# ------------------------------------------------------------------------------
# Referendum dataset: keep variables and compute others
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
    comune_key    = clean_key(comune),
    provincia_key = clean_key(provincia),
    participation_rate_perc = (votanti / elettori) * 100,
    perc_republic           = (numvotisi / (numvotisi + numvotino)) * 100,
    perc_monarchy           = (numvotino / (numvotisi + numvotino)) * 100
  ) |>
  distinct(provincia_key, comune_key, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# Master municipality file (Fontana et al. 2023)
# ------------------------------------------------------------------------------
# This dataset does not include ISTAT municipality identifiers (cod_istat103),
# which are required for consistent merging across historical datasets.
# We therefore rely on the master municipality file from Fontana et al. (2023),
# which provides a complete list of municipalities with harmonized ISTAT codes
# and administrative names as of 2001.
fontana_master <- df_additional |>
  select(cod_istat103, name, province103, region103) |>
  mutate(
    comune_key    = clean_key(name),
    provincia_key = clean_key(province103)
  ) |>
  distinct(cod_istat103, .keep_all = TRUE)

message("Fontana master — Rows: ", nrow(fontana_master))

# ------------------------------------------------------------------------------
# Merge strategy (five-step):
# 1) Safe match on municipality name only (comune_key) WHEN the name is unique
#    in the referendum dataset (reduces issues from province changes over time).
# 2) For remaining unmatched municipalities, fall back to stricter matching on
#    provincia_key + comune_key.
# 3) For remaining unmatched cases, apply a conservative fuzzy match on comune_key
#    (Levenshtein distance <= 1), accepting only unique candidates.
# 4) Final recovery: fuzzy match with distance <= 2 ONLY for sufficiently long
#    names and ONLY when the match is unique (minimizes false positives).
# 4b) Additional recovery for short names (len < 8): fuzzy match within province
#     (dist<=2), accepting only unique candidates.
# ------------------------------------------------------------------------------

# Identify municipality names that are unique in the referendum dataset
ref_unique_names <- ref_sel |>
  count(comune_key, name = "n_ref") |>
  filter(n_ref == 1) |>
  select(comune_key)

# Keep only unique-name referendum observations for the safe name-only join
ref_sel_unique <- ref_sel |>
  semi_join(ref_unique_names, by = "comune_key")

message(
  "Referendum rows (total): ", nrow(ref_sel),
  " | Unique-name rows (used in step 1): ", nrow(ref_sel_unique)
)

# Step 1: safe join on comune_key only (unique names)
df_step1 <- fontana_master |>
  left_join(
    ref_sel_unique |>
      select(
        comune_key,
        circoscrizione, provincia, comune,
        elettori, votanti, numvotisi, numvotino,
        participation_rate_perc, perc_republic, perc_monarchy
      ),
    by = "comune_key"
  )

n_unmatched_step1 <- sum(is.na(df_step1$elettori))
message("After step 1 (safe name-only join) — Missing referendum info (NA): ", n_unmatched_step1)

# Step 2: fallback join for still-unmatched using provincia_key + comune_key
to_fill <- df_step1 |>
  filter(is.na(elettori)) |>
  select(cod_istat103, provincia_key, comune_key)

fallback <- to_fill |>
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
    circoscrizione, provincia, comune,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy
  )

df_step2 <- df_step1 |>
  left_join(fallback, by = "cod_istat103", suffix = c("", "_fb")) |>
  mutate(
    circoscrizione = coalesce(circoscrizione, circoscrizione_fb),
    provincia      = coalesce(provincia, provincia_fb),
    comune         = coalesce(comune, comune_fb),
    elettori       = coalesce(elettori, elettori_fb),
    votanti        = coalesce(votanti, votanti_fb),
    numvotisi      = coalesce(numvotisi, numvotisi_fb),
    numvotino      = coalesce(numvotino, numvotino_fb),
    participation_rate_perc = coalesce(participation_rate_perc, participation_rate_perc_fb),
    perc_republic  = coalesce(perc_republic, perc_republic_fb),
    perc_monarchy  = coalesce(perc_monarchy, perc_monarchy_fb)
  ) |>
  select(-ends_with("_fb"))

n_unmatched_step2 <- sum(is.na(df_step2$elettori))
message("After step 2 (fallback province+name join) — Missing referendum info (NA): ", n_unmatched_step2)

# Step 3: conservative fuzzy match on comune_key (dist <= 1), unique candidates only
still_na <- df_step2 |>
  filter(is.na(elettori)) |>
  select(cod_istat103, comune_key)

message("Starting step 3 (fuzzy dist<=1) — candidates to recover: ", nrow(still_na))

fuzzy_raw <- stringdist_left_join(
  still_na,
  ref_sel |>
    select(
      comune_key,
      circoscrizione, provincia, comune,
      elettori, votanti, numvotisi, numvotino,
      participation_rate_perc, perc_republic, perc_monarchy
    ),
  by = c("comune_key" = "comune_key"),
  method = "lv",
  max_dist = 1,
  distance_col = "dist"
)

fuzzy_unique <- fuzzy_raw |>
  group_by(cod_istat103) |>
  filter(n() == 1) |>
  ungroup()

message("Step 3 (fuzzy dist<=1) — unique matches found: ", nrow(fuzzy_unique))

df_step3 <- df_step2 |>
  left_join(
    fuzzy_unique |>
      select(
        cod_istat103,
        circoscrizione, provincia, comune,
        elettori, votanti, numvotisi, numvotino,
        participation_rate_perc, perc_republic, perc_monarchy
      ),
    by = "cod_istat103",
    suffix = c("", "_fz")
  ) |>
  mutate(
    circoscrizione = coalesce(circoscrizione, circoscrizione_fz),
    provincia      = coalesce(provincia, provincia_fz),
    comune         = coalesce(comune, comune_fz),
    elettori       = coalesce(elettori, elettori_fz),
    votanti        = coalesce(votanti, votanti_fz),
    numvotisi      = coalesce(numvotisi, numvotisi_fz),
    numvotino      = coalesce(numvotino, numvotino_fz),
    participation_rate_perc = coalesce(participation_rate_perc, participation_rate_perc_fz),
    perc_republic  = coalesce(perc_republic, perc_republic_fz),
    perc_monarchy  = coalesce(perc_monarchy, perc_monarchy_fz)
  ) |>
  select(-ends_with("_fz"))

n_unmatched_step3 <- sum(is.na(df_step3$elettori))
message("After step 3 (fuzzy dist<=1) — Missing referendum info (NA): ", n_unmatched_step3)

# Step 4: fuzzy match (dist <= 2) with safeguards (len>=8), unique candidates only
still_na2 <- df_step3 |>
  filter(is.na(elettori)) |>
  mutate(key_len = nchar(comune_key)) |>
  filter(key_len >= 8) |>
  select(cod_istat103, comune_key)

message("Starting step 4 (fuzzy dist<=2, len>=8) — candidates to recover: ", nrow(still_na2))

fuzzy2_raw <- stringdist_left_join(
  still_na2,
  ref_sel |>
    select(
      comune_key,
      circoscrizione, provincia, comune,
      elettori, votanti, numvotisi, numvotino,
      participation_rate_perc, perc_republic, perc_monarchy
    ),
  by = c("comune_key" = "comune_key"),
  method = "lv",
  max_dist = 2,
  distance_col = "dist2"
)

fuzzy2_unique <- fuzzy2_raw |>
  group_by(cod_istat103) |>
  filter(n() == 1) |>
  ungroup()

message("Step 4 (fuzzy dist<=2) — unique matches found: ", nrow(fuzzy2_unique))

df_final <- df_step3 |>
  left_join(
    fuzzy2_unique |>
      select(
        cod_istat103,
        circoscrizione, provincia, comune,
        elettori, votanti, numvotisi, numvotino,
        participation_rate_perc, perc_republic, perc_monarchy
      ),
    by = "cod_istat103",
    suffix = c("", "_fz2")
  ) |>
  mutate(
    circoscrizione = coalesce(circoscrizione, circoscrizione_fz2),
    provincia      = coalesce(provincia, provincia_fz2),
    comune         = coalesce(comune, comune_fz2),
    elettori       = coalesce(elettori, elettori_fz2),
    votanti        = coalesce(votanti, votanti_fz2),
    numvotisi      = coalesce(numvotisi, numvotisi_fz2),
    numvotino      = coalesce(numvotino, numvotino_fz2),
    participation_rate_perc = coalesce(participation_rate_perc, participation_rate_perc_fz2),
    perc_republic  = coalesce(perc_republic, perc_republic_fz2),
    perc_monarchy  = coalesce(perc_monarchy, perc_monarchy_fz2)
  ) |>
  select(-ends_with("_fz2"))

# ----------------------------------------------------------------------
# Manual fix (optional): explicit one-off corrections
# IMPORTANT: we keep keys in df_final for diagnostics and manual fixes.
# ----------------------------------------------------------------------
manual_fix <- tibble(
  comune_key_from = character(),
  comune_key_to   = character()
  # Example (uncomment and edit):
  # comune_key_from = c("FROM_KEY"),
  # comune_key_to   = c("TO_KEY")
)

if (nrow(manual_fix) > 0) {
  df_final <- df_final |>
    left_join(manual_fix, by = c("comune_key" = "comune_key_from")) |>
    mutate(comune_key = coalesce(comune_key_to, comune_key)) |>
    select(-comune_key_to)
  
  manual_fill <- df_final |>
    filter(is.na(elettori)) |>
    select(cod_istat103, comune_key) |>
    left_join(
      ref_sel |>
        select(
          comune_key,
          circoscrizione, provincia, comune,
          elettori, votanti, numvotisi, numvotino,
          participation_rate_perc, perc_republic, perc_monarchy
        ),
      by = "comune_key"
    ) |>
    select(
      cod_istat103,
      circoscrizione, provincia, comune,
      elettori, votanti, numvotisi, numvotino,
      participation_rate_perc, perc_republic, perc_monarchy
    )
  
  df_final <- df_final |>
    left_join(manual_fill, by = "cod_istat103", suffix = c("", "_mf")) |>
    mutate(
      circoscrizione = coalesce(circoscrizione, circoscrizione_mf),
      provincia      = coalesce(provincia, provincia_mf),
      comune         = coalesce(comune, comune_mf),
      elettori       = coalesce(elettori, elettori_mf),
      votanti        = coalesce(votanti, votanti_mf),
      numvotisi      = coalesce(numvotisi, numvotisi_mf),
      numvotino      = coalesce(numvotino, numvotino_mf),
      participation_rate_perc = coalesce(participation_rate_perc, participation_rate_perc_mf),
      perc_republic  = coalesce(perc_republic, perc_republic_mf),
      perc_monarchy  = coalesce(perc_monarchy, perc_monarchy_mf)
    ) |>
    select(-ends_with("_mf"))
}

# ----------------------------------------------------------------------
# Diagnostics: remaining unmatched (keys must exist here)
# ----------------------------------------------------------------------
message("Prepared referendum dataset (master = Fontana additional) — Rows: ", nrow(df_final),
        " | Columns: ", ncol(df_final))

n_unmatched <- sum(is.na(df_final$elettori))
message("Municipalities with missing referendum info (NA): ", n_unmatched)

remaining_unmatched <- df_final |>
  filter(is.na(elettori)) |>
  select(cod_istat103, name, province103, region103, comune_key, provincia_key)

message("Remaining unmatched municipalities (showing up to 50):")
print(remaining_unmatched, n = min(50, nrow(remaining_unmatched)))

# ----------------------------------------------------------------------
# Output: drop merge keys from the saved file (keep them only for debugging)
# ----------------------------------------------------------------------
df_out <- df_final |>
  select(
    cod_istat103,
    name, province103, region103,
    circoscrizione, provincia, comune,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy
  )

out_path <- "data/processed/select/referendum_1946_selected.rds"
dir_create(path_dir(out_path))

saveRDS(df_out, out_path)
message("Saved to:  ", out_path)

message("Referendum dataset variable selection prepared successfully.")