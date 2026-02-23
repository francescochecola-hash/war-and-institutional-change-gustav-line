# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 08_select_referendum_1946_data.R
# Purpose: Prepare municipality-level outcomes from the 1946 Italian referendum by:
#          (1) harmonizing municipality names to a reference administrative year (2001);
#          (2) generating robust text keys for merging (name/province cleaning);
#          (3) attaching harmonized ISTAT municipality identifiers (cod_istat103)
#          via the dataset by Fontana et al., 2023;
#          (4) selecting core variables and computing derived measures used in the thesis.
# Inputs: data/processed/import/referendum_1946.rds
#         data/processed/import/replication_data_additional_data.rds
# Output: data/processed/select/referendum_1946_selected.rds
# Notes: Variable selection reflects empirical specifications used in the thesis
# ==============================================================================

suppressPackageStartupMessages({
  library(fs)        # filesystem utilities: directory creation and file existence checks
  library(dplyr)     # data manipulation and variable construction
  library(stringr)   # string processing and variable name standardization
  library(fuzzyjoin) # controlled fuzzy matching joins
  library(stringdist)# string distances used by fuzzyjoin
  library(tibble)    # convenient tibble construction for manual fixes
  library(here)      # robust file paths relative to project root
})

# Paths

file_referendum <- here("data", "processed", "import", "referendum_1946.rds")
file_fontana    <- here("data", "processed", "import", "replication_data_additional_data.rds")

# Check that the input files exist before proceeding
if (!file_exists(file_referendum)) stop("Missing input file: ", file_referendum)
if (!file_exists(file_fontana))    stop("Missing input file: ", file_fontana)

message("Loading: ", file_referendum)
df_sample <- readRDS(file_referendum)

message("Loading: ", file_fontana)
df_additional <- readRDS(file_fontana)


# 1) Standardize text keys for merges and administrative harmonization 
#    toward reference year (2001)

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

admin_adjustments <- tribble(
  ~type,              ~comune_from,                 ~comune_to,
  
  # ---- Name changes ----
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
  "name_change",      "TRENTOLA DUCENTA",           "TRENTOLA DUCENTA", # harmless safeguard
  
  # ---- Disaggregation handling (choose one successor municipality) ----
  "disaggregation",   "ARIENZO SAN FELICE",         "ARIENZO",
  "disaggregation",   "FERTILIA",                   "CASALUCE"
) |>
  mutate(
    comune_from_key = clean_key(comune_from),
    comune_to_key   = clean_key(comune_to)
  ) |>
  select(type, comune_from, comune_to, comune_from_key, comune_to_key) |>
  distinct(comune_from_key, .keep_all = TRUE)

# 2) 1946 Referendum dataset: select variables, apply admin harmonization, compute rates

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

# Master municipality file (Fontana et al., 2023) — reference year 2001
fontana_master <- df_additional |>
  select(cod_istat103, name, province103, region103) |>
  mutate(
    comune_key    = clean_key(name),
    provincia_key = clean_key(province103)
  ) |>
  distinct(cod_istat103, .keep_all = TRUE)

message("Fontana master — Rows: ", nrow(fontana_master))

# 3) Merge strategy (multi-step): from most conservative to more permissive

ref_unique_names <- ref_sel |>
  count(comune_key, name = "n_ref") |>
  filter(n_ref == 1) |>
  select(comune_key)

ref_sel_unique <- ref_sel |>
  semi_join(ref_unique_names, by = "comune_key")

message(
  "Referendum rows (total): ", nrow(ref_sel),
  " | Unique-name rows (used in step 1): ", nrow(ref_sel_unique)
)

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

# 6) Manual fix (optional): explicit one-off corrections

manual_fix <- tibble(
  comune_key_from = character(),
  comune_key_to   = character()
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

# 7) Diagnostics: remaining unmatched (keep keys for debugging)

message(
  "Prepared referendum dataset (master = Fontana additional) — Rows: ", nrow(df_final),
  " | Columns: ", ncol(df_final)
)

n_unmatched <- sum(is.na(df_final$elettori))
message("Municipalities with missing referendum info (NA): ", n_unmatched)

remaining_unmatched <- df_final |>
  filter(is.na(elettori)) |>
  select(cod_istat103, name, province103, region103, comune_key, provincia_key)

message("Remaining unmatched municipalities (showing up to 50):")
print(remaining_unmatched, n = min(50, nrow(remaining_unmatched)))

# 8) Output: drop merge keys from the saved file (keep them only for debugging)

df_out <- df_final |>
  select(
    cod_istat103,
    province103, region103,
    circoscrizione, provincia, comune,
    elettori, votanti, numvotisi, numvotino,
    participation_rate_perc, perc_republic, perc_monarchy
  )

# Output path (project-root relative)
out_path <- here("data", "processed", "select", "referendum_1946_selected.rds")
dir_create(path_dir(out_path))

saveRDS(df_out, out_path)
message("Saved to:  ", out_path)

message("Referendum dataset variable selection prepared successfully.")