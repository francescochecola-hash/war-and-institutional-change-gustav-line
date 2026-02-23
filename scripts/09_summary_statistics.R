# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 12_results_table_1_summary_statistics.R
# Purpose:
#   Compute summary statistics for Italy and for municipalities within 100 km of the Gustav Line.
#   Export a dataset (with n. of obs., mean, sd, min, max) for replication.
#
# Input:  data/processed/merge/gustav_line_dataset.rds
# Output: results/tables/table1_summary_stats.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)  # data manipulation and summary statistics
  library(tidyr)  # data reshaping
  library(readr)  # export of csv files
  library(fs)     # filesystem utilities: directory creation
  library(here)   # robust file paths relative to project root
})

# Paths (project-root relative via here())
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")
out_csv     <- here("results", "tables", "table1_summary_stats.csv")

dir_create(results_dir)
dir_create(tables_dir)

# Load data
df <- readRDS(in_file)

if (!("distance_km" %in% names(df))) {
  stop("Variable 'distance_km' not found in gustav_line_dataset.rds")
}

df <- df %>%
  mutate(within_100km = !is.na(distance_km) & distance_km <= 100)

# Table structure
table_spec <- tibble::tribble(
  ~panel,                           ~var,                                   ~label,
  
  # --- Geographic Factors ---
  "Geographic Factors",             "gagliarducci_mun_elev",                 "Elevation of the City Hall",
  "Geographic Factors",             "fontana_alt_max",                       "Max. elevation of the Municipality",
  
  # --- Demographic and Economic Factors ---
  "Demographic and Economic Factors","fontana_popres_1921_tot",              "Population (1921)",
  "Demographic and Economic Factors","fontana_popres_1921_f",                "% Female Population (1921)",
  "Demographic and Economic Factors","fontana_analfshare_1921_tot",          "% Illiterates (1921)",
  "Demographic and Economic Factors","gagliarducci_popres_1951_tot",         "Population (1951)",
  "Demographic and Economic Factors","gagliarducci_female_share_1951",       "% Female Population (1951)",
  "Demographic and Economic Factors","gagliarducci_analfshare_1951_tot",     "% Illiterates (1951)",
  "Demographic and Economic Factors","fontana_pop_agr_princ_1929_shpop",     "% Farmworkers (1929)",
  "Demographic and Economic Factors","fontana_az_agricole_1929_shpop",       "% Farmers (1929)",
  "Demographic and Economic Factors","fontana_bestiame_1929_shpop",          "Cattle per capita (1929)",
  "Demographic and Economic Factors","fontana_addetti1927_shpop",            "% Industrial workers (1927)",
  "Demographic and Economic Factors","fontana_imprese1927_shpop",            "% Plants (1927)",
  "Demographic and Economic Factors","fontana_plant_pop_1951",               "% Plants (1951)",
  
  # --- Prewar Electoral Outcomes ---
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_socialisti1919",   "% Socialist 1919",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_cattolici1919",    "% Catholic 1919",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_liberali1919",     "% Liberal 1919",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_socialisti1921",   "% Socialist 1921",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_comunisti1921",    "% Communist 1921",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_cattolici1921",    "% Catholic 1921",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_liberali1921",     "% Liberal 1921",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_fascisti1921",     "% Fascist 1921",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_socialisti1924",   "% Socialist 1924",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_comunisti1924",    "% Communist 1924",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_cattolici1924",    "% Catholic 1924",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_liberali1924",     "% Liberal 1924",
  "Prewar Electoral Outcomes",      "gagliarducci_p_voti2_fascisti1924",     "% Fascist 1924",
  
  # --- Political Attitude during Fascism ---
  "Political Attitude during Fascism","fontana_shP_comsocantifasc_En_25_42",  "Left-wing citizens under surveillance (1925-42) per 1,000",
  "Political Attitude during Fascism","fontana_shP_Ncomsocantifasc_En_25_42", "Other citizens under surveillance (1925-42) per 1,000"
) %>%
  distinct(panel, var, .keep_all = TRUE) %>%
  filter(!is.na(var), var != "")

# Sanity check: variables exist
missing_vars <- setdiff(table_spec$var, names(df))
if (length(missing_vars) > 0) {
  stop(
    "These variables are missing in the dataset:\n- ",
    paste(missing_vars, collapse = "\n- ")
  )
}

# Convert shares to percentage points (x100)
pct_vars <- table_spec %>%
  filter(grepl("^%\\s", label)) %>%
  pull(var) %>%
  unique()

df <- df %>%
  mutate(across(all_of(pct_vars), ~ .x * 100))

# Summary-stat helpers
summ_one <- function(data, var) {
  x <- data[[var]]
  tibble(
    Observations = sum(!is.na(x)),
    Mean         = mean(x, na.rm = TRUE),
    SD           = sd(x, na.rm = TRUE),
    Min          = suppressWarnings(min(x, na.rm = TRUE)),
    Max          = suppressWarnings(max(x, na.rm = TRUE))
  )
}

make_panel <- function(data, group_name) {
  bind_rows(lapply(seq_len(nrow(table_spec)), function(i) {
    v   <- table_spec$var[i]
    lab <- table_spec$label[i]
    pnl <- table_spec$panel[i]
    
    summ_one(data, v) %>%
      mutate(panel = pnl, variable = lab, variable_raw = v, group = group_name) %>%
      select(panel, variable, variable_raw, group, everything())
  }))
}

# Compute panels
tab_italy <- make_panel(df, "Italy")
tab_100km <- make_panel(df %>% filter(within_100km), "Within 100 km from Gustav Line")

tab_long <- bind_rows(tab_italy, tab_100km)

# Wide layout + ordering + formatting
panel_order <- c(
  "Geographic Factors",
  "Demographic and Economic Factors",
  "Prewar Electoral Outcomes",
  "Political Attitude during Fascism"
)

tab_wide <- tab_long %>%
  pivot_wider(
    id_cols = c(panel, variable, variable_raw),
    names_from = group,
    values_from = c(Observations, Mean, SD, Min, Max),
    names_glue = "{group}_{.value}"
  ) %>%
  mutate(
    panel = factor(panel, levels = panel_order),
    variable_raw = factor(variable_raw, levels = table_spec$var)
  ) %>%
  arrange(panel, variable_raw) %>%
  select(panel, variable, starts_with("Italy_"), starts_with("Within 100 km from Gustav Line_")) %>%
  mutate(
    across(contains("Observations"), as.integer),
    across(contains("Mean"), ~ round(.x, 3)),
    across(contains("SD"),   ~ round(.x, 3)),
    across(contains("Min"),  ~ round(.x, 3)),
    across(contains("Max"),  ~ round(.x, 3))
  )

# Export CSV
write_csv(tab_wide, out_csv)
message("Saved table to: ", out_csv)