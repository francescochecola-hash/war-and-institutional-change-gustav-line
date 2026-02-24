# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 13_table_2_balance_checks.R
# Purpose:
#   Run balance checks (covariate continuity) around the Gustav Line using a linear polynomial
#   in longitude and latitude, and export Table-2 results (coef, robust SE, Conley SE, Adj. R2).
#   The sample is restricted to municipalities within 100 km from the Gustav Line.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table2_balance_checks.csv
# Notes:
#   - Treatment: gagliarducci_gustav (1 = North of Gustav Line)
#   - Controls: gagliarducci_longitude, gagliarducci_latitude (linear)
#   - SEs: HC1 robust; Conley (spatial HAC) if the fixest package is available
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)    # data manipulation
  library(tidyr)    # data reshaping
  library(readr)    # csv export
  library(broom)    # model tidying (kept for extensibility)
  library(sandwich) # robust vcov
  library(lmtest)   # coeftest
  library(fs)       # filesystem utilities
  library(here)     # robust file paths relative to project root
})

# Paths
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")
out_csv     <- here("results", "tables", "table2_balance_checks.csv")

dir_create(results_dir)
dir_create(tables_dir)

# Load data
df <- readRDS(in_file)

req_vars <- c("distance_km", "gagliarducci_gustav", "gagliarducci_longitude", "gagliarducci_latitude")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Restrict sample: within 100 km, exclude exactly-on-the-line observations
df <- df %>%
  filter(!is.na(distance_km)) %>%
  filter(distance_km > 0, distance_km <= 100)

# Table structure
table_spec <- tibble::tribble(
  ~panel,                           ~var,                                   ~label,
  
  # --- Geographic Factors ---
  "Geographic Factors",             "gagliarducci_mun_elev",                 "Elevation of the City Hall",
  "Geographic Factors",             "fontana_alt_max",                       "Max. elevation of the Municipality",
  
  # --- Demo-economic variables ---
  "Demo-economic variables",        "gagliarducci_popres_1951_tot",          "Log Population (1951)",
  "Demo-economic variables",        "gagliarducci_female_share_1951",        "% Female Population (1951)",
  "Demo-economic variables",        "gagliarducci_analfshare_1951_tot",      "% Illiterates (1951)",
  "Demo-economic variables",        "fontana_pop_agr_princ_1929_shpop",      "% Agricultural workers (1929)",
  "Demo-economic variables",        "fontana_az_agricole_1929_shpop",        "% Farms (1929)",
  "Demo-economic variables",        "fontana_bestiame_1929_shpop",           "Log Cattle per capita (1929)",
  "Demo-economic variables",        "fontana_addetti1927_shpop",             "% Industrial workers (1927)",
  "Demo-economic variables",        "fontana_imprese1927_shpop",             "% Plants (1927)",
  
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
  
  # --- Political Attitudes during Fascism ---
  "Political Attitudes during Fascism","fontana_shP_comsocantifasc_En_25_42",  "Left-wing citizens under surveillance",
  "Political Attitudes during Fascism","fontana_shP_Ncomsocantifasc_En_25_42", "Other citizens under surveillance"
) %>%
  distinct(panel, var, .keep_all = TRUE) %>%
  filter(!is.na(var), var != "")

missing_cov <- setdiff(table_spec$var, names(df))
if (length(missing_cov) > 0) {
  stop("These balance-check covariates are missing in the dataset:\n- ",
       paste(missing_cov, collapse = "\n- "))
}

# Transformations to match the paper:
# - % variables to percentage points (x100) based on label
# - log transforms for the variables labeled as logs
pct_vars <- table_spec %>%
  filter(grepl("^%\\s", label)) %>%
  pull(var) %>%
  unique()

df <- df %>%
  mutate(across(all_of(pct_vars), ~ .x * 100))

log_pop_1951_var <- "gagliarducci_popres_1951_tot"
log_cattle_var   <- "fontana_bestiame_1929_shpop"

df <- df %>%
  mutate(
    gagliarducci_log_pop_1951 = ifelse(!is.na(.data[[log_pop_1951_var]]) & .data[[log_pop_1951_var]] > 0,
                                       log(.data[[log_pop_1951_var]]), NA_real_),
    fontana_log_cattle_pc_1929 = ifelse(!is.na(.data[[log_cattle_var]]) & .data[[log_cattle_var]] > 0,
                                        log(.data[[log_cattle_var]]), NA_real_)
  )

table_spec <- table_spec %>%
  mutate(
    var = case_when(
      var == log_pop_1951_var ~ "gagliarducci_log_pop_1951",
      var == log_cattle_var   ~ "fontana_log_cattle_pc_1929",
      TRUE ~ var
    )
  )

# Calculate Conley Standard Error
have_fixest <- requireNamespace("fixest", quietly = TRUE)

conley_se <- function(formula, data, coef_name,
                      lat = "gagliarducci_latitude",
                      lon = "gagliarducci_longitude") {
  if (!have_fixest) return(NA_real_)
  
  m <- fixest::feols(formula, data = data, vcov = fixest::vcov_conley(lat = lat, lon = lon))
  ct <- fixest::coeftable(m)
  
  if (!(coef_name %in% rownames(ct))) return(NA_real_)
  as.numeric(ct[coef_name, "Std. Error"])
}

# Single regression runner
run_balance <- function(y_var, y_label, panel_name) {
  
  d <- df %>%
    select(all_of(c(y_var, "gagliarducci_gustav", "gagliarducci_longitude", "gagliarducci_latitude"))) %>%
    filter(!is.na(.data[[y_var]]))
  
  fml <- as.formula(paste0(y_var, " ~ gagliarducci_gustav + gagliarducci_longitude + gagliarducci_latitude"))
  m <- lm(fml, data = d)
  
  vc <- sandwich::vcovHC(m, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  
  coef_name <- "gagliarducci_gustav"
  beta <- as.numeric(ct[coef_name, "Estimate"])
  se_rob <- as.numeric(ct[coef_name, "Std. Error"])
  
  se_conley <- conley_se(fml, d, coef_name = coef_name)
  
  adj_r2 <- summary(m)$adj.r.squared
  
  tibble(
    panel = panel_name,
    variable = y_label,
    Observations = nobs(m),
    Coefficient = beta,
    Robust_SE = se_rob,
    Conley_SE = se_conley,
    Adj_R2 = adj_r2
  )
}

# Run all balance checks
res <- bind_rows(lapply(seq_len(nrow(table_spec)), function(i) {
  run_balance(
    y_var = table_spec$var[i],
    y_label = table_spec$label[i],
    panel_name = table_spec$panel[i]
  )
}))

panel_order <- c(
  "Geographic Factors",
  "Demo-economic variables",
  "Prewar Electoral Outcomes",
  "Political Attitudes during Fascism"
)

res <- res %>%
  mutate(
    panel = factor(panel, levels = panel_order),
    variable = factor(variable, levels = table_spec$label)
  ) %>%
  arrange(panel, variable) %>%
  mutate(
    across(c(Coefficient, Robust_SE, Conley_SE), ~ round(.x, 3)),
    Adj_R2 = round(Adj_R2, 3)
  )

# Export CSV
readr::write_csv(res, out_csv, na = "")
message("Saved table to: ", out_csv)

if (!have_fixest) {
  message("Note: fixest not installed -> Conley_SE column is NA. Install 'fixest' to compute Conley SEs.")
}