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
#   - Treatment: gustav (1 = North of Gustav Line)
#   - Controls: longitude, latitude (linear)
#   - SEs: HC1 robust; Conley (spatial HAC)
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

# Stars helper based on |t| thresholds:
# *   if |t| >= 1.64
# **  if |t| >= 1.96
# *** if |t| >= 2.58
star_from_t <- function(t) {
  if (is.na(t)) return("")
  at <- abs(t)
  if (at >= 2.58) return("***")
  if (at >= 1.96) return("**")
  if (at >= 1.64) return("*")
  ""
}

req_vars <- c("distance_gustav_km", "gustav", "gagliarducci_longitude", "gagliarducci_latitude")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

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

# Convert shares to percentage points (x100) based on label
pct_vars <- table_spec %>%
  filter(grepl("^%\\s", label)) %>%
  pull(var) %>%
  unique()

df <- df %>%
  mutate(across(all_of(pct_vars), ~ .x * 100))

# Log transforms to match labels
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

# Calculate Conley Standard Error (if fixest is available)
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
run_balance <- function(y_var, y_label, panel_name, bandwidth_km) {
  
  d <- df %>%
    filter(!is.na(distance_gustav_km)) %>%
    filter(between(distance_gustav_km, -bandwidth_km, bandwidth_km)) %>%
    # exclude exactly-on-the-line observations
    filter(distance_gustav_km != 0) %>%
    select(all_of(c(
      y_var,
      "gustav",
      "gagliarducci_longitude",
      "gagliarducci_latitude"
    ))) %>%
    filter(!is.na(.data[[y_var]]))
  
  fml <- as.formula(paste0(y_var, " ~ gustav + gagliarducci_longitude + gagliarducci_latitude"))
  m <- lm(fml, data = d)
  
  vc <- sandwich::vcovHC(m, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  
  coef_name <- "gustav"
  beta <- as.numeric(ct[coef_name, "Estimate"])
  se_rob <- as.numeric(ct[coef_name, "Std. Error"])
  
  # Stars based on |t| = |beta/se|
  stars_rob <- star_from_t(beta / se_rob)
  
  se_conley <- conley_se(fml, d, coef_name = coef_name)
  stars_conley <- if (is.na(se_conley)) "" else star_from_t(beta / se_conley)
  
  adj_r2 <- summary(m)$adj.r.squared
  
  # format SEs with stars (as strings)
  robust_se_star <- paste0(formatC(se_rob, format = "f", digits = 3), stars_rob)
  conley_se_star <- if (is.na(se_conley)) {
    ""
  } else {
    paste0(formatC(se_conley, format = "f", digits = 3), stars_conley)
  }
  
  tibble(
    panel = panel_name,
    variable = y_label,
    bandwidth_km = bandwidth_km,
    Observations = nobs(m),
    Coefficient = beta,
    Robust_SE = robust_se_star,
    Conley_SE = conley_se_star,
    Adj_R2 = adj_r2
  )
}

# Run balance checks for multiple bandwidths
bandwidths <- c(100, 75, 50)

res <- bind_rows(lapply(bandwidths, function(h) {
  bind_rows(lapply(seq_len(nrow(table_spec)), function(i) {
    run_balance(
      y_var = table_spec$var[i],
      y_label = table_spec$label[i],
      panel_name = table_spec$panel[i],
      bandwidth_km = h
    )
  }))
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
    variable = factor(variable, levels = table_spec$label),
    bandwidth_km = factor(bandwidth_km, levels = c(50, 75, 100))
  ) %>%
  arrange(bandwidth_km, panel, variable) %>%
  mutate(
    Coefficient = round(Coefficient, 3),
    Adj_R2 = round(Adj_R2, 3)
  )

# Export CSV
readr::write_csv(res, out_csv, na = "")
message("Saved table to: ", out_csv)

if (!have_fixest) {
  message("Note: fixest not installed -> Conley_SE column is NA. Install 'fixest' to compute Conley SEs.")
}