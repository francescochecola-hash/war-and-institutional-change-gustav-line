# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 14_table_3_main_results.R
# Purpose:
#   Replicate Table 3 (Main Results) in the same layout as in the slide:
#   columns (1)-(6) for <100/<75/<50 km (no controls) and <100/<75/<50 km (with controls),
#   for Panel A (poly in longitude/latitude), Panel B (poly in signed distance), and OLS.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table3_main_results.csv
# Notes:
#   - SEs in table: Robust (HC1) and Conley (spatial HAC).
#   - Controls (when ON): Elevation City Hall + log(pop 1951) + %female1951 + %illiterates1951
#     + prewar electoral outcomes (1919/1921/1924).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sandwich)
  library(lmtest)
  library(here)   # robust paths for replication
})

# Paths (project-root relative via here())
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")
out_csv     <- here("results", "tables", "table3_main_results.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Load data + required vars
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav", "gagliarducci_longitude", "gagliarducci_latitude")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Outcome: Republic vote share in 1946
nm <- names(df)
cands <- nm[grepl("ref", nm, ignore.case = TRUE) & grepl("46|1946", nm, ignore.case = TRUE)]
cands <- unique(c(
  cands[grepl("repub", cands, ignore.case = TRUE) & grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands[grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands
))
if (length(cands) == 0) {
  stop("Could not auto-detect the 1946 Republic vote share variable. Set outcome_var manually.")
}
outcome_var <- cands[1]
message("Using outcome variable: ", outcome_var)

# Sample restrictions + running var
# - Use signed distance from GIS: distance_gustav_km
# - Exclude exactly-on-the-line observations (distance == 0)
df <- df %>%
  filter(!is.na(distance_gustav_km)) %>%
  filter(distance_gustav_km != 0) %>%
  mutate(signed_dist_km = distance_gustav_km)

# Scale outcome to percentage points if in [0,1]
if (is.numeric(df[[outcome_var]])) {
  mx <- suppressWarnings(max(df[[outcome_var]], na.rm = TRUE))
  if (is.finite(mx) && mx <= 1.5) {
    df[[outcome_var]] <- df[[outcome_var]] * 100
    message("Scaled outcome to percentage points (x100).")
  }
}

# Controls (ONLY the ones you requested)
base_controls <- c(
  "gagliarducci_mun_elev",
  "gagliarducci_popres_1951_tot",
  "gagliarducci_female_share_1951",
  "gagliarducci_analfshare_1951_tot"
)

prewar_controls <- c(
  "gagliarducci_p_voti2_socialisti1919",
  "gagliarducci_p_voti2_cattolici1919",
  "gagliarducci_p_voti2_liberali1919",
  "gagliarducci_p_voti2_fascisti1919",
  "gagliarducci_p_voti2_socialisti1921",
  "gagliarducci_p_voti2_comunisti1921",
  "gagliarducci_p_voti2_cattolici1921",
  "gagliarducci_p_voti2_liberali1921",
  "gagliarducci_p_voti2_fascisti1921",
  "gagliarducci_p_voti2_socialisti1924",
  "gagliarducci_p_voti2_comunisti1924",
  "gagliarducci_p_voti2_cattolici1924",
  "gagliarducci_p_voti2_liberali1924",
  "gagliarducci_p_voti2_fascisti1924"
)

base_controls   <- intersect(base_controls, names(df))
prewar_controls <- intersect(prewar_controls, names(df))

# Convert shares to percentage points if they look like shares
share_like <- unique(c("gagliarducci_female_share_1951", "gagliarducci_analfshare_1951_tot", prewar_controls))
share_like <- intersect(share_like, names(df))
for (v in share_like) {
  if (is.numeric(df[[v]])) {
    mx <- suppressWarnings(max(df[[v]], na.rm = TRUE))
    if (is.finite(mx) && mx <= 1.5) df[[v]] <- df[[v]] * 100
  }
}

# Log population (1951)
if ("gagliarducci_popres_1951_tot" %in% names(df)) {
  df <- df %>%
    mutate(log_pop_1951 = ifelse(!is.na(gagliarducci_popres_1951_tot) & gagliarducci_popres_1951_tot > 0,
                                 log(gagliarducci_popres_1951_tot), NA_real_))
  base_controls <- setdiff(base_controls, "gagliarducci_popres_1951_tot")
  base_controls <- unique(c(base_controls, "log_pop_1951"))
}

all_controls <- unique(c(base_controls, prewar_controls))
all_controls <- intersect(all_controls, names(df))

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

# Conley SE via fixest (optional)
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

fit_extract <- function(data, fml, coef_name = "gustav") {
  m  <- lm(fml, data = data)
  vc <- sandwich::vcovHC(m, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  
  beta  <- as.numeric(ct[coef_name, "Estimate"])
  se_rob <- as.numeric(ct[coef_name, "Std. Error"])
  se_con <- conley_se(fml, data, coef_name = coef_name)
  
  list(
    n        = nobs(m),
    beta     = beta,
    se_rob   = se_rob,
    se_con   = se_con,
    stars    = star_from_t(beta / se_rob),  # stars based on robust t-stat
    adjr2    = summary(m)$adj.r.squared
  )
}

fmt_cell <- function(beta, se_rob, se_con, stars, digits = 3) {
  if (is.na(beta) || is.na(se_rob)) return("")
  beta_s <- paste0(formatC(beta, format = "f", digits = digits), stars)
  rob_s  <- paste0("(", formatC(se_rob, format = "f", digits = digits), ")")
  con_s  <- if (is.na(se_con)) "" else paste0("[", formatC(se_con, format = "f", digits = digits), "]")
  paste(beta_s, rob_s, con_s, sep = "\n")
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = digits)
}

make_formula <- function(outcome, rhs_terms) {
  as.formula(paste(outcome, "~", paste(rhs_terms, collapse = " + ")))
}

# Column definitions (bandwidths: 100/75/50 km, with/without controls)
col_defs <- tibble::tribble(
  ~col_id,         ~bw,  ~controls,
  "(1) < 100 km",  100,  FALSE,
  "(2) < 75 km",    75,  FALSE,
  "(3) < 50 km",    50,  FALSE,
  "(4) < 100 km",  100,  TRUE,
  "(5) < 75 km",    75,  TRUE,
  "(6) < 50 km",    50,  TRUE
)
cols <- col_defs$col_id
empty_cols <- setNames(rep("", length(cols)), cols)

# Table 3 specifications
A_linear_rhs <- c("gustav", "gagliarducci_longitude", "gagliarducci_latitude")
A_quad_rhs   <- c(
  "gustav",
  "gagliarducci_longitude", "gagliarducci_latitude",
  "I(gagliarducci_longitude^2)", "I(gagliarducci_latitude^2)",
  "I(gagliarducci_longitude*gagliarducci_latitude)"
)

B_linear_rhs <- c("gustav", "signed_dist_km")
B_quad_rhs   <- c("gustav", "signed_dist_km", "I(signed_dist_km^2)")

OLS_rhs <- c("gustav")

# Estimation for one column
estimate_for_col <- function(rhs_base, bw, controls_on) {
  d <- df %>%
    filter(between(distance_gustav_km, -bw, bw)) %>%
    filter(distance_gustav_km != 0)
  
  rhs <- rhs_base
  geo_flag   <- "No"
  elect_flag <- "No"
  
  if (controls_on) {
    rhs <- unique(c(rhs, all_controls))
    geo_flag <- "Yes"
    elect_flag <- "Yes"
  }
  
  fml <- make_formula(outcome_var, rhs)
  est <- fit_extract(d, fml, coef_name = "gustav")
  
  list(
    cell  = fmt_cell(est$beta, est$se_rob, est$se_con, est$stars),
    adjr2 = fmt_num(est$adjr2),
    n     = as.character(est$n),
    geo   = geo_flag,
    elect = elect_flag
  )
}

get_block <- function(rhs_base) {
  out <- list()
  for (i in seq_len(nrow(col_defs))) {
    cd <- col_defs[i, ]
    out[[cd$col_id]] <- estimate_for_col(rhs_base, cd$bw, cd$controls)
  }
  out
}

extract_field <- function(block, field) {
  vals <- setNames(vector("character", length(cols)), cols)
  for (k in cols) vals[[k]] <- block[[k]][[field]]
  vals
}

# Force 6 real columns
make_row <- function(row_label, values_named) {
  stopifnot(all(names(values_named) %in% cols))
  out <- data.frame(row = row_label, stringsAsFactors = FALSE)
  for (k in cols) out[[k]] <- values_named[[k]]
  tibble::as_tibble(out)
}

# Compute blocks
A_lin  <- get_block(A_linear_rhs)
A_quad <- get_block(A_quad_rhs)

B_lin  <- get_block(B_linear_rhs)
B_quad <- get_block(B_quad_rhs)

OLS_b  <- get_block(OLS_rhs)

geo_flags   <- extract_field(OLS_b, "geo")
elect_flags <- extract_field(OLS_b, "elect")
obs_vals    <- extract_field(OLS_b, "n")

# Assemble final wide table
table_out <- dplyr::bind_rows(
  make_row("Dependent variable: Republic vote share in 1946", empty_cols),
  make_row("", empty_cols),
  
  make_row("PANEL A. Polynomial in Longitude and Latitude", empty_cols),
  
  make_row("Linear — Gustav Line", extract_field(A_lin,  "cell")),
  make_row("Adj. R2",             extract_field(A_lin,  "adjr2")),
  
  make_row("Quadratic — Gustav Line", extract_field(A_quad, "cell")),
  make_row("Adj. R2",                extract_field(A_quad, "adjr2")),
  
  make_row("", empty_cols),
  
  make_row("PANEL B. Polynomial in Euclidean Distance to Gustav Line", empty_cols),
  
  make_row("Linear — Gustav Line", extract_field(B_lin,  "cell")),
  make_row("Adj. R2",             extract_field(B_lin,  "adjr2")),
  
  make_row("Quadratic — Gustav Line", extract_field(B_quad, "cell")),
  make_row("Adj. R2",                extract_field(B_quad, "adjr2")),
  
  make_row("", empty_cols),
  
  make_row("Ordinary Least Squares", empty_cols),
  make_row("Gustav Line",            extract_field(OLS_b, "cell")),
  make_row("Adj. R2",                extract_field(OLS_b, "adjr2")),
  
  make_row("", empty_cols),
  
  make_row("Geo-demographic Controls", geo_flags),
  make_row("1919–24 Elections Controls", elect_flags),
  make_row("Observations", obs_vals)
)

# Export (wide CSV)
readr::write_csv(table_out, out_csv, na = "", eol = "\n")
message("Saved table to: ", out_csv)

if (!have_fixest) {
  message("Note: fixest not installed -> Conley SEs are blank. Install 'fixest' to compute Conley SEs.")
}