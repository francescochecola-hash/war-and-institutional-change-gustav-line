# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 11_main_results.R
# Purpose:
#   Replicate Table 3 (Main Results) in the same layout as in the slide:
#   six columns (<100/<75/<50 km) without controls (1)-(3) and with controls (4)-(6),
#   for Panel A (poly in longitude/latitude), Panel B (poly in signed distance),
#   and an OLS block. Export as a wide CSV table.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table3_main_results.csv
# Notes:
#   - Treatment: gagliarducci_gustav (1 = North of Gustav Line)
#   - Coordinates: gagliarducci_longitude, gagliarducci_latitude
#   - Distance: dist_gustav_km (unsigned). Signed distance is constructed using gagliarducci_gustav.
#   - SEs in table: Robust (HC1), as in the screenshot.
#   - Controls INCLUDED when controls_on==TRUE:
#       (i) Elevation of the City Hall
#       (ii) Log Population (1951)
#       (iii) % Female Population (1951)
#       (iv) % Illiterates (1951)
#       (v) ALL Prewar Electoral Outcomes variables (1919/1921/1924)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(sandwich)
  library(lmtest)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_file <- "data/processed/merge/gustav_line_dataset.rds"

results_dir <- "results"
tables_dir  <- file.path(results_dir, "tables")
out_csv     <- file.path(tables_dir, "table3_main_results.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load data + required vars
# ------------------------------------------------------------------------------
df <- readRDS(in_file)

req_vars <- c(
  "dist_gustav_km",
  "gagliarducci_gustav",
  "gagliarducci_longitude",
  "gagliarducci_latitude"
)
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# ------------------------------------------------------------------------------
# Outcome: Republic vote share in 1946 (auto-detect; edit if you want)
# ------------------------------------------------------------------------------
nm <- names(df)
candidates <- nm[grepl("ref", nm, ignore.case = TRUE) & grepl("46|1946", nm, ignore.case = TRUE)]
candidates <- unique(c(
  candidates[grepl("repub", candidates, ignore.case = TRUE) & grepl("share|sh|perc|pct|p_", candidates, ignore.case = TRUE)],
  candidates[grepl("share|sh|perc|pct|p_", candidates, ignore.case = TRUE)],
  candidates
))

if (length(candidates) == 0) {
  stop(
    "Could not auto-detect the 1946 Republic vote share variable.\n",
    "Please set outcome_var manually in Script 11."
  )
}
outcome_var <- candidates[1]
message("Using outcome variable: ", outcome_var)

# ------------------------------------------------------------------------------
# Sample restrictions: exclude dist==0; keep dist>0
# ------------------------------------------------------------------------------
df <- df %>%
  filter(!is.na(dist_gustav_km)) %>%
  filter(dist_gustav_km > 0)

# Signed distance (north positive, south negative)
df <- df %>%
  mutate(signed_dist_km = ifelse(gagliarducci_gustav == 1, dist_gustav_km, -dist_gustav_km))

# ------------------------------------------------------------------------------
# Scale outcome to percentage points if it looks like a share in [0,1]
# ------------------------------------------------------------------------------
if (is.numeric(df[[outcome_var]])) {
  mx <- suppressWarnings(max(df[[outcome_var]], na.rm = TRUE))
  if (is.finite(mx) && mx <= 1.5) {
    df[[outcome_var]] <- df[[outcome_var]] * 100
    message("Scaled outcome to percentage points (x100).")
  }
}

# ------------------------------------------------------------------------------
# Controls (as requested)
# ------------------------------------------------------------------------------
# (i) Elevation City Hall
# (ii) Log Population (1951)
# (iii) % Female Population (1951)
# (iv) % Illiterates (1951)
# (v) ALL Prewar Electoral Outcomes

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

# Keep only controls that exist
base_controls   <- intersect(base_controls, names(df))
prewar_controls <- intersect(prewar_controls, names(df))

# Convert share-like controls to percentage points if they are in [0,1]
share_like <- unique(c(
  "gagliarducci_female_share_1951",
  "gagliarducci_analfshare_1951_tot",
  prewar_controls
))
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

# ------------------------------------------------------------------------------
# Helpers: robust regression + stars + formatting "coef (se)***"
# ------------------------------------------------------------------------------
star_from_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

fit_extract <- function(data, fml, coef_name = "gagliarducci_gustav") {
  m <- lm(fml, data = data)
  vc <- sandwich::vcovHC(m, type = "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc)
  
  beta <- as.numeric(ct[coef_name, "Estimate"])
  se   <- as.numeric(ct[coef_name, "Std. Error"])
  pval <- as.numeric(ct[coef_name, "Pr(>|t|)"])
  
  list(
    n = nobs(m),
    beta = beta,
    se = se,
    p = pval,
    adjr2 = summary(m)$adj.r.squared
  )
}

fmt_cell <- function(beta, se, p, digits = 3) {
  if (is.na(beta) || is.na(se)) return("")
  st <- star_from_p(p)
  paste0(formatC(beta, format = "f", digits = digits), st, "\n(",
         formatC(se, format = "f", digits = digits), ")")
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = digits)
}

make_formula <- function(rhs_terms) {
  as.formula(paste(outcome_var, "~", paste(rhs_terms, collapse = " + ")))
}

# ------------------------------------------------------------------------------
# Column definitions (as in the slide)
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Specs (exactly like the slide)
# ------------------------------------------------------------------------------
# Panel A: polynomial in longitude and latitude
A_linear_rhs <- c("gagliarducci_gustav", "gagliarducci_longitude", "gagliarducci_latitude")
A_quad_rhs   <- c(
  "gagliarducci_gustav",
  "gagliarducci_longitude", "gagliarducci_latitude",
  "I(gagliarducci_longitude^2)", "I(gagliarducci_latitude^2)",
  "I(gagliarducci_longitude*gagliarducci_latitude)"
)

# Panel B: polynomial in (signed) distance
B_linear_rhs <- c("gagliarducci_gustav", "signed_dist_km")
B_quad_rhs   <- c("gagliarducci_gustav", "signed_dist_km", "I(signed_dist_km^2)")

# OLS
OLS_rhs <- c("gagliarducci_gustav")

# ------------------------------------------------------------------------------
# One cell estimator for a given rhs_base and table column
# ------------------------------------------------------------------------------
estimate_for_col <- function(rhs_base, bw, controls_on) {
  d <- df %>% filter(dist_gustav_km <= bw)
  
  rhs <- rhs_base
  geo_flag   <- "No"
  elect_flag <- "No"
  
  if (controls_on) {
    rhs <- unique(c(rhs, all_controls))
    geo_flag <- "Yes"
    elect_flag <- "Yes"
  }
  
  fml <- make_formula(rhs)
  est <- fit_extract(d, fml)
  
  list(
    cell  = fmt_cell(est$beta, est$se, est$p),
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

make_row <- function(row_label, values_named) {
  tibble(row = row_label) %>% bind_cols(as_tibble(values_named))
}

# ------------------------------------------------------------------------------
# Compute all blocks
# ------------------------------------------------------------------------------
A_lin  <- get_block(A_linear_rhs)
A_quad <- get_block(A_quad_rhs)

B_lin  <- get_block(B_linear_rhs)
B_quad <- get_block(B_quad_rhs)

OLS_b  <- get_block(OLS_rhs)

# Bottom flags/obs from OLS block (same per column by construction)
geo_flags   <- extract_field(OLS_b, "geo")
elect_flags <- extract_field(OLS_b, "elect")
obs_vals    <- extract_field(OLS_b, "n")

# ------------------------------------------------------------------------------
# Assemble final wide table exactly in the slide layout
# ------------------------------------------------------------------------------
table_out <- bind_rows(
  make_row("TABLE 3. DEMAND FOR INSTITUTIONAL CHANGE", setNames(rep("", length(cols)), cols)),
  make_row("Dependent variable: Republic vote share in 1946", setNames(rep("", length(cols)), cols)),
  make_row("", setNames(rep("", length(cols)), cols)),
  
  make_row("PANEL A. Polynomial in Longitude and Latitude", setNames(rep("", length(cols)), cols)),
  
  make_row("Linear  — Gustav Line",  extract_field(A_lin,  "cell")),
  make_row("          Adj. R2",       extract_field(A_lin,  "adjr2")),
  
  make_row("Quadratic — Gustav Line", extract_field(A_quad, "cell")),
  make_row("            Adj. R2",     extract_field(A_quad, "adjr2")),
  
  make_row("", setNames(rep("", length(cols)), cols)),
  
  make_row("PANEL B. Polynomial in Euclidean Distance to Gustav Line", setNames(rep("", length(cols)), cols)),
  
  make_row("Linear  — Gustav Line",  extract_field(B_lin,  "cell")),
  make_row("          Adj. R2",       extract_field(B_lin,  "adjr2")),
  
  make_row("Quadratic — Gustav Line", extract_field(B_quad, "cell")),
  make_row("            Adj. R2",     extract_field(B_quad, "adjr2")),
  
  make_row("", setNames(rep("", length(cols)), cols)),
  
  make_row("Ordinary Least Squares", setNames(rep("", length(cols)), cols)),
  make_row("Gustav Line",            extract_field(OLS_b,  "cell")),
  make_row("Adj. R2",                extract_field(OLS_b,  "adjr2")),
  
  make_row("", setNames(rep("", length(cols)), cols)),
  
  make_row("Geo-demographic Controls",      geo_flags),
  make_row("1919–24 Elections Controls",    elect_flags),
  make_row("Observations",                 obs_vals)
)

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------
readr::write_csv(table_out, out_csv, na = "", eol = "\n")
message("Saved table to: ", out_csv)