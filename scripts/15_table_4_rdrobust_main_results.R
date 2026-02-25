# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 15_table_4_rdrobust_main_results.R
# Purpose:
#   Estimate RD treatment effects using the Calonico-Cattaneo–Titiunik framework (rdrobust):
#   local polynomial RD with robust SEs and 95% confidence intervals.
#   Produce results both without covariates and with the controls used in Table 3.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table3_rdrobust_results.csv
# Notes:
#   - Running variable: signed distance to the Gustav Line (km), cutoff c = 0.
#     North of the line: positive; South: negative;
#   - Excludes municipalities exactly on the line (dist_gustav_km == 0);
#   - By default uses manual bandwidths (100/75/50 km) to mirror Table 3;
#   - rdrobust provides RD-robust inference (heteroskedasticity-robust).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)     # data manipulation
  library(readr)     # csv export
  library(rdrobust)  # RD estimation using the Calonico-Cattaneo-Titiunik framework 
                     # (local polynomial RD with robust bias-corrected inference)
  library(here)      # robust paths for replication
})

# Paths
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")
out_csv     <- here("results", "tables", "table4_rdrobust_main_results.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Load data + variables used
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav")
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

# Construct running variable (km) and outcome (percentage points)
# IMPORTANT: coerce "gustav" to numeric 0/1 safely.
df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav))))

if (!all(df$gustav %in% c(0L, 1L) | is.na(df$gustav))) {
  stop("gustav is not coded as 0/1 after coercion. Check its values.")
}

df <- df %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%  # exclude dist==0
  filter(!is.na(gustav)) %>%
  mutate(
    x = ifelse(gustav == 1L, abs(distance_gustav_km), -abs(distance_gustav_km)),
    y = .data[[outcome_var]]
  ) %>%
  filter(!is.na(x), !is.na(y))

# Scale y to percentage points if it looks like share
mx <- suppressWarnings(max(df$y, na.rm = TRUE))
if (is.finite(mx) && mx <= 1.5) {
  df <- df %>% mutate(y = y * 100)
  message("Scaled outcome to percentage points (x100).")
}

# Quick support check
message("Observations left of cutoff:  ", sum(df$x < 0, na.rm = TRUE))
message("Observations right of cutoff: ", sum(df$x > 0, na.rm = TRUE))
if (sum(df$x < 0, na.rm = TRUE) == 0 || sum(df$x > 0, na.rm = TRUE) == 0) {
  stop("No support on one side of the cutoff after constructing x. Check gustav coding.")
}

# Controls: exactly as in your Table 3 controls set
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

# Convert share-like controls to percentage points if they are likely 0-1
share_like <- unique(c("gagliarducci_female_share_1951", "gagliarducci_analfshare_1951_tot", prewar_controls))
share_like <- intersect(share_like, names(df))
for (v in share_like) {
  if (is.numeric(df[[v]])) {
    mxv <- suppressWarnings(max(df[[v]], na.rm = TRUE))
    if (is.finite(mxv) && mxv <= 1.5) df[[v]] <- df[[v]] * 100
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

make_cov_matrix <- function(d, cov_names) {
  if (length(cov_names) == 0) return(NULL)
  as.matrix(d[, cov_names, drop = FALSE])
}

# Bandwidth choice
use_manual_bw <- TRUE
manual_bw <- c(100, 75, 50)

# Skip bandwidths where one side is too small (rdrobust becomes unstable)
min_side_n <- 30

# Extract rdrobust results into a tidy row (robust inference) using rd_obj directly
extract_rd <- function(rd_obj) {
  est <- rd_obj$Estimate
  se  <- rd_obj$se
  pv  <- rd_obj$pv
  ci  <- rd_obj$ci
  bws <- rd_obj$bws
  
  if (is.null(est) || is.null(se) || is.null(ci) || is.null(bws)) {
    stop("Unexpected rdrobust object structure: missing Estimate/se/ci/bws.")
  }
  
  pick <- if (!is.null(rownames(est)) && any(grepl("Robust", rownames(est), ignore.case = TRUE))) {
    which(grepl("Robust", rownames(est), ignore.case = TRUE))[1]
  } else {
    nrow(est)
  }
  
  tibble::tibble(
    tau    = as.numeric(est[pick, 1]),
    se     = as.numeric(se[pick, 1]),
    p      = as.numeric(pv[pick, 1]),
    ci95_l = as.numeric(ci[pick, 1]),
    ci95_u = as.numeric(ci[pick, 2]),
    h_l    = as.numeric(bws[1, 1]),
    h_r    = as.numeric(bws[1, 2])
  )
}

# Runner: one specification (with/without covariates)
run_rd <- function(d, with_covariates = FALSE, h = NULL) {
  if (with_covariates) {
    cov_mat <- make_cov_matrix(d, all_controls)
    keep <- complete.cases(cov_mat)
    dd <- d[keep, , drop = FALSE]
    cov_mat <- cov_mat[keep, , drop = FALSE]
    if (nrow(dd) < 50) stop("Too few observations after covariate complete-case filtering.")
    
    rd <- if (is.null(h)) {
      rdrobust(y = dd$y, x = dd$x, c = 0, covs = cov_mat)
    } else {
      rdrobust(y = dd$y, x = dd$x, c = 0, covs = cov_mat, h = h)
    }
  } else {
    dd <- d
    rd <- if (is.null(h)) {
      rdrobust(y = dd$y, x = dd$x, c = 0)
    } else {
      rdrobust(y = dd$y, x = dd$x, c = 0, h = h)
    }
  }
  
  tmp <- extract_rd(rd)
  if (nrow(tmp) == 0) stop("extract_rd() returned 0 rows unexpectedly.")
  
  tmp %>%
    mutate(
      with_covariates = with_covariates,
      N_used = nrow(dd)
    )
}

# Build results table
results <- list()

if (use_manual_bw) {
  for (h in manual_bw) {
    dsub <- df %>% filter(abs(x) <= h)
    
    nL <- sum(dsub$x < 0, na.rm = TRUE)
    nR <- sum(dsub$x > 0, na.rm = TRUE)
    
    if (nL < min_side_n || nR < min_side_n) {
      message("Skipping h=", h, " (too few obs on one side: L=", nL, ", R=", nR, ").")
      next
    }
    
    results[[length(results) + 1]] <- run_rd(dsub, with_covariates = FALSE, h = h) %>%
      mutate(bandwidth_km = h, bandwidth_type = "Manual (fixed)")
    
    results[[length(results) + 1]] <- run_rd(dsub, with_covariates = TRUE, h = h) %>%
      mutate(bandwidth_km = h, bandwidth_type = "Manual (fixed)")
  }
} else {
  results[[length(results) + 1]] <- run_rd(df, with_covariates = FALSE, h = NULL) %>%
    mutate(bandwidth_km = NA_real_, bandwidth_type = "Data-driven (rdrobust default)")
  
  results[[length(results) + 1]] <- run_rd(df, with_covariates = TRUE, h = NULL) %>%
    mutate(bandwidth_km = NA_real_, bandwidth_type = "Data-driven (rdrobust default)")
}

out <- bind_rows(results) %>%
  mutate(
    spec = ifelse(with_covariates, "With controls", "No controls"),
    tau = round(tau, 3),
    se = round(se, 3),
    ci95_l = round(ci95_l, 3),
    ci95_u = round(ci95_u, 3),
    h_l = round(h_l, 3),
    h_r = round(h_r, 3),
    p = round(p, 3)
  ) %>%
  select(
    spec, bandwidth_type, bandwidth_km, N_used,
    tau, se, p, ci95_l, ci95_u,
    h_l, h_r
  ) %>%
  arrange(bandwidth_type, bandwidth_km, spec)

# Export CSV
readr::write_csv(out, out_csv, na = "", eol = "\n")
message("Saved rdrobust results to: ", out_csv)