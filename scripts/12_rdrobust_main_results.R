# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 14_rdrobust_main_results.R
# Purpose:
#   Estimate RD treatment effects using the Cattaneo–Titiunik framework (rdrobust):
#   local polynomial RD with robust SEs and 95% confidence intervals.
#   Produce results both without covariates and with the controls used in Table 3,
#   and export a replication-ready CSV.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table3_rdrobust_results.csv
# Notes:
#   - Running variable: signed distance to the Gustav Line (km), cutoff c = 0.
#     North of the line: positive; South: negative.
#   - Excludes municipalities exactly on the line (dist_gustav_km == 0).
#   - Uses rdrobust default bandwidth selection unless you set manual bandwidths below.
#   - rdrobust provides RD-robust inference (heteroskedasticity-robust), not Conley SEs.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rdrobust)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_file <- "data/processed/merge/gustav_line_dataset.rds"

results_dir <- "results"
tables_dir  <- file.path(results_dir, "tables")
out_csv     <- file.path(tables_dir, "table3_rdrobust_results.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load data + required vars
# ------------------------------------------------------------------------------
df <- readRDS(in_file)

req_vars <- c("dist_gustav_km", "gagliarducci_gustav")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# ------------------------------------------------------------------------------
# Outcome: Republic vote share in 1946 (AUTO-detect; set manually if needed)
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Construct signed running variable (km) and outcome (percentage points)
# ------------------------------------------------------------------------------
df <- df %>%
  filter(!is.na(dist_gustav_km)) %>%
  filter(dist_gustav_km > 0) %>%  # exclude dist==0
  mutate(
    x = ifelse(gagliarducci_gustav == 1, dist_gustav_km, -dist_gustav_km),
    y = .data[[outcome_var]]
  ) %>%
  filter(!is.na(x), !is.na(y))

# scale y to percentage points if it looks like share
mx <- suppressWarnings(max(df$y, na.rm = TRUE))
if (is.finite(mx) && mx <= 1.5) {
  df <- df %>% mutate(y = y * 100)
  message("Scaled outcome to percentage points (x100).")
}

# ------------------------------------------------------------------------------
# Controls: exactly as in your Table 3 controls set
# (Elevation City Hall + log(pop1951) + %female1951 + %illiterates1951 + all prewar outcomes)
# ------------------------------------------------------------------------------
# Base controls
base_controls <- c(
  "gagliarducci_mun_elev",
  "gagliarducci_popres_1951_tot",
  "gagliarducci_female_share_1951",
  "gagliarducci_analfshare_1951_tot"
)

# Prewar electoral outcomes
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

# Build covariate matrix (complete cases will be handled below)
make_cov_matrix <- function(d, cov_names) {
  if (length(cov_names) == 0) return(NULL)
  as.matrix(d[, cov_names, drop = FALSE])
}

# ------------------------------------------------------------------------------
# Optional: manual bandwidths (to mirror Table 3)
# If you want to force h = 100/75/50, set use_manual_bw <- TRUE.
# Note: rdrobust expects bandwidths in the same units as x (km).
# ------------------------------------------------------------------------------
use_manual_bw <- TRUE
manual_bw <- c(100, 75, 50)

# ------------------------------------------------------------------------------
# Extract rdrobust results into a tidy row
# ------------------------------------------------------------------------------
extract_rd <- function(rd_obj) {
  # rdrobust output structure is stable, but column names may vary slightly by version.
  # We use numeric positions robustly:
  #
  # Estimates table usually has rows: Conventional, Bias-corrected, Robust
  # and columns including tau, se, z, pv, ci_l, ci_r etc.
  #
  # We'll prefer the "Robust" row if available; fallback to last row.
  summ <- summary(rd_obj)
  
  # Convert to data.frame for safe indexing
  est <- as.data.frame(summ$Estimate)
  
  # pick "Robust" if rownames exist
  rnames <- rownames(est)
  pick <- if (!is.null(rnames) && any(grepl("Robust", rnames, ignore.case = TRUE))) {
    which(grepl("Robust", rnames, ignore.case = TRUE))[1]
  } else {
    nrow(est)
  }
  
  # Try to locate columns by name (with fallbacks)
  col_tau <- if ("tau" %in% names(est)) "tau" else names(est)[1]
  col_se  <- if ("se"  %in% names(est)) "se"  else names(est)[2]
  col_pv  <- if ("pv"  %in% names(est)) "pv"  else {
    # sometimes "pval"
    if ("pval" %in% names(est)) "pval" else NA_character_
  }
  
  # CI matrix
  ci <- as.data.frame(summ$ci)
  ci_pick <- if (!is.null(rownames(ci)) && any(grepl("Robust", rownames(ci), ignore.case = TRUE))) {
    which(grepl("Robust", rownames(ci), ignore.case = TRUE))[1]
  } else {
    nrow(ci)
  }
  
  # bandwidths
  bws <- as.data.frame(summ$bws)
  
  tibble(
    tau = as.numeric(est[pick, col_tau]),
    se  = as.numeric(est[pick, col_se]),
    p   = if (!is.na(col_pv)) as.numeric(est[pick, col_pv]) else NA_real_,
    ci95_l = as.numeric(ci[ci_pick, 1]),
    ci95_u = as.numeric(ci[ci_pick, 2]),
    h_l = if (ncol(bws) >= 1) as.numeric(bws[1, 1]) else NA_real_,
    h_r = if (ncol(bws) >= 2) as.numeric(bws[1, 2]) else NA_real_
  )
}

# ------------------------------------------------------------------------------
# Runner: one spec (with/without covariates), with either auto or manual bandwidth
# ------------------------------------------------------------------------------
run_rd <- function(d, with_covariates = FALSE, h = NULL) {
  if (with_covariates) {
    cov_mat <- make_cov_matrix(d, all_controls)
    # Keep complete cases for covariates to avoid rdrobust dropping inconsistently
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
  
  extract_rd(rd) %>%
    mutate(
      with_covariates = with_covariates,
      N_used = nrow(dd)
    )
}

# ------------------------------------------------------------------------------
# Build results table
# ------------------------------------------------------------------------------
results <- list()

if (use_manual_bw) {
  for (h in manual_bw) {
    # Restrict sample to |x| <= h to mirror your Table 3 bandwidth logic
    dsub <- df %>% filter(abs(x) <= h)
    
    results[[length(results) + 1]] <- run_rd(dsub, with_covariates = FALSE, h = h) %>%
      mutate(bandwidth_km = h, bandwidth_type = "Manual (fixed)")
    
    results[[length(results) + 1]] <- run_rd(dsub, with_covariates = TRUE, h = h) %>%
      mutate(bandwidth_km = h, bandwidth_type = "Manual (fixed)")
  }
} else {
  # Default: data-driven bandwidth
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
    h_r = round(h_r, 3)
  ) %>%
  select(
    spec, bandwidth_type, bandwidth_km, N_used,
    tau, se, p, ci95_l, ci95_u,
    h_l, h_r
  ) %>%
  arrange(bandwidth_type, bandwidth_km, spec)

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------
readr::write_csv(out, out_csv, na = "", eol = "\n")
message("Saved rdrobust results to: ", out_csv)

# Optional: print to console
print(out)