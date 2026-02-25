# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 16_table_5_rdrobust_datadriven.R
# Purpose:
#   RD estimates using rdrobust with data-driven bandwidth selection (MSE or CER),
#   reporting robust inference (tau, SE, p-value, 95% CI) with/without covariates.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/tables/table5_rdrobust_datadriven.csv
# Notes:
#   - Running variable: signed distance to Gustav Line (km), cutoff c = 0.
#   - Excludes distance_gustav_km == 0.
#   - Inference: rdrobust robust (bias-corrected) 95% CI.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)     # 
  library(readr)     # export csv
  library(rdrobust)  # RD estimation (local polynomial, bias-corrected inference)
  library(here)      # robust project-root paths for replication
})

# Paths (project-root relative)
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
tables_dir  <- here("results", "tables")
out_csv     <- here("results", "tables", "table5_rdrobust_datadriven.csv")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Load data + required vars
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Outcome variable
nm <- names(df)
cands <- nm[grepl("ref", nm, ignore.case = TRUE) & grepl("46|1946", nm, ignore.case = TRUE)]
cands <- unique(c(
  cands[grepl("repub", cands, ignore.case = TRUE) & grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands[grepl("share|sh|perc|pct|p_", cands, ignore.case = TRUE)],
  cands
))
if (length(cands) == 0) stop("Could not auto-detect the 1946 Republic vote share variable.")
outcome_var <- cands[1]
message("Using outcome variable: ", outcome_var)

# Build signed running variable x and outcome y
df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav))))

if (!all(df$gustav %in% c(0L, 1L) | is.na(df$gustav))) {
  stop("gustav is not coded as 0/1 after coercion. Check its values.")
}

df <- df %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%
  filter(!is.na(gustav)) %>%
  mutate(
    x = distance_gustav_km,   # already signed
    y = .data[[outcome_var]]
  ) %>%
  filter(!is.na(x), !is.na(y))

# Scale y to percentage points if needed
mx <- suppressWarnings(max(df$y, na.rm = TRUE))
if (is.finite(mx) && mx <= 1.5) {
  df <- df %>% mutate(y = y * 100)
  message("Scaled outcome to percentage points (x100).")
}

if (sum(df$x < 0) == 0 || sum(df$x > 0) == 0) {
  stop("No support on one side of the cutoff.")
}

# Controls (same set used in Table 3)
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

share_like <- unique(c("gagliarducci_female_share_1951",
                       "gagliarducci_analfshare_1951_tot",
                       prewar_controls))
share_like <- intersect(share_like, names(df))

for (v in share_like) {
  mxv <- suppressWarnings(max(df[[v]], na.rm = TRUE))
  if (is.finite(mxv) && mxv <= 1.5) df[[v]] <- df[[v]] * 100
}

if ("gagliarducci_popres_1951_tot" %in% names(df)) {
  df <- df %>%
    mutate(log_pop_1951 = ifelse(!is.na(gagliarducci_popres_1951_tot) &
                                   gagliarducci_popres_1951_tot > 0,
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

# Extract results
extract_rd <- function(rd_obj) {
  est <- rd_obj$Estimate
  se  <- rd_obj$se
  pv  <- rd_obj$pv
  ci  <- rd_obj$ci
  bws <- rd_obj$bws
  
  pick <- if (!is.null(rownames(est)) &&
              any(grepl("Robust", rownames(est), ignore.case = TRUE))) {
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

run_rd <- function(d, with_covariates = FALSE, bwselect = c("mserd", "cerrd")) {
  bwselect <- match.arg(bwselect)
  
  if (with_covariates) {
    cov_mat <- make_cov_matrix(d, all_controls)
    keep <- complete.cases(cov_mat)
    dd <- d[keep, , drop = FALSE]
    cov_mat <- cov_mat[keep, , drop = FALSE]
    rd <- rdrobust(y = dd$y, x = dd$x, c = 0,
                   covs = cov_mat, bwselect = bwselect)
  } else {
    dd <- d
    rd <- rdrobust(y = dd$y, x = dd$x, c = 0,
                   bwselect = bwselect)
  }
  
  extract_rd(rd) %>%
    mutate(
      with_covariates = with_covariates,
      N_used = nrow(dd),
      bwselect = bwselect
    )
}

# Run: MSE-optimal and CER-optimal
res <- dplyr::bind_rows(
  run_rd(df, FALSE, "mserd"),
  run_rd(df, TRUE,  "mserd"),
  run_rd(df, FALSE, "cerrd"),
  run_rd(df, TRUE,  "cerrd")
) %>%
  mutate(
    spec = ifelse(with_covariates, "With controls", "No controls"),
    tau = round(tau, 3),
    se = round(se, 3),
    p = round(p, 3),
    ci95_l = round(ci95_l, 3),
    ci95_u = round(ci95_u, 3),
    h_l = round(h_l, 3),
    h_r = round(h_r, 3)
  ) %>%
  select(spec, bwselect, N_used, tau, se, p,
         ci95_l, ci95_u, h_l, h_r) %>%
  arrange(bwselect, spec)

# Export CSV
readr::write_csv(res, out_csv, na = "", eol = "\n")
message("Saved: ", out_csv)