# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 13_figure_rd_plot.R
# Purpose:
#   Create the main RD figure (binned scatter + fitted curves with CI) for
#   Republic vote share in 1946 against signed distance to the Gustav Line.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/figures/figure2_rd_plot_main.png
# Notes:
#   - Excludes municipalities exactly on the line (dist_gustav_km == 0).
#   - Uses signed distance: North positive, South negative (km).
#   - Outcome is auto-detected (you can set it manually if needed).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_file <- "data/processed/merge/gustav_line_dataset.rds"

results_dir <- "results"
figures_dir <- file.path(results_dir, "figures")
out_png     <- file.path(figures_dir, "figure2_rd_plot_main.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load data + required vars
# ------------------------------------------------------------------------------
df <- readRDS(in_file)

req_vars <- c("dist_gustav_km", "gagliarducci_gustav", "gagliarducci_longitude", "gagliarducci_latitude")
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
  stop(
    "Could not auto-detect the 1946 Republic vote share variable.\n",
    "Please set outcome_var manually in Script 12."
  )
}
outcome_var <- cands[1]
message("Using outcome variable: ", outcome_var)

# ------------------------------------------------------------------------------
# Prepare RD running variable (signed distance)
# ------------------------------------------------------------------------------
df <- df %>%
  filter(!is.na(dist_gustav_km)) %>%
  filter(dist_gustav_km > 0) %>%  # exclude dist==0
  mutate(
    signed_dist_km = ifelse(gagliarducci_gustav == 1, dist_gustav_km, -dist_gustav_km)
  )

# Scale outcome to percentage points if stored as share
if (is.numeric(df[[outcome_var]])) {
  mx <- suppressWarnings(max(df[[outcome_var]], na.rm = TRUE))
  if (is.finite(mx) && mx <= 1.5) {
    df[[outcome_var]] <- df[[outcome_var]] * 100
    message("Scaled outcome to percentage points (x100).")
  }
}

# ------------------------------------------------------------------------------
# Figure settings (match the slide style)
# ------------------------------------------------------------------------------
x_lim <- 75          # show [-75, 75]
bin_w <- 3           # bin width in km (adjust if you want smoother/more granular)
poly_degree <- 2     # fitted curve degree (2 makes it look like the slide)

plot_df <- df %>%
  filter(!is.na(.data[[outcome_var]])) %>%
  filter(signed_dist_km >= -x_lim, signed_dist_km <= x_lim)

# ------------------------------------------------------------------------------
# Binned scatter: mean outcome within distance bins
# ------------------------------------------------------------------------------
binned <- plot_df %>%
  mutate(
    bin = floor(signed_dist_km / bin_w) * bin_w,
    bin_center = bin + bin_w / 2
  ) %>%
  group_by(bin, bin_center) %>%
  summarise(
    y_mean = mean(.data[[outcome_var]], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5)  # drop very sparse bins (stability)

# ------------------------------------------------------------------------------
# Fitted curves + CI (separate polynomial each side)
# ------------------------------------------------------------------------------
fit_side <- function(d, side_label) {
  # side_label: "left" (negative) or "right" (positive)
  if (side_label == "left") {
    dd <- d %>% filter(signed_dist_km < 0)
    x_grid <- seq(-x_lim, -1e-6, by = 0.5)
  } else {
    dd <- d %>% filter(signed_dist_km > 0)
    x_grid <- seq(1e-6, x_lim, by = 0.5)
  }
  
  # If too few observations, return empty
  if (nrow(dd) < 30) return(tibble())
  
  # Polynomial in signed distance (like the slide shape)
  fml <- as.formula(paste0(outcome_var, " ~ poly(signed_dist_km, ", poly_degree, ", raw = TRUE)"))
  m <- lm(fml, data = dd)
  
  pred <- predict(m, newdata = data.frame(signed_dist_km = x_grid), se.fit = TRUE)
  tibble(
    signed_dist_km = x_grid,
    fit = as.numeric(pred$fit),
    lo  = as.numeric(pred$fit - 1.96 * pred$se.fit),
    hi  = as.numeric(pred$fit + 1.96 * pred$se.fit),
    side = side_label
  )
}

curve_left  <- fit_side(plot_df, "left")
curve_right <- fit_side(plot_df, "right")
curve_df <- bind_rows(curve_left, curve_right)

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
p <- ggplot() +
  # CI band
  geom_ribbon(
    data = curve_df,
    aes(x = signed_dist_km, ymin = lo, ymax = hi),
    alpha = 0.25
  ) +
  # Fitted curve
  geom_line(
    data = curve_df,
    aes(x = signed_dist_km, y = fit),
    linewidth = 1
  ) +
  # Binned means
  geom_point(
    data = binned,
    aes(x = bin_center, y = y_mean),
    size = 2
  ) +
  # Cutoff
  geom_vline(xintercept = 0, linewidth = 1) +
  coord_cartesian(xlim = c(-x_lim, x_lim)) +
  labs(
    x = "Distance from Gustav Line (km)",
    y = "Demand for Institutional Change (%)"
  ) +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
ggsave(
  filename = out_png,
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

message("Saved figure to: ", out_png)