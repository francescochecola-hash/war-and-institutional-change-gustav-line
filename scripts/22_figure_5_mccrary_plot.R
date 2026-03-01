# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 22_figure_5_mccrary_plot.R
# Purpose: Plot McCrary density test at cutoff using rddensity.
#
# Inputs:  data/processed/merge/gustav_line_dataset.rds
# Output:  results/figures/figure5_mccrary_plot.png
# Notes:
#   - Running variable: distance_gustav_km (already signed), cutoff = 0
#   - Excludes municipalities exactly on the line (distance_gustav_km == 0)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(rddensity)
  library(here)
  library(ggplot2)
})

# Paths
in_file <- here("data", "processed", "merge", "gustav_line_dataset.rds")

results_dir <- here("results")
figures_dir <- here("results", "figures")
out_png     <- here("results", "figures", "figure5_mccrary_plot.png")

if (!dir.exists(results_dir)) dir.create(results_dir)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# Load data
df <- readRDS(in_file)

req_vars <- c("distance_gustav_km", "gustav")
missing_req <- setdiff(req_vars, names(df))
if (length(missing_req) > 0) {
  stop("Missing required variables:\n- ", paste(missing_req, collapse = "\n- "))
}

# Build running variable + sample restrictions
df <- df %>%
  mutate(gustav = suppressWarnings(as.integer(as.character(gustav)))) %>%
  filter(!is.na(distance_gustav_km), distance_gustav_km != 0) %>%  # exclude exact cutoff
  filter(!is.na(gustav)) %>%
  mutate(x = distance_gustav_km) %>%
  filter(!is.na(x)) %>%
  filter(abs(x) <= 200)  # symmetric window within +/- 200 km

if (sum(df$x < 0) == 0 || sum(df$x > 0) == 0) {
  stop("No support on one side of the cutoff for x within +/-200 km.")
}

# ---- McCrary-style density test ----
den <- rddensity(X = df$x, c = 0)

# Calculate robust p-value
get_robust_pval <- function(den_obj) {
  ss <- tryCatch({
    capture.output(tmp <- summary(den_obj))  # swallow printing
    tmp
  }, error = function(e) NULL)
  
  if (is.null(ss)) return(NA_real_)
  
  # Usually ss$test exists with row "Robust" and col "P > |T|" (or similar)
  if (!is.null(ss$test) && (is.matrix(ss$test) || is.data.frame(ss$test))) {
    tt <- ss$test
    rn <- rownames(tt)
    cn <- colnames(tt)
    
    # p-value column guess (handles "P > |T|" etc.)
    cn_low <- tolower(cn)
    pcol <- which(cn_low %in% c("p>|t|", "p > |t|", "p.value", "pval", "p-value", "pvalue"))
    if (length(pcol) == 0) pcol <- which(grepl("^p", cn_low) | grepl("p\\s*>", cn_low) | grepl("p\\|", cn_low))
    if (length(pcol) == 0) pcol <- which(grepl("p", cn_low))
    if (length(pcol) == 0) return(NA_real_)
    
    ridx <- which(grepl("robust", tolower(rn)))
    if (length(ridx) > 0) return(as.numeric(tt[ridx[1], pcol[1]]))
    
    return(as.numeric(tt[1, pcol[1]]))
  }
  
  NA_real_
}

pval <- get_robust_pval(den)
caption_txt <- if (!is.na(pval)) {
  sprintf("McCrary density test (rddensity): robust p-value = %.3f", pval)
} else {
  "McCrary density test (rddensity)"
}

# Build rdplotdensity
xlab_txt <- "Distance to Gustav Line (km)"
ylab_txt <- "Estimated density"

rp <- tryCatch(
  rddensity::rdplotdensity(
    den,
    X = df$x,
    plotRange = c(-200, 200),
    xlabel = xlab_txt,
    ylabel = ylab_txt
  ),
  error = function(e) {
    rddensity::rdplotdensity(den, X = df$x)
  }
)

# Extract ggplot from rdplotdensity output
extract_ggplot <- function(obj) {
  if (inherits(obj, "ggplot")) return(obj)
  if (is.list(obj)) {
    if (!is.null(obj$ggplot) && inherits(obj$ggplot, "ggplot")) return(obj$ggplot)
    if (!is.null(obj$plot)   && inherits(obj$plot,   "ggplot")) return(obj$plot)
    # last resort: first ggplot found in list
    for (nm in names(obj)) {
      if (inherits(obj[[nm]], "ggplot")) return(obj[[nm]])
    }
  }
  stop("Could not extract a ggplot object from rdplotdensity() output. Check rddensity version.")
}

p <- extract_ggplot(rp)

# Remove histogram layers
is_hist_layer <- function(layer) {
  tryCatch({
    g <- layer$geom
    if (is.null(g)) return(FALSE)
    class(g)[1] %in% c("GeomBar", "GeomCol")
  }, error = function(e) FALSE)
}

keep <- !vapply(p$layers, is_hist_layer, logical(1))
p$layers <- p$layers[keep]

col_left  <- "grey20"
col_right <- "navy"

is_geom <- function(layer, classes) {
  tryCatch({
    g <- layer$geom
    if (is.null(g)) return(FALSE)
    class(g)[1] %in% classes
  }, error = function(e) FALSE)
}

# Make CI ribbons subtle + thinner lines
for (i in seq_along(p$layers)) {
  if (is_geom(p$layers[[i]], c("GeomRibbon", "GeomPolygon"))) {
    p$layers[[i]]$aes_params$alpha <- 0.18
    if (is.null(p$layers[[i]]$aes_params$fill))   p$layers[[i]]$aes_params$fill <- "grey60"
    if (is.null(p$layers[[i]]$aes_params$colour)) p$layers[[i]]$aes_params$colour <- NA
  }
  
  if (is_geom(p$layers[[i]], c("GeomLine", "GeomPath"))) {
    if (!is.null(p$layers[[i]]$aes_params$linewidth)) {
      p$layers[[i]]$aes_params$linewidth <- 0.9
    } else {
      p$layers[[i]]$aes_params$size <- 0.9
    }
  }
}

# Recolor first two line layers (left/right fits)
is_line <- vapply(p$layers, function(l) is_geom(l, c("GeomLine", "GeomPath")), logical(1))
line_idx <- which(is_line)
if (length(line_idx) >= 2) {
  p$layers[[line_idx[1]]]$aes_params$colour <- col_left
  p$layers[[line_idx[2]]]$aes_params$colour <- col_right
}

# Add cutoff line
p <- p +
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "solid", colour = "grey30") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",              
    plot.title = element_text(
      hjust = 0,                           
      size = 16,
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, size = 11),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Save plot
png(filename = out_png, width = 1200, height = 800, res = 150)
print(p)
dev.off()

message("Saved: ", out_png)