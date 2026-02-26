# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 18_figure_map_italy_occupation.R
# Purpose:
#   Plot Italy municipality map colored by Occupation days and overlay Gustav Line.
#
# Inputs:
#   data/processed/merge/comuni_2001_boundaries_merged.rds
#   data/processed/import/gustav_line.rds
#
# Output:
#   results/figures/figure_map_italy_occupation.png
#
# Notes:
#   - occupation is stored as YEARS in many cases -> convert to DAYS using floor()
#     to avoid pushing borderline values (e.g., 447.x) into the next bin (448+).
#   - NA occupation -> "Excluded" (black)
#   - No municipal borders (no outline)
#   - Gustav line in black
#   - CRS: enforce EPSG:32632 for both layers
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(here)
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_comuni <- here("data", "processed", "merge", "comuni_2001_boundaries_merged.rds")
in_line   <- here("data", "processed", "import", "gustav_line.rds")

out_png <- here("results", "figures", "figure1_map_italy_occupation.png")
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
comuni <- readRDS(in_comuni)
gline  <- readRDS(in_line)

if (!inherits(comuni, "sf")) stop("comuni_2001_boundaries_merged.rds is not an sf object.")
if (!inherits(gline,  "sf")) stop("gustav_line.rds is not an sf object.")

# Geometry sanity checks
gtypes_comuni <- unique(as.character(st_geometry_type(comuni, by_geometry = TRUE)))
if (!all(gtypes_comuni %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("comuni geometry is not polygonal. Found: ", paste(gtypes_comuni, collapse = ", "))
}
gtypes_line <- unique(as.character(st_geometry_type(gline, by_geometry = TRUE)))
if (!all(gtypes_line %in% c("LINESTRING", "MULTILINESTRING"))) {
  stop("gustav_line geometry is not linear. Found: ", paste(gtypes_line, collapse = ", "))
}

# ------------------------------------------------------------------------------
# CRS: enforce EPSG:32632 for both layers
# ------------------------------------------------------------------------------
target_epsg <- 32632
if (is.na(st_crs(comuni)$epsg)) stop("comuni has missing CRS.")
if (is.na(st_crs(gline)$epsg))  stop("gustav_line has missing CRS.")

if (st_crs(comuni)$epsg != target_epsg) comuni <- st_transform(comuni, target_epsg)
if (st_crs(gline)$epsg  != target_epsg) gline  <- st_transform(gline,  target_epsg)

# ------------------------------------------------------------------------------
# Build occupation_days + bins (IMPORTANT FIX: floor(), not round())
# ------------------------------------------------------------------------------
if (!("occupation" %in% names(comuni))) stop("Missing variable 'occupation' in comuni dataset.")

comuni <- comuni %>%
  mutate(
    occupation_days = case_when(
      is.na(occupation) ~ NA_real_,
      occupation < 10   ~ floor(occupation * 365 + 1e-8),  # years -> days (floor avoids 447.x -> 448)
      TRUE              ~ as.numeric(occupation)           # if already days
    ),
    # cap (the paper map seems capped at 724)
    occupation_days = pmin(occupation_days, 724)
  ) %>%
  mutate(
    occ_cat = case_when(
      is.na(occupation_days) ~ "Excluded",
      between(occupation_days,  63, 100) ~ "63 - 100",
      between(occupation_days, 101, 179) ~ "101 - 179",
      between(occupation_days, 180, 447) ~ "180 - 447",
      between(occupation_days, 448, 501) ~ "448 - 501",
      between(occupation_days, 502, 579) ~ "502 - 579",
      between(occupation_days, 580, 724) ~ "580 - 724",
      TRUE ~ "Excluded"
    ),
    occ_cat = factor(
      occ_cat,
      levels = c("63 - 100", "101 - 179", "180 - 447", "448 - 501", "502 - 579", "580 - 724", "Excluded")
    )
  )

# ------------------------------------------------------------------------------
# Palette (manual colors close to the reference map)
# ------------------------------------------------------------------------------
occ_cols <- c(
  "63 - 100"  = "#FFF200",  # yellow
  "101 - 179" = "#E6C300",  # dark yellow
  "180 - 447" = "#FFA500",  # orange
  "448 - 501" = "#D97700",  # dark orange
  "502 - 579" = "#CC0000",  # red
  "580 - 724" = "#7A0000",  # dark red
  "Excluded"  = "#000000"   # black
)

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
p <- ggplot() +
  # Comuni (no borders)
  geom_sf(
    data = comuni,
    aes(fill = occ_cat),
    color = NA,
    linewidth = 0
  ) +
  # Gustav line (put in legend)
  geom_sf(
    data = gline,
    aes(color = "Gustav Line"),
    linewidth = 0.8
  ) +
  scale_fill_manual(
    values = occ_cols,
    drop = FALSE,
    name = "Occupation days"
  ) +
  scale_color_manual(
    values = c("Gustav Line" = "black"),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linetype = 1, linewidth = 1.1)
    ),
    fill = guide_legend(order = 2)
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

ggsave(out_png, plot = p, width = 8, height = 8, dpi = 300)
message("Saved: ", out_png)