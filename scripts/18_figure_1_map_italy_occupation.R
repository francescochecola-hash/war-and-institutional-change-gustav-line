# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 18_figure_1_map_italy_occupation.R
# Purpose:
#   Plot Italy municipality map (2001 boundaries) colored by occupation DAYS,
#   and overlay the Gustav Line (black).
#
# Inputs:
#   data/processed/merge/comuni_2001_boundaries_merged.rds
#   data/processed/import/gustav_line.rds
#
# Output:
#   results/figures/figure1_map_italy_occupation.png
#
# Notes:
#   - occupation is stored in YEARS -> converted to days (years * 365.25)
#   - No municipal boundary borders
#   - NA occupation -> black ("Excluded")
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(here)
})

# Paths
comuni_file <- here("data", "processed", "merge", "comuni_2001_boundaries_merged.rds")
gustav_file <- here("data", "processed", "import", "gustav_line.rds")

out_png <- here("results", "figures", "figure1_map_italy_occupation.png")
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

# Read data
comuni <- readRDS(comuni_file)
gustav <- readRDS(gustav_file)

# Checks: sf + geometry types
if (!inherits(comuni, "sf")) stop("comuni_2001_boundaries_merged.rds is not an sf object.")
if (!inherits(gustav, "sf")) stop("gustav_line.rds is not an sf object.")

g_comuni <- unique(as.character(st_geometry_type(comuni, by_geometry = TRUE)))
if (!all(g_comuni %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("comuni geometry is not polygonal. Found: ", paste(g_comuni, collapse = ", "))
}

g_gustav <- unique(as.character(st_geometry_type(gustav, by_geometry = TRUE)))
if (!all(g_gustav %in% c("LINESTRING", "MULTILINESTRING"))) {
  stop("gustav_line geometry is not line. Found: ", paste(g_gustav, collapse = ", "))
}

# CRS: align line to comuni CRS
if (is.na(st_crs(comuni))) stop("comuni has missing CRS.")
if (is.na(st_crs(gustav))) stop("gustav_line has missing CRS.")

if (st_crs(gustav) != st_crs(comuni)) {
  gustav <- st_transform(gustav, st_crs(comuni))
}

# Ensure occupation numeric (protect from factor/character)
if (!("occupation" %in% names(comuni))) stop("Variable 'occupation' not found in comuni dataset.")

comuni <- comuni %>%
  mutate(
    occupation_num = suppressWarnings(as.numeric(as.character(occupation)))
  )

# Robust conversion to DAYS:
# - if values look like "years" (max <= 10) -> days = years * 365
# - else -> already days
mx <- suppressWarnings(max(comuni$occupation_num, na.rm = TRUE))
use_years <- is.finite(mx) && mx <= 10

comuni <- comuni %>%
  mutate(
    # occupation è in ANNI (come hai mostrato: 0.1726 ecc.)
    occupation_days = ifelse(is.na(occupation), NA_real_, occupation * 365),
    
    # clamp: tutto sopra 724 va a 724, tutto sotto 63 va a 63
    occupation_days = pmax(pmin(occupation_days, 724), 63),
    
    occ_cat = case_when(
      is.na(occupation_days) ~ "Excluded",
      occupation_days <= 100 ~ "63 - 100",
      occupation_days <= 179 ~ "101 - 179",
      occupation_days <= 447 ~ "180 - 447",
      occupation_days <= 501 ~ "448 - 501",
      occupation_days <= 579 ~ "502 - 579",
      TRUE ~ "580 - 724"
    ),
    occ_cat = factor(
      occ_cat,
      levels = c("63 - 100","101 - 179","180 - 447",
                 "448 - 501","502 - 579","580 - 724","Excluded")
    )
  )

# Plot (no municipal borders; Gustav line black)
p <- ggplot() +
  geom_sf(data = comuni, aes(fill = occ_cat), color = NA) +
  geom_sf(data = gustav, color = "black", linewidth = 0.7) +
  scale_fill_manual(
    name = "Occupation days",
    values = c(
      "63 - 100"  = "yellow",
      "101 - 179" = "gold2",  
      "180 - 447" = "orange",
      "448 - 501" = "orangered",  
      "502 - 579" = "red3",
      "580 - 724" = "red4", 
      "Excluded"  = "black"
    ),
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

ggsave(out_png, p, width = 7, height = 7, dpi = 300)
message("Saved: ", out_png)