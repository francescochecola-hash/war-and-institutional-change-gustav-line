# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 19_figure2_map_within100km_republic.R
# Purpose:
#   Map municipalities within [-100, 100] km from Gustav line,
#   colored by perc_republic (blue gradient), with Gustav line overlay,
#   and NA perc_republic shown as "Excluded" in black with its own legend key.
#
# Inputs:
#   data/processed/merge/comuni_2001_boundaries_merged.rds
#   data/processed/import/gustav_line.rds
#
# Output:
#   results/figures/figure2_map_within100km_republic.png
#
# Notes:
#   - Filter: distance_gustav_km in [-100, 100] inclusive
#   - Exclude specific municipalities by cod_istat103
#   - Color: perc_republic (light -> dark blue, continuous)
#   - NA perc_republic -> "Excluded" (black) shown as separate legend item
#   - No municipal borders
#   - Gustav line in black with legend key (line)
#   - CRS: enforce EPSG:32632 for both layers
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(here)
  library(scales)
  library(ggnewscale)  # <- needed for 2 fill scales
})

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
in_comuni <- here("data", "processed", "merge", "comuni_2001_boundaries_merged.rds")
in_line   <- here("data", "processed", "import", "gustav_line.rds")

out_png <- here("results", "figures", "figure2_map_within100km_republic.png")
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
comuni <- readRDS(in_comuni)
gline  <- readRDS(in_line)

if (!inherits(comuni, "sf")) stop("comuni_2001_boundaries_merged.rds is not an sf object.")
if (!inherits(gline,  "sf")) stop("gustav_line.rds is not an sf object.")

# ------------------------------------------------------------------------------
# CRS: enforce EPSG:32632 for both layers
# ------------------------------------------------------------------------------
target_epsg <- 32632
if (is.na(st_crs(comuni)$epsg)) stop("comuni has missing CRS.")
if (is.na(st_crs(gline)$epsg))  stop("gustav_line has missing CRS.")

if (st_crs(comuni)$epsg != target_epsg) comuni <- st_transform(comuni, target_epsg)
if (st_crs(gline)$epsg  != target_epsg) gline  <- st_transform(gline,  target_epsg)

# ------------------------------------------------------------------------------
# Filter within [-100, 100] km + exclude specific cod_istat103
# ------------------------------------------------------------------------------
need_vars <- c("distance_gustav_km", "perc_republic", "cod_istat103")
miss <- setdiff(need_vars, names(comuni))
if (length(miss) > 0) stop("Missing variables in comuni: ", paste(miss, collapse = ", "))

exclude_ids <- c(
  63049, 59033, 59018, 63037, 63007, 63014, 63004,
  71026, 63019, 63078, 63047, 63061, 63031, 63038,
  82075, 81020, 81009, 81014, 81024, 81011, 81021,
  81013, 81008, 81022, 81002, 81005, 81007
)

comuni_filt <- comuni %>%
  mutate(
    distance_gustav_km = as.numeric(distance_gustav_km),
    perc_republic      = as.numeric(perc_republic),
    cod_istat103       = as.integer(cod_istat103)
  ) %>%
  filter(!is.na(distance_gustav_km)) %>%
  filter(distance_gustav_km >= -100, distance_gustav_km <= 100) %>%
  filter(!(cod_istat103 %in% exclude_ids))

if (nrow(comuni_filt) == 0) stop("No municipalities found within [-100, 100] km after exclusions.")

comuni_ok  <- comuni_filt %>% filter(!is.na(perc_republic))
comuni_na  <- comuni_filt %>% filter(is.na(perc_republic)) %>%
  mutate(excluded = "Excluded")  # dummy categorical for legend

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
p <- ggplot() +
  # 1) Non-NA: continuous blue gradient
  geom_sf(
    data = comuni_ok,
    aes(fill = perc_republic),
    color = NA,
    linewidth = 0
  ) +
  scale_fill_gradient(
    low  = "#deebf7",
    high = "#08519c",
    name = "Repubblica (%)",
    labels = label_number(accuracy = 1)
  ) +
  
  # 2) Start a NEW fill scale for Excluded
  ggnewscale::new_scale_fill() +
  
  # NA: black, with its own legend key
  geom_sf(
    data = comuni_na,
    aes(fill = excluded),
    color = NA,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = c("Excluded" = "black"),
    name   = NULL
  ) +
  
  # Gustav line in black with legend
  geom_sf(
    data = gline,
    aes(color = "Gustav Line"),
    linewidth = 0.8
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
    fill = guide_legend(order = 3) # this applies to the Excluded scale (second fill)
  ) +
  
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  )

ggsave(out_png, plot = p, width = 8, height = 8, dpi = 300)
message("Saved: ", out_png)