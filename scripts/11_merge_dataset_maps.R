# ==============================================================================
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
#
# Script: 11_merge_dataset_maps.R
# Purpose:
#   Merge 2001 municipality boundaries (sf polygons) with:
#     (1) GIS distance-to-Gustav output
#     (2) Fontana et al. selected dataset
#     (3) Referendum 1946 selected dataset
#   Keep only mapping-ready variables + key outcomes.
#
# Inputs:
#   data/processed/import/comuni_2001_boundaries.rds
#   data/processed/select/gis_gustav_distance_selected.rds
#   data/processed/select/fontana_et_al_selected.rds
#   data/processed/select/referendum_1946_selected.rds
#
# Output:
#   data/processed/merge/comuni_2001_boundaries_merged.rds
#
# Notes:
#   - Master dataset: comuni_2001_boundaries.rds (sf POLYGON/MULTIPOLYGON)
#   - Merge key: pro_com (master) == cod_istat103 (others)
#   - CRS check: EPSG:32632 (WGS 84 / UTM zone 32N)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(fs)
  library(sf)
})

# Paths
in_import_dir <- here("data", "processed", "import")
in_select_dir <- here("data", "processed", "select")
out_dir <- here("data", "processed", "merge")
out_rds <- here("data", "processed", "merge", "comuni_2001_boundaries_merged.rds")
dir_create(out_dir)

master_file  <- file.path(in_import_dir, "comuni_2001_boundaries.rds")
gis_file     <- file.path(in_select_dir, "gis_gustav_distance_selected.rds")
fontana_file <- file.path(in_select_dir, "fontana_et_al_selected.rds")
ref46_file   <- file.path(in_select_dir, "referendum_1946_selected.rds")

for (f in c(master_file, gis_file, fontana_file, ref46_file)) {
  if (!file_exists(f)) stop("Missing input file: ", f)
}

assert_no_duplicates <- function(df, key, df_name = "dataset") {
  dup <- df %>% count(.data[[key]]) %>% filter(n > 1)
  if (nrow(dup) > 0) stop(sprintf("Duplicated keys in %s on '%s'.", df_name, key))
  invisible(TRUE)
}

# helper: pick first existing column among candidates
pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ------------------------------------------------------------------------------
# 1) Master boundaries (sf)
# ------------------------------------------------------------------------------
comuni <- readRDS(master_file)
if (!inherits(comuni, "sf")) stop("comuni_2001_boundaries.rds is not an sf object.")

gtypes <- unique(as.character(st_geometry_type(comuni, by_geometry = TRUE)))
if (!all(gtypes %in% c("POLYGON", "MULTIPOLYGON"))) {
  stop("Master geometry types not polygonal. Found: ", paste(gtypes, collapse = ", "))
}

epsg <- st_crs(comuni)$epsg
if (is.na(epsg) || epsg != 32632) stop("Master CRS is not EPSG:32632. Found EPSG: ", epsg)

# master id column (your screenshot shows PRO_COM)
master_id_col <- pick_col(comuni, c("pro_com", "PRO_COM", "Pro_Com", "PROCOM", "procom"))
if (is.na(master_id_col)) {
  stop("Master is missing municipality id column. Tried: pro_com / PRO_COM / Pro_Com / PROCOM / procom")
}

comuni <- comuni %>%
  mutate(cod_istat103 = suppressWarnings(as.integer(as.character(.data[[master_id_col]]))))

assert_no_duplicates(comuni, "cod_istat103", "comuni_2001_boundaries.rds")

# ------------------------------------------------------------------------------
# 2) GIS distance (attach second)
# ------------------------------------------------------------------------------
gis <- readRDS(gis_file)

gis_id_col <- pick_col(gis, c("pro_com", "PRO_COM", "Pro_Com", "PROCOM", "procom"))
if (is.na(gis_id_col)) stop("GIS distance is missing pro_com/PRO_COM id column.")

need_gis <- c("gustav", "distance_gustav_km")
miss_gis <- setdiff(need_gis, names(gis))
if (length(miss_gis) > 0) stop("GIS distance missing: ", paste(miss_gis, collapse = ", "))

gis <- gis %>%
  mutate(cod_istat103 = suppressWarnings(as.integer(as.character(.data[[gis_id_col]])))) %>%
  select(cod_istat103, gustav, distance_gustav_km)

assert_no_duplicates(gis, "cod_istat103", "gis_gustav_distance_selected.rds")

# ------------------------------------------------------------------------------
# 3) Fontana + Ref46
# ------------------------------------------------------------------------------
fontana <- readRDS(fontana_file)
if (!("cod_istat103" %in% names(fontana))) stop("Fontana selected missing cod_istat103.")
if (!("occupation" %in% names(fontana))) stop("Fontana selected missing occupation.")

fontana <- fontana %>%
  mutate(cod_istat103 = suppressWarnings(as.integer(as.character(cod_istat103)))) %>%
  select(cod_istat103, occupation)

assert_no_duplicates(fontana, "cod_istat103", "fontana_et_al_selected.rds")

ref46 <- readRDS(ref46_file)
if (!("cod_istat103" %in% names(ref46))) stop("Referendum selected missing cod_istat103.")
if (!("perc_republic" %in% names(ref46))) stop("Referendum selected missing perc_republic.")

ref46 <- ref46 %>%
  mutate(cod_istat103 = suppressWarnings(as.integer(as.character(cod_istat103)))) %>%
  select(cod_istat103, perc_republic)

assert_no_duplicates(ref46, "cod_istat103", "referendum_1946_selected.rds")

# ------------------------------------------------------------------------------
# 4) Merge (left joins on master)
# ------------------------------------------------------------------------------
merged <- comuni %>%
  left_join(gis,     by = "cod_istat103") %>%
  left_join(fontana, by = "cod_istat103") %>%
  left_join(ref46,   by = "cod_istat103")

# ------------------------------------------------------------------------------
# 5) Keep only requested columns + geometry
# ------------------------------------------------------------------------------
geom_col <- attr(merged, "sf_column")

# name column in master (your screenshot shows COMUNE)
name_col <- pick_col(merged, c("name", "NAME", "comune", "COMUNE"))
if (is.na(name_col)) stop("Could not find municipality name column (tried name/COMUNE/comune).")

# keep code columns in the exact case they exist
keep_cols <- c(
  pick_col(merged, c("cod_rip", "COD_RIP")),
  pick_col(merged, c("cod_reg", "COD_REG")),
  pick_col(merged, c("cod_prov", "COD_PROV")),
  pick_col(merged, c("pro_com", "PRO_COM")),
  pick_col(merged, c("pro_com_t", "PRO_COM_T")),
  name_col,
  pick_col(merged, c("province103", "PROVINCE103")),
  pick_col(merged, c("region103", "REGION103")),
  "cod_istat103",
  "occupation",
  "perc_republic",
  "gustav",
  "distance_gustav_km",
  geom_col
)

keep_cols <- keep_cols[!is.na(keep_cols)]

merged <- merged %>%
  select(all_of(keep_cols)) %>%
  rename(geom = all_of(geom_col)) %>%
  st_as_sf(sf_column_name = "geom")

# ------------------------------------------------------------------------------
# 6) Save
# ------------------------------------------------------------------------------
saveRDS(merged, out_rds)
message("Saved: ", out_rds)
message("Geometry: ", paste(unique(as.character(st_geometry_type(merged))), collapse = ", "))
message("CRS EPSG: ", st_crs(merged)$epsg)