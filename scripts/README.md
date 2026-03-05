# Scripts

This folder contains the R scripts used in the replication workflow of the project *“War and Institutional Change: The Case of the Gustav Line”*.  
Each script performs a specific step in the data preparation or analysis pipeline.

## Software requirements

The scripts were written and tested using:

- **R** version 4.2 or higher  
- **RStudio** (recommended)

The following R packages are required:

- `here` (1.0.2)
- `fs` (1.6.6)
- `tidyverse` (2.0.0)
- `sf` (1.0-21)
- `rdrobust` (3.0.0)
- `rddensity` (2.6)
- `fixest` (0.13.2)
- `haven` (2.5.5)
- `tools` (4.4.2)
- `fuzzyjoin` (0.1.6.1)
- `stringdist` (0.9.15)
- `broom` (1.0.7)
- `sandwich` (3.1-1)
- `lmtest` (0.9-40)
- `ggplot2` (3.5.1)
- `ggnewscale` (0.5.2)

Package versions correspond to those used to produce the results in the paper.

## Script descriptions

### 01_import_replication_data.R

Imports the original replication datasets in Stata format (`.dta`) and converts them into R objects (`.rds`).  
The script reads the raw files from `data/raw/` and saves the processed versions in `data/processed/import/`.  
It also checks that input files exist and creates the required output directories if needed.

### 02_import_referendum_1946_data.R

Imports the raw dataset of the 1946 Italian institutional referendum from a delimited text file (`.txt`) and converts it into an R object (`.rds`).  
The script reads the data from `data/raw/`, standardizes variable names, and saves the processed dataset in `data/processed/import/`.

### 03_import_gis_distance_gustav.R

Imports the GIS dataset containing municipality-level distance to the Gustav Line from a GeoPackage file (`.gpkg`).
The script removes spatial geometry, standardizes variable names, constructs distance measures used in the analysis, 
and and converts it into an R object (`.rds`) in the processed dataset in `data/processed/import/`.

### 04_import_comuni_2001_boundaries.R

Imports the ISTAT 2001 municipal boundaries from a GeoPackage file (`.gpkg`).  
The script loads the spatial polygons, verifies the geometry type, and saves the dataset as an R object (`.rds`) in `data/processed/import/`.

### 05_import_gustav_line.R

Imports the Gustav Line geometry from a GeoPackage file (`.gpkg`).  
The script loads the spatial line layer, verifies the geometry type, and saves the dataset as an R object (`.rds`) in `data/processed/import/`.

### 06_select_fontana_et_al_data.R

Selects the variables used in the analysis from the replication datasets of Fontana, Nannicini and Tabellini (2023).  
The script extracts the relevant variables, merges the datasets by municipality identifiers, and saves the resulting dataset in `data/processed/select/`.

### 07_select_gagliarducci_et_al_data.R

Selects the variables used in the analysis from the dataset of Gagliarducci et al. (2020). 
The script collapses the panel dataset to one observation per municipality, extracts the relevant variables, 
and saves the resulting dataset in `data/processed/select/`.

### 08_select_referendum_1946_data.R

Prepares municipality-level outcomes from the 1946 Italian institutional referendum for use in the analysis.  
The script harmonizes municipality names, matches them to 2001 ISTAT identifiers (using exact and fuzzy joins), computes key rates (turnout and vote shares), and saves the selected dataset in `data/processed/select/`.

### 09_select_gis_distance_gustav_data.R

Selects the variables used in the analysis from the dataset containing municipality-level distance to the Gustav Line.  
The script extracts the relevant identifiers and distance measures and saves the resulting dataset in `data/processed/select/`.

### 10_merge_dataset.R

Merges the municipality-level datasets (GIS distance, Fontana et al., Gagliarducci et al., and the 1946 referendum) 
using the ISTAT municipality identifier (`cod_istat103`).  
The script computes the Euclidean distance from municipality points to the Gustav Line and creates a set 
of derived variables used in the empirical specifications, then saves the merged dataset in `data/processed/merge/`.

### 11_merge_dataset_maps.R

Merges the 2001 municipality boundaries (sf polygons) with selected datasets (GIS distance to the Gustav Line, Fontana et al., and the 1946 referendum) 
to create a mapping-ready spatial dataset.  
The script performs CRS and key checks, extracts only the variables needed for maps, and saves the merged sf object in `data/processed/merge/`.

### 12_table_1_summary_stats.R

Computes summary statistics for the full Italy sample and for municipalities within 100 km of the Gustav Line.  
The script summarizes key variables (N, mean, SD, min, max) and exports the resulting table to `results/tables/table1_summary_stats.csv`.

### 13_table_2_balance_checks.R

Runs balance checks (covariate continuity) around the Gustav Line for municipalities within 100 km, estimating linear models with longitude/latitude controls.  
The script reports treatment coefficients with robust (HC1) and Conley (spatial HAC) standard errors across multiple bandwidths (100/75/50 km), 
exporting results to `results/tables/table2_balance_checks.csv`.

### 14_table_3_main_results.R

Estimates the main specifications using a Geographic Regression Discontinuity Design around the Gustav Line.  
The script computes treatment effects across multiple bandwidths (100/75/50 km) with and without controls, reporting robust (HC1) and Conley 
(spatial HAC) standard errors and exporting results to `results/tables/table3_main_results.csv`.

### 15_table_4_rdrobust_main_results.R

Estimates treatment effects using a Geographic Regression Discontinuity Design with local polynomial RD inference via `rdrobust` (Calonico–Cattaneo–Titiunik).  
The script reports RD estimates with robust standard errors and 95% confidence intervals (with and without covariates) across multiple bandwidths (100/75/50 km), 
exporting results to `results/tables/table4_rdrobust_main_results.csv`.

### 16_table_5_rdrobust_datadriven.R

Estimates treatment effects using a Geographic Regression Discontinuity Design with data-driven bandwidth selection in `rdrobust` (e.g., MSE- or CER-optimal).  
The script reports robust RD estimates (tau, SE, p-value, 95% CI) with and without covariates and exports 
results to `results/tables/table5_rdrobust_datadriven.csv`.

### 17_table_6_rddensity_mccrary.R

Runs a McCrary density test at the cutoff using `rddensity` to assess potential sorting around the Gustav Line.  
The script applies the test to the signed distance running variable (excluding observations at zero) and exports 
results to `results/tables/table6_rddensity_mccrary.csv`.

### 18_figure_map_italy_occupation.R

Creates a municipality-level map of Italy colored by the length of occupation and overlays the Gustav Line.  
The script loads the merged municipality boundaries and line geometry of Gustav Line, [standardizes CRS, bins occupation into categories], 
and saves the figure to `results/figures/figure1_map_italy_occupation.png`.

### 19_figure_2_map_within100km_republic.R

Maps municipalities within ±100 km of the Gustav Line, colored by the 1946 Republic vote share, and overlays the Gustav Line.  
The script applies distance and municipality exclusions, marks missing referendum outcomes as “Excluded”, 
and saves the figure to `results/figures/figure2_map_within100km_republic.png`.

### 20_figure_3_rd_plot.R

Creates the main Geographic RD plot for the 1946 Republic vote share against signed distance to the Gustav Line.  
The script produces a binned scatter with separate polynomial fits (and confidence intervals) on each side of the cutoff 
and saves the figure to `results/figures/figure3_rd_plot_main.png`.

### 21_figure_4_rdrobust_plot.R

Generates the `rdrobust` RD plot for the 1946 Republic vote share against signed distance to the Gustav Line (cutoff at 0).  
The script excludes observations at the cutoff, produces the standard `rdplot` visualization, and saves the figure to `results/figures/figure4_rdrobust_plot.png`.

### 22_figure_5_mccrary_plot.R

Generates the McCrary density test plot at the cutoff using `rddensity`, based on the signed distance to the Gustav Line.  
The script excludes observations at the cutoff, restricts the range around the threshold, and saves the figure to `results/figures/figure5_mccrary_plot.png`.
