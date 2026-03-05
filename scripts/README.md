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
The script harmonizes municipality names, matches them to 2001 ISTAT identifiers (using exact and fuzzy joins), 
computes key rates (turnout and vote shares), and saves the selected dataset in `data/processed/select/`.
