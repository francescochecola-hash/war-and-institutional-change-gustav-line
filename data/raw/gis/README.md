# Geographic data (GIS)

This folder contains the geographic data used to construct municipality-level distance measures from the Gustav Line.

These files are used to compute the distance between each Italian municipality and the nearest municipality located along the Gustav Line, which serves as the running variable in the empirical analysis.

Due to size and licensing restrictions, some original geographic files are **not included** in this repository. Users interested in replication should obtain the raw geographic data from the original sources listed below and reproduce the preprocessing steps described here.

---

## Data sources

The main geographic source is:

**Italian National Institute of Statistics (ISTAT)**  
Administrative boundaries of Italian municipalities  
https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/

Specifically, the following data are required:

- Municipality boundaries (shapefiles)
- Municipality identifiers (ISTAT municipality codes)

These files allow the construction of municipality centroids and distance measures.

---

## GIS preprocessing steps

Geographic preprocessing is performed using QGIS. The main steps are:

1. Load municipality boundary shapefiles into QGIS.
2. Restrict the dataset to the relevant year and municipality definitions.
3. Compute municipality centroids using the polygon geometry.
4. Import or digitize the Gustav Line geographic reference.
5. Identify municipalities located along the Gustav Line.
6. Compute the distance between each municipality centroid and the nearest municipality located along the Gustav Line.
7. Export the resulting dataset containing:
   - municipality code  
   - centroid coordinates  
   - distance to the nearest Gustav Line municipality  

The final dataset is exported in a format readable by R (e.g., `.gpkg`, `.shp`, or `.csv`) and stored in this folder.

---

## Role in the project

The processed GIS data are used as input in the script:

- `03_import_gis_distance_gustav.R`

which converts the geographic data into `.rds` format and prepares them for merging with other datasets.

The distance measure constructed from these GIS files is a key variable in the empirical analysis.

---

## Reproducibility

To fully reproduce the GIS data preparation:

1. Download municipality boundary shapefiles from ISTAT.
2. Perform the preprocessing steps described above using QGIS.
3. Save the resulting dataset in this folder.
4. Run the import and data preparation scripts in the `scripts/` folder.

No manual modifications are applied after the GIS preprocessing stage.
