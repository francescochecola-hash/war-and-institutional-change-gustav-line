# Raw data

This directory contains placeholders for all raw input data used in the project.
Raw datasets are **not included** in this repository.

To replicate the analysis, users must obtain the original data from the sources
listed below and place the files in this directory using the filenames expected
by the import scripts.

---

## Replication datasets from published studies

### Fontana, Nannicini, and Tabellini (2023)

**Fontana, N., Nannicini, T., and Tabellini, G. (2023)**  
*Historical roots of political extremism: The effects of Nazi occupation of Italy*  
Journal of Comparative Economics, 51(3), 723–743  
https://doi.org/10.1016/j.jce.2023.05.006

Replication data are provided by the authors as supplementary material to the
published article.

Expected files:
- `replication_data_additional_data.dta`
- `replication_data_sample_analysis.dta`

---

### Gagliarducci et al. (2020)

**Gagliarducci, S., Onorato, M. G., Sobbrio, F., and Tabellini, G. (2020)**  
*War of the Waves: Radio and Resistance during World War II*  
American Economic Journal: Applied Economics, 12(4), 1–38  
https://doi.org/10.1257/app.20190410

Replication data:
- *Replication Data for: War of the Waves: Radio and Resistance during World War II*  
  American Economic Association / ICPSR  
  https://doi.org/10.3886/E111901V1

Expected file:
- `data_final_wow.dta`

---

## Italian Institutional Referendum (1946)

Municipality-level results from the Italian institutional referendum held in 1946.
These data provide the main outcome variable used in the empirical analysis.

The original dataset is distributed as a plain text file (`.txt`) and is not
included in this repository.

Source:
- Italian Ministry of the Interior (Ministero dell’Interno)  
  https://elezionistorico.interno.gov.it/eligendo/opendata.php

Expected file:
- `referendum-19460602.txt`
  
---

## Geographic data (GIS)

This project also relies on geographic data processed in QGIS, including:
- municipality boundaries as of 2001,
- centroids of municipalities,
- distances from each municipality to the nearest municipality located along
  the Gustav Line.

**Source:**
- Italian National Institute of Statistics (ISTAT)  
  https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/

All geographic files are stored locally in the `data/raw/gis/` directory and are
not included in this repository.

Users interested in replication should reproduce the GIS preprocessing steps
described in the documentation.
