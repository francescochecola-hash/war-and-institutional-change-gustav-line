# War and Institutional Change: The Case of Gustav Line

This repository contains data, tables, and reproducible analysis supporting
the thesis **"War and Institutional Change: The Case of Gustav Line"**.

## Project structure

```
├── data/
│   ├── raw/                  # Original raw data (excluded from version control)
│   └── processed/            # Datasets generated during data preparation
│       ├── import/           # Raw-to-R imports (.dta/.txt/.gpkg → .rds)
│       ├── select/           # Variable selection and harmonization
│       └── merge/            # Merged datasets used for analysis
├── scripts/                  # R scripts for data preparation and analysis
├── results/                  # Tables and figures produced by the analysis
├── README.md                 # Project overview and replication instructions
└── LICENSE                   # License information
```

## Raw data

The analysis is based on replication datasets from the following studies:

- **Fontana, N., Nannicini, T., and Tabellini, G. (2023)**  
  *Historical roots of political extremism: The effects of Nazi occupation of Italy*  
  Journal of Comparative Economics, 51(3), 723–743  
  https://doi.org/10.1016/j.jce.2023.05.006

- **Gagliarducci, S., Onorato, M. G., Sobbrio, F., and Tabellini, G. (2020)**  
  *War of the Waves: Radio and Resistance during World War II*  
  American Economic Journal: Applied Economics, 12(4), 1–38  
  https://doi.org/10.1257/app.20190410

In addition, the analysis uses municipality-level results from the Italian
institutional referendum of 1946, which constitute the main outcome variable
in the empirical analysis.

The project also relies on geographic data processed in QGIS, including
municipality boundaries as of 2001 and distance measures from each municipality
to the nearest municipality located along the Gustav Line.

Raw data are not included in this repository. Detailed information on data
sources, access instructions, and GIS preprocessing steps is provided in
`data/raw/README.md`.
