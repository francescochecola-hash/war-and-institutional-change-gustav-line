# Geographic data (GIS)

This folder contains the geographic data used to construct municipality-level distance measures from the Gustav Line.

These files are used to compute the distance between each Italian municipality and the nearest municipality located along the Gustav Line, which serves as the running variable in the empirical analysis.

Due to size and licensing restrictions, some original geographic files are **not included** in this repository. Users interested in replication should obtain the raw geographic data from the original sources listed below and reproduce the preprocessing steps described here.

---

## Data sources

The geographic source is:

**Italian National Institute of Statistics (ISTAT)**  
Administrative boundaries of Italian municipalities  
https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/

To replicate the analysis, users must download the following dataset from the ISTAT website:

- **Versione non generalizzata (full resolution)**
- **Reference system: WGS84 / UTM zone 32N**
- **Year: 2001 (Census reference)**

Specifically, download the compressed folder:
