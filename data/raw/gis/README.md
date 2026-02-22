# Geographic data (GIS)

This folder contains the geographic data used to construct municipality-level distance measures from the Gustav Line.

The distance to the Gustav Line serves as the running variable in the geographic regression discontinuity design (RD) implemented in the empirical analysis.

Due to licensing restrictions, original geographic boundary files are **not included** in this repository. Users interested in replication must download the raw data from the original sources listed below and reproduce the preprocessing steps described here.

---

## Data sources

### 1. Municipality Boundaries

The geographic source is:

**Italian National Institute of Statistics (ISTAT)**  
Administrative boundaries of Italian municipalities  
https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/

To replicate the analysis, users must download the following dataset from the ISTAT website:

- **Versione non generalizzata (full resolution)**
- **Reference system: WGS84 / UTM zone 32N**
- **Year: 2001 (Census reference)**

Specifically, download the compressed folder:

```
Limiti2001.zip
```

After extracting the zip folder, navigate to:

```
Limiti2001/Com2001/
```

This folder contains the municipality boundary shapefile, consisting of the following required components:

- `.shp` — geometry file  
- `.shx` — shape index file  
- `.dbf` — attribute table containing municipality identifiers (including ISTAT municipality codes)  
- `.prj` — projection information  

These files together define the complete municipality boundary shapefile and must be kept in the same folder.

---

### 2. Gustav Line Coordinates

The historical path of the Gustav Line is reconstructed using:

```
gustavcoord_december.dta
```

This file is obtained from the replication package of:

Gagliarducci, Stefano, Massimiliano Gaetano Onorato, Francesco Sobbrio, and Guido Tabellini (2020),  
“War of the Waves: Radio and Resistance during World War II,”  
*American Economic Journal: Applied Economics*, 12(4): 1–38.

The replication materials are publicly available and are referenced in the main `data/raw` README file of this repository.

The dataset contains ordered projected coordinates (X, Y) defining the geographic trajectory of the Gustav defensive line as used in the original study.

---

# Loading Municipality Boundaries in QGIS

1. Open QGIS
2. Go to **Layer → Add Layer → Add Vector Layer**
3. Select the `.shp` file located in:

```
Limiti2001/Com2001/
```

---

# Coordinate Reference System (CRS)

Distance calculations must be performed in a projected coordinate system.

If the layer CRS is:

```
EPSG:4326 — WGS84
```

reproject it to:

```
EPSG:32632 — WGS 84 / UTM zone 32N
```

In QGIS:

**Right-click layer → Export → Save Features As…**
Set CRS to EPSG:32632.

All subsequent operations must use this projected CRS.

---

# Fixing Invalid Geometries

Some municipality geometries may be invalid.

Use:

Processing → Fix geometries

Output example:

```
comuni_2001_fixed.gpkg
```

All following steps must use the corrected layer.

---

# Constructing the Gustav Line LineString

The file `gustavcoord_december.dta` contains X and Y projected coordinates.

### Step 1 — Convert .dta to CSV (in R)

```r
library(haven)
library(readr)

gustav <- read_dta("path/to/gustavcoord_december.dta")
write_csv(gustav, "gustavcoord_december.csv")
```

### Step 2 — Import into QGIS

**Layer → Add Layer → Add Delimited Text Layer**

Geometry definition: Coordinate X/Y

- X field: `_X`
- Y field: `_Y`
- CRS: EPSG:32632

This creates a point layer.

### Step 3 — Convert Points to Line

**Processing → Points to Path**

- Input layer: gustavcoord_december
- Group field: `_ID`

This produces a LineString geometry representing the Gustav Line.

Verify:

- Geometry type: LineString
- Number of features: 1

If necessary, use:

**Processing → Dissolve**
to obtain a single continuous line.

Export as:

```
gustav_line_true.gpkg
```

### Step 3 — Compute Municipality Centroids

Distances are computed between municipality centroids and the Gustav Line.

**Processing → Centroids**

Input:

```
comuni_2001_fixed.gpkg
```

Output:

```
comuni_2001_centroids.gpkg
```

---

### Step 4 — Compute Distance to the Gustav Line

Distances are defined as the minimum Euclidean distance between each municipality centroid and the Gustav Line LineString.

In QGIS:

**Processing → Join attributes by nearest**

Input layer:
- `comuni_2001_centroids`

Input layer 2:
- `gustav_line_true`

Maximum nearest neighbors:
- 1

Export the resulting layer as:

```
comuni_2001_dist_gustav.gpkg
```

Distances are expressed in meters.

---

## Output

The final dataset contains one observation per municipality, including:

- Municipality identifier
- Distance to the Gustav Line (in meters)

This distance variable is imported into R using:

```
03_import_gis_distance_gustav.R
```

It serves as the running variable in the geographic RD design.

Distances are converted to kilometers within the R scripts.

---

## Methodological Note

The Gustav Line geometry is reconstructed directly from historical coordinate data (`gustavcoord_december.dta`) provided 
in the replication package of Gagliarducci et al. (2020).

Distances are computed between municipality centroids and the continuous LineString representing the historical front.

Because the line geometry is independent of administrative municipal boundaries, the resulting running variable is **exogenous**
to municipal administrative definitions and suitable for geographic regression discontinuity analysis.
