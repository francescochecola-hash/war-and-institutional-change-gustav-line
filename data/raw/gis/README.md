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

The replication materials are publicly available and are referenced in the main `data/raw/README` file of this repository.

The dataset contains ordered projected coordinates (X, Y) defining the geographic trajectory of the Gustav defensive line as used in the original study.

---

## Loading Municipality Boundaries in QGIS

1. Open QGIS
2. Go to **Layer → Add Layer → Add Vector Layer**
3. Select the `.shp` file located in:

```
Limiti2001/Com2001/
```

---

## Coordinate Reference System (CRS)

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

## Fixing Invalid Geometries

Some municipality geometries may be invalid.

Use:

Processing → Fix geometries

Output example:

```
comuni_2001_fixed.gpkg
```

All following steps must use the corrected layer.

---

## Constructing the Gustav Line LineString

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
gustav_line_december.gpkg
```

## Extending the Gustav Line

Before computing distances, the reconstructed Gustav Line must be extended so that it fully crosses the Italian peninsula.

This step ensures that the line defines a complete geographic partition of the study area and prevents edge effects in distance calculations.

**Processing → Toolbox → Extend lines**

Input layer:

```
gustav_line_december
```

Parameters:

- Start distance: `2000000`
- End distance: `2000000`

(The value is expressed in meters. A sufficiently large value must be used to ensure that the line extends well beyond the Italian bounding box.)

Output example:

```
gustav_line_extended.gpkg
```

The extended LineString must be used in all subsequent distance computations.

---

## Creating the Study Area Bounding Box

To ensure that the extended Gustav Line fully crosses the territorial domain used for distance calculations, create a bounding box based on the municipality layer.

**Processing → Create layer from extent**

Input layer:

```
comuni_2001_fixed.gpkg
```

Output:

```
italy_box.gpkg
```

This layer represents the full spatial extent of Italian municipalities used in the analysis.

The bounding box is used for visual verification to confirm that the extended Gustav Line completely intersects the study area.

---

## Splitting the Study Area by the Extended Gustav Line

To define the two sides of the front, split the study area bounding box using the extended Gustav Line.

**Processing → Split with lines**

Input layer:

```
italy_box.gpkg
```

Lines:

```
gustav_line_extended.gpkg
```

Output:

```
gustav_sides.gpkg
```

The resulting layer must contain exactly **two polygon features**, corresponding to the two sides of the Gustav Line.

If more than two polygons are produced, verify that:

- The Gustav Line is a single continuous LineString  
- The line fully intersects and crosses the bounding box  
- No geometry errors are present in either layer

---

## Assigning Municipalities to Gustav Line Sides

To determine on which side of the Gustav Line each municipality lies, perform a spatial join between municipalities and the split polygons.

**Processing → Join attributes by location**

Input layer:

```
comuni_2001_fixed.gpkg
```

Join layer:

```
gustav_sides.gpkg
```

Geometric predicate:
- `intersects`

Output:

```
municipalities_with_side.gpkg
```

The resulting layer assigns each municipality to one of the two polygons generated by the split operation.

Each municipality must be matched to exactly one side.

If municipalities are matched to multiple polygons, verify that:

- The split produced exactly two non-overlapping polygons  
- No invalid geometries remain in the municipality layer  
- The bounding box was correctly split by the extended Gustav Line 

---

## Creating the Side Indicator Variable

Open the layer:

```
municipalities_with_side.gpkg
```

The attribute table must contain exactly **two rows**, corresponding to the two polygons created by the split operation.

### Step 1 — Enable Editing Mode

Click the **Toggle Editing** button (✏️) to activate editing mode.

### Step 2 — Create a Numeric Field

Open the **Field Calculator**.

Create a new field with the following properties:

- Output field name: `gustav`
- Output field type: Integer (Whole number)

### Step 3 — Assign Value 1 to the Northern Side

Select the polygon located north of the Gustav Line.

In the Field Calculator:

- Check: **Update selected features only**
- Expression:

```
1
```

Click **Run**.

### Step 4 — Assign Value 0 to the Southern Side

Select the polygon located south of the Gustav Line.

In the Field Calculator:

- Select **Update existing field → gustav**
- Check: **Update selected features only**
- Expression:

```
0
```

Click **Run**.

Save the edits.

The layer `municipalities_gustav.gpkg` must now contain:

| gustav |
|--------|
| 1      |
| 0      |

The variable `gustav` defines the binary geographic partition used to construct the signed running variable.

---

## Joining the Side Indicator to Municipalities

To assign the Gustav Line side indicator to each municipality, perform a spatial join.

**Processing → Join attributes by location**

Fill the parameters as follows:

Join to features in:

```
comuni_2001_fixed.gpkg
```

By comparing to:

```
municipalities_with_side.gpkg
```

Geometric predicate:
- `intersects`

Fields to add:

Click **…** and select **only**:

```
gustav
```

Join type:

Select:
- **Take attributes of the first matching feature only (one-to-one)**  

(This is essential to avoid duplicating municipalities.)

Output:

Save as:

```
municipalities_gustav.gpkg
```

Click **Run**.

---

## Validation Check

Open the attribute table of:

```
municipalities_gustav.gpkg
```

Verify that:

- The number of rows is **8101**  
- The column `gustav` is present  
- No `NULL` values appear in the `gustav` field  

If any of these conditions are not satisfied, review the spatial join settings and ensure that the join type was set to one-to-one.

---

## Compute Municipality Centroids

Distances are computed between municipality centroids and the Gustav Line.

**Processing → Centroids**

Input:

```
municipalities_gustav.gpkg
```

Output:

```
municipalities_gustav_centroids.gpkg
```

---

## Compute Distance to the Extended Gustav Line

Distances are defined as the minimum Euclidean distance between each municipality centroid and the extended Gustav Line.

**Processing → Distance to nearest hub**

Input layer:

```
municipalities_gustav_centroids.gpkg
```

Hub layer:

```
gustav_line_extended.gpkg
```

Output:

```
municipalities_dist_centroids.gpkg
```

This operation creates a new field:

```
HubDist
```

which stores the minimum Euclidean distance (in meters) between each municipality centroid and the extended Gustav Line.

---

## Construct Distance Variables

Open the attribute table of:

```
municipalities_dist_centroids.gpkg
```

### 1. Rename the Distance Variable

For clarity, rename the automatically generated field `HubDist` to:

```
distance
```

This variable represents the Euclidean distance from the Gustav Line (in meters).

### 2. Construct the Signed Running Variable

Open the **Field Calculator** and create a new field:

- Name: `distance_gustav`
- Type: Decimal number

Use the following expression:

```
CASE
WHEN "gustav" = 1 THEN "distance"
ELSE - "distance"
END
```

The resulting variable has the following interpretation:

- Municipalities North of the Gustav Line: `distance_gustav > 0`
- Municipalities South of the Gustav Line: `distance_gustav < 0`
- Cutoff (Gustav Line): `distance_gustav = 0`

This signed distance (in meters) is the running variable used in the geographic regression discontinuity design.

---

## Output

The final dataset contains one observation per municipality, including:

- Municipality identifier
- `gustav` — Binary indicator equal to 1 if the municipality lies north of the Gustav Line and 0 if it lies south
- `distance` — Euclidean distance to the Gustav Line (in meters)  
- `distance_gustav` — Signed distance to the Gustav Line (in meters)  

The dataset is imported into R using:
```
03_import_gis_distance_gustav.R
```

Distances are converted to kilometers within the R scripts.

---

## Methodological Note

The Gustav Line geometry is reconstructed directly from historical coordinate data (`gustavcoord_december.dta`) provided 
in the replication package of Gagliarducci et al. (2020).

Distances are computed between municipality centroids and the continuous LineString representing the historical front.

Because the line geometry is independent of administrative municipal boundaries, the resulting running variable is **exogenous**
to municipal administrative definitions and suitable for geographic regression discontinuity analysis.
