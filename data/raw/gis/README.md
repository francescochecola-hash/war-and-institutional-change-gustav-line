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

## Loading data in QGIS

To load municipality boundaries into QGIS:

1. Open QGIS
2. Go to **Layer → Add Layer → Add Vector Layer**
3. Select the `.shp` file located in:

```
Limiti2001/Com2001/
```

QGIS will load the municipality boundary layer.

---

## Coordinate Reference System (CRS) check and reprojection

Before computing centroids and distances, it is essential to ensure that the layer uses a projected 
Coordinate Reference System (CRS) suitable for distance calculations.

### Step 1 — Check the current CRS

In QGIS:
 
1. Click **Properties → Source** on the municipality layer   
2. Check the CRS

If the CRS is:

```
EPSG:4326 — WGS84
```

this CRS uses geographic coordinates (degrees) and is **not suitable for distance calculations**.
Distance calculations must be performed using a projected CRS in **meters**.

---

### Step 2 — Reproject the layer

Reproject the layer using the following CRS:

```
EPSG:32632 – WGS 84 / UTM zone 32N
```

This projected CRS is appropriate for Italy and allows accurate distance calculations.

In QGIS:

- Select **Export → Save Features As…** on the municipality layer

Set:

- Format: ESRI Shapefile  
- CRS: EPSG:32632 — WGS 84 / UTM zone 32N+
  
---

## Identifying municipalities located along the Gustav Line

This step creates a layer containing only the municipalities located along the Gustav Line. These municipalities serve as the reference units for computing distance measures.

### Step 1 — Open the municipality layer

Load the reprojected municipality layer in QGIS.

Open the attribute table:

- Click **Open Attribute Table** on the layer

---

### Step 2 — Select municipalities along the Gustav Line

Use the expression-based selection tool:

- Click **Select by Expression**

Enter the following expression:

```
"COMUNE" IN (
'Gaeta',
'Formia',
'Minturno',
'Santi Cosma e Damiano',
'Castelforte',
'Ausonia',
'Cassino',
'Coreno Ausonio',
'Vallemaio',
'Sant''Apollinare',
'Sant''Elia Fiumerapido',
'Vallerotonda',
'San Biagio Saracinisco',
'Picinisco',
'Alfedena',
'Scontrone',
'Castel di Sangro',
'Roccaraso',
'Ateleta',
'San Pietro Avellana',
'Palena',
'Taranta Peligna',
'Lama dei Peligni',
'Fara San Martino',
'Pennapiedimonte',
'Guardiagrele',
'Orsogna',
'Poggiofiorito',
'Arielli',
'Crecchio',
'Ortona'
)
```

This selects all municipalities located along the Gustav Line.

---

### Step 3 — Export selected municipalities as a new layer

With the municipalities selected:

- Click **Export → Save Selected Features As…** on the municipality layer

Set:

- Format: ESRI Shapefile  
- CRS: EPSG:32632 — WGS 84 / UTM zone 32N  

This creates a new layer containing only municipalities located along the Gustav Line.

---

## Computing municipality centroids

The objective is to compute, for each Italian municipality, the minimum distance to the nearest municipality located along the Gustav Line.

Formally, for each municipality *i*, the distance measure is defined as:

dist_i = min d(i, g), for g ∈ Gustav

where distances are computed between municipality centroids.

Because distance calculations must be performed between points rather than polygons, municipality boundaries must be converted into centroids.

---

### Step 1 — Compute centroids for all municipalities

Load the reprojected municipality layer in QGIS.

Open the Processing Toolbox:

- Go to **Processing → Toolbox**
- Search for: **Centroids**
- Select: **Vector geometry → Centroids**

Set:

- Input layer: the reprojected municipality layer
- CRS: EPSG:32632 — WGS 84 / UTM zone 32N

This creates a point layer containing one centroid for each municipality.

Distances computed using this layer will be expressed in meters because the CRS uses projected coordinates.

---

### Step 2 — Fix invalid geometries

Some municipality geometries may be invalid, which prevents centroid computation.

To fix geometries:

1. Open the **Processing Toolbox**
2. Search for: **Fix geometries**
3. Select: **Vector geometry → Fix geometries**
4. Set:

   - Input layer: `...`
   - Output file (example):

```
comuni_2001_fixed.gpkg
```

This creates a corrected municipality layer with valid geometries.
All subsequent operations should be performed using this corrected layer.

---

### Step 3 — Compute centroids for Gustav Line municipalities only

After fixing geometries and creating the corrected municipality layer (`comuni_2001_fixed.gpkg`), compute centroids only 
for the municipalities located along the Gustav Line.

First, select the 31 Gustav Line municipalities:

- Click **Select by Expression** on `comuni_2001_fixed.gpkg`
- Use the same expression defined in `Step 2 — Select municipalities along the Gustav Line`

With the municipalities selected:

- Click **Export → Save Selected Features As…** on `comuni_2001_fixed.gpkg`

This creates a point layer containing centroids for all municipalities located along the Gustav Line.

---

## Computing distance to the nearest Gustav Line municipality

This step computes, for each Italian municipality, the **minimum distance** to the nearest municipality located along the Gustav Line.

Distances are computed between municipality centroids using a projected coordinate system (EPSG:32632 — WGS 84 / UTM zone 32N). 
Distances are initially expressed in meters and later converted to kilometers in the empirical analysis.

### Step 1 — Compute distance to nearest Gustav Line municipality

Use the nearest-neighbor distance tool.

In QGIS:

1. Open **Processing Toolbox**
2. Navigate to:

```
Vector analysis → Distance to nearest hub (points)
```

3. Set:

**Input point layer:**

```
comuni_2001_centroids
```

**Hub layer:**

```
gustav_line_centroids
```

**Hub layer name attribute:**

Select:

- Municipality name

**Distance units:**

Meters (default, since CRS is projected)

**Output file (example):**

```
comuni_2001_distance_gustav.gpkg
```
---

## Output

The resulting layer contains one observation per municipality, including:

- Municipality identifier  
- Nearest Gustav Line municipality  
- Minimum distance to the Gustav Line (in meters)

This distance variable is the key geographic measure used in the empirical analysis.

The resulting dataset is exported and imported into R using the script `03_import_gis_distance_gustav.R`.
