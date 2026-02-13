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

4. Click **Add**

QGIS will load the municipality boundary layer.

---

## Coordinate Reference System (CRS) check and reprojection

Before computing centroids and distances, it is essential to ensure that the layer uses a projected Coordinate Reference System (CRS) suitable for distance calculations.

### Step 1 — Check the current CRS

In QGIS:

1. Right-click on the municipality layer  
2. Click **Properties → Source**  
3. Check the CRS

If the CRS is:

```
EPSG:4326 — WGS84
```

this CRS uses geographic coordinates (degrees) and is **not suitable for distance calculations**.

Distance calculations must be performed using a projected CRS in **meters**.

---

### Step 2 — Reproject the layer

Reproject the layer using the following CRS:
