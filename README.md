



# Eastern White Pine Health Assessment

A remote sensing pipeline for classifying the health of Eastern White Pine (*Pinus strobus*) forests across Maine, New Hampshire, and Vermont using multi-source satellite imagery and machine learning.

## Overview

The project covers 154 field plots and classifies forest health into three classes — **Healthy**, **Mod Healthy**, and **Unhealthy** — using:

- **Sentinel-2** (10/20 m): 25 spectral indices (NDVI, EVI, GNDVI, S2REP, etc.) + raw bands, monthly composites Apr–Nov
- **Sentinel-1 GRD** (10 m): VV/VH backscatter + GLCM texture features, monthly composites
- **USGS 3DEP** (10 m): elevation, slope, aspect

## Demo
https://github.com/user-attachments/assets/814861df-46e4-42b0-8e98-2c37517a49c0

## Repository Structure


## Workflow

1. **Feature extraction** (`s1_s2_extraction.py`): Run in Google Colab. Authenticates to GEE, extracts Sentinel-1, Sentinel-2, GEDI, terrain, and NLCD features at all 154 field plot locations, and exports a CSV to Google Drive.

2. **Model training** (`health_class_classification.R`): Loads the extracted CSV, runs correlation pruning + Boruta feature selection, trains five classifiers (RF, SVM, GBM, XGBoost, CatBoost), evaluates with 10-fold CV, and saves the best model.

3. **Spatial prediction** (`gee_export_raster_for_prediction.js` + `02_r_predict_on_raster.R`): Exports the selected feature rasters from GEE, then applies the saved R model to produce wall-to-wall health class maps.

4. **Visualization** (`ewp_app.js`): A GEE web app to explore annual health predictions (2023–2025) by state, county, named site, or drawn AOI.

## Dependencies

**Python** (Colab): `earthengine-api`, `geemap`, `geopandas`, `pandas`, `numpy`, `rasterio`, `scikit-learn`, `openpyxl`

**R**: `randomForest`, `xgboost`, `catboost`, `caret`, `Boruta`, `ggplot2`, `readxl`, `openxlsx`, `showtext`

**GEE assets** (private, `projects/ee-garimayadav/assets/`): site boundary polygons, SDI mask raster, annual prediction rasters

## Study Area

Maine, New Hampshire, and Vermont — 154 field plots visited in 2023 and 2024.
