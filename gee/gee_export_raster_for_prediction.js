/**
 * gee_export_raster_for_prediction.js
 * ====================================
 * Google Earth Engine script to export FULL RASTER (all pixels) for R prediction
 *
 * Exports complete feature stack (all pixels within mask) as multi-band GeoTIFF
 *
 * Usage in GEE Code Editor:
 *   1. Paste this script into https://code.earthengine.google.com/
 *   2. Update MASK_ASSET_PATH (your raster mask)
 *   3. Update SELECTED_VARS (from R metadata)
 *   4. Run → Monitor Tasks tab
 *   5. Download GeoTIFF from Drive
 *   6. Use in R: 02_r_predict_on_raster.R
 */

// ============================================================
// ⚙️ CONFIGURATION
// ============================================================

// 📍 YOUR RASTER MASK (GEE Asset)
// Format: 1 = inside AOI, 0 = outside
var MASK_ASSET_PATH = "users/garimayadav/maine_aoi_mask";  // ← UPDATE THIS

// ✅ SELECTED VARS from R metadata
// Copy from R output: selected_vars list
var SELECTED_VARS = [
  // ← REPLACE WITH YOUR FEATURE LIST FROM R
  'NDVI_Apr', 'NDVI_May', 'NDVI_Jun', 'NDVI_Jul', 'NDVI_Aug',
  'NDVI_Sep', 'NDVI_Oct', 'NDVI_Nov',
  'elevation_10m', 'slope_10m', 'aspect_10m'
];

var EXPORT_YEAR = 2024;

// Phenological windows (same as R training)
var MONTH_WINDOWS = {
  "Apr": ["04-01", "04-30"],
  "May": ["04-25", "06-05"],
  "Jun": ["05-25", "07-08"],
  "Jul": ["06-25", "07-31"],
  "Aug": ["07-27", "09-04"],
  "Sep": ["09-01", "09-30"],
  "Oct": ["09-24", "11-05"],
  "Nov": ["10-25", "12-05"]
};

// ============================================================
// 🌍 LOAD MASK
// ============================================================
print("Loading raster mask from GEE Assets...");
var mask_raster = ee.Image(MASK_ASSET_PATH);
print("✓ Mask loaded: " + MASK_ASSET_PATH);
print("  Bands: " + mask_raster.bandNames().getInfo());

// Get AOI from mask
var roi = mask_raster.select(0).eq(1).geometry(100);
print("✓ ROI geometry extracted from mask");

// ============================================================
// ☁️ SENTINEL-2 CLOUD MASKING (SCL only)
// ============================================================
function maskS2Clouds(image) {
  var scl = image.select('SCL');
  var mask = scl.neq(3)
    .and(scl.neq(8))
    .and(scl.neq(9))
    .and(scl.neq(10));
  return image.updateMask(mask).divide(10000);
}

// ============================================================
// 📊 SENTINEL-2 SPECTRAL INDICES
// ============================================================
function calculateIndices(image) {
  var NDVI = image.expression('(NIR - RED) / (NIR + RED)', {
    'NIR': image.select('B8A'),
    'RED': image.select('B4')
  }).rename('NDVI');

  var EVI8 = image.expression('2.5 * (NIR - RED) / (1 + NIR + 6*RED - 7.5*BLUE)', {
    'NIR': image.select('B8A'),
    'RED': image.select('B4'),
    'BLUE': image.select('B2')
  }).rename('EVI8');

  var SAVI = image.expression('1.5 * (NIR - RED) / (NIR + RED + 0.5)', {
    'NIR': image.select('B8A'),
    'RED': image.select('B4')
  }).rename('SAVI');

  var MSR = image.expression('((RE3/RED) - 1) / sqrt((RE3/RED) + 1)', {
    'RE3': image.select('B7'),
    'RED': image.select('B4')
  }).rename('MSR');

  var CRI2 = image.expression('(1/BLUE) - (1/RE1)', {
    'BLUE': image.select('B2'),
    'RE1': image.select('B5')
  }).rename('CRI2');

  var NDII11 = image.expression('(NIR - SWIR1) / (NIR + SWIR1)', {
    'NIR': image.select('B8A'),
    'SWIR1': image.select('B11')
  }).rename('NDII11');

  var GCI = image.expression('(NIR/GREEN) - 1', {
    'NIR': image.select('B8A'),
    'GREEN': image.select('B3')
  }).rename('GCI');

  var Clre = image.expression('(RE3/RE1) - 1', {
    'RE3': image.select('B7'),
    'RE1': image.select('B5')
  }).rename('Clre');

  var IRECI = image.expression('(RE2/RE1) * (RE3 - RED)', {
    'RE2': image.select('B6'),
    'RE1': image.select('B5'),
    'RE3': image.select('B7'),
    'RED': image.select('B4')
  }).rename('IRECI');

  return image.addBands([NDVI, EVI8, SAVI, MSR, CRI2, NDII11, GCI, Clre, IRECI]);
}

// ============================================================
// 🏞️ BUILD S2 MONTHLY COMPOSITES (clipped to mask)
// ============================================================
print("\n📊 Building S2 composites for " + EXPORT_YEAR + "...");
var s2_composites = {};

Object.keys(MONTH_WINDOWS).forEach(function(month) {
  var dates = MONTH_WINDOWS[month];
  var startDate = EXPORT_YEAR + "-" + dates[0];
  var endDate = EXPORT_YEAR + "-" + dates[1];

  var col = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
    .filterDate(startDate, endDate)
    .filterBounds(roi)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 40))
    .select(['B2','B3','B4','B5','B6','B7','B8','B8A','B11','B12','SCL'])
    .map(maskS2Clouds)
    .map(calculateIndices);

  var n = col.size().getInfo();
  if (n === 0) {
    print("  [WARN] No S2 for " + month + " " + EXPORT_YEAR);
    s2_composites[month] = null;
  } else {
    // Clip to mask
    s2_composites[month] = col.median()
      .clip(roi)
      .updateMask(mask_raster.select(0));
    print("  [OK] S2 " + month + " " + EXPORT_YEAR + ": " + n + " scenes");
  }
});

// ============================================================
// 📡 SENTINEL-1 BACKSCATTER
// ============================================================
print("\n📡 Building S1 composite for " + EXPORT_YEAR + "...");
var s1_composite = null;

var s1_col = ee.ImageCollection('COPERNICUS/S1_GRD')
  .filterDate(EXPORT_YEAR + "-01-01", EXPORT_YEAR + "-12-31")
  .filterBounds(roi)
  .filter(ee.Filter.eq('instrumentMode', 'IW'))
  .filter(ee.Filter.eq('orbitProperties_pass', 'ASCENDING'))
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  .select(['VV', 'VH']);

var n_s1 = s1_col.size().getInfo();
if (n_s1 > 0) {
  s1_composite = s1_col.median()
    .clip(roi)
    .updateMask(mask_raster.select(0));
  print("  [OK] S1 " + EXPORT_YEAR + ": " + n_s1 + " scenes");
} else {
  print("  [WARN] No S1 data for " + EXPORT_YEAR);
}

// ============================================================
// 🏔️ TERRAIN
// ============================================================
print("\n🏔️ Preparing terrain...");
var dem = ee.ImageCollection("USGS/3DEP/10m_collection").mosaic();
var terrain = dem.rename('elevation_10m')
  .addBands(ee.Terrain.slope(dem).rename('slope_10m'))
  .addBands(ee.Terrain.aspect(dem).rename('aspect_10m'))
  .clip(roi)
  .updateMask(mask_raster.select(0));

// ============================================================
// 🌲 GEDI
// ============================================================
print("\n🌲 Preparing GEDI...");
var gedi = ee.ImageCollection('LARSE/GEDI/GEDI02_A_002_MONTHLY')
  .filterBounds(roi)
  .filterDate('2019-01-01', '2024-12-31')
  .map(function(img) {
    return img.updateMask(
      img.select('quality_flag').eq(1)
        .and(img.select('degrade_flag').eq(0))
    );
  })
  .select(['rh50', 'rh75', 'rh98', 'landsat_treecover'])
  .mean()
  .clip(roi)
  .updateMask(mask_raster.select(0))
  .rename(['GEDI_rh50', 'GEDI_rh75', 'GEDI_rh98', 'GEDI_cover']);

// ============================================================
// 🌳 NLCD
// ============================================================
print("\n🌳 Preparing NLCD...");
var nlcd = ee.ImageCollection('USGS/NLCD_RELEASES/2023_REL/TCC/v2023-5')
  .first()
  .select('NLCD_Percent_Tree_Canopy_Cover')
  .rename('NLCD_canopy_pct')
  .clip(roi)
  .updateMask(mask_raster.select(0));

// ============================================================
// 🔄 BUILD STACKED RASTER (all pixels, no sampling!)
// ============================================================
print("\n🔄 Stacking selected features (ALL PIXELS)...");

var feature_stack = null;

// Helper function to check if array contains string
function arrayContains(arr, str) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] === str) return true;
  }
  return false;
}

// Helper function to check if string contains substring
function stringContains(str, substr) {
  return str.indexOf(substr) !== -1;
}

// Add S2 indices
Object.keys(s2_composites).forEach(function(month) {
  var composite = s2_composites[month];
  if (composite !== null) {
    for (var i = 0; i < SELECTED_VARS.length; i++) {
      var var_name = SELECTED_VARS[i];
      if (stringContains(var_name, month)) {
        var band_name = var_name.replace(month + "_", "");
        try {
          var band = composite.select(band_name);
          if (feature_stack === null) {
            feature_stack = band;
          } else {
            feature_stack = feature_stack.addBands(band);
          }
        } catch(e) {
          // Band not found, skip
        }
      }
    }
  }
});

// Add S1 if needed
if (s1_composite !== null) {
  for (var i = 0; i < SELECTED_VARS.length; i++) {
    var var_name = SELECTED_VARS[i];
    if (stringContains(var_name, 'VV') || stringContains(var_name, 'VH')) {
      try {
        var band = s1_composite.select(var_name);
        if (feature_stack === null) {
          feature_stack = band;
        } else {
          feature_stack = feature_stack.addBands(band);
        }
      } catch(e) {
        // Skip
      }
    }
  }
}

// Add terrain
var terrain_vars = ['elevation_10m', 'slope_10m', 'aspect_10m'];
for (var i = 0; i < terrain_vars.length; i++) {
  var var_name = terrain_vars[i];
  if (arrayContains(SELECTED_VARS, var_name)) {
    var band = terrain.select(var_name);
    if (feature_stack === null) {
      feature_stack = band;
    } else {
      feature_stack = feature_stack.addBands(band);
    }
  }
}

// Add GEDI
var gedi_vars = ['GEDI_rh50', 'GEDI_rh75', 'GEDI_rh98', 'GEDI_cover'];
for (var i = 0; i < gedi_vars.length; i++) {
  var var_name = gedi_vars[i];
  if (arrayContains(SELECTED_VARS, var_name)) {
    var band = gedi.select(var_name);
    if (feature_stack === null) {
      feature_stack = band;
    } else {
      feature_stack = feature_stack.addBands(band);
    }
  }
}

// Add NLCD
if (arrayContains(SELECTED_VARS, 'NLCD_canopy_pct')) {
  var band = nlcd.select('NLCD_canopy_pct');
  if (feature_stack === null) {
    feature_stack = band;
  } else {
    feature_stack = feature_stack.addBands(band);
  }
}

print("✓ Feature stack ready: " + SELECTED_VARS.length + " bands");
print("  Band names: " + feature_stack.bandNames().getInfo());

// ============================================================
// 🚀 EXPORT RASTER TO GOOGLE DRIVE
// ============================================================
var timestamp = ee.Date(ee.Date.now()).format('YYYYMMdd_HHmmss').getInfo();
var export_filename = "forest_features_raster_" + EXPORT_YEAR + "_" + timestamp;

print("\n🚀 Exporting raster to Google Drive...");
print("   Filename: " + export_filename);
print("   Size: Full AOI (all pixels, ~10m resolution)");
print("   Expected size: ~50-500 MB depending on AOI size");

Export.image.toDrive({
  image: feature_stack,
  description: export_filename,
  folder: 'GEE_exports',
  fileNamePrefix: export_filename,
  scale: 10,  // 10m resolution
  crs: 'EPSG:4326',
  maxPixels: 1e13,  // Allow large exports
  region: roi,
  fileFormat: 'GeoTIFF'
});

print("\n✓ Export task created!");
print("   Monitor at: https://code.earthengine.google.com/tasks");
print("   Once complete, download GeoTIFF and use in R: 02_r_predict_on_raster.R");