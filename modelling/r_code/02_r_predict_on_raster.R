# =========================================================
# 🎯 R RASTER PREDICTION PIPELINE
# =========================================================
# Load GEE raster → apply saved model → output prediction raster
#
# Usage:
#   1. Export raster from GEE using: gee_export_raster_for_prediction.js
#   2. Download GeoTIFF from Google Drive
#   3. Update paths below
#   4. Run this script
#   5. Output: prediction_raster.tif (LAI predictions for all pixels)

library(raster)
library(terra)
library(dplyr)

# =========================================================
# ⚙️ CONFIGURATION
# =========================================================

# Path to GEE-downloaded GeoTIFF raster
GEE_RASTER_PATH <- "path/to/your/forest_features_raster_2024.tif"  # ← UPDATE THIS

# Paths to saved model + metadata (from 01_r_train_and_save_model.R)
MODEL_DIR <- "results/lai_reg_old2/saved_models_r"
MODEL_PATH <- file.path(MODEL_DIR, "best_model_CH_RF.rds")  # Update if different model
METADATA_PATH <- file.path(MODEL_DIR, "metadata_CH.rds")

# Output path for prediction raster
OUTPUT_RASTER_PATH <- "predictions_raster.tif"

# =========================================================
# 📂 CHECK FILES EXIST
# =========================================================
if (!file.exists(GEE_RASTER_PATH)) {
  stop("GEE raster not found: ", GEE_RASTER_PATH,
       "\nPlease download from Google Drive (GEE_exports folder)")
}

if (!file.exists(MODEL_PATH)) {
  stop("Model not found: ", MODEL_PATH,
       "\nRun: 01_r_train_and_save_model.R first")
}

if (!file.exists(METADATA_PATH)) {
  stop("Metadata not found: ", METADATA_PATH,
       "\nRun: 01_r_train_and_save_model.R first")
}

message("✓ All input files found")

# =========================================================
# 📊 LOAD RASTER
# =========================================================
message("\n📊 Loading GEE raster...")
raster_obj <- terra::rast(GEE_RASTER_PATH)
message("✓ Raster loaded: ", GEE_RASTER_PATH)
print(raster_obj)

# Get properties
n_bands <- nlyr(raster_obj)
n_pixels <- ncell(raster_obj)
message(sprintf("  Dimensions: %d x %d pixels", nrow(raster_obj), ncol(raster_obj)))
message(sprintf("  Bands: %d", n_bands))
message(sprintf("  Total cells: %d", n_pixels))

# Band names
band_names <- names(raster_obj)
message("  Bands: ", paste(band_names, collapse = ", "))

# =========================================================
# 💾 LOAD SAVED MODEL + METADATA
# =========================================================
message("\n💾 Loading saved model and metadata...")
model <- readRDS(MODEL_PATH)
metadata <- readRDS(METADATA_PATH)

best_model_name <- metadata$best_model_name
selected_vars <- metadata$selected_vars
min_vals <- metadata$min_vals
max_vals <- metadata$max_vals
range_vals <- metadata$range_vals

message("✓ Model loaded: ", best_model_name)
message("  Selected features: ", length(selected_vars))
message("  Features: ", paste(selected_vars, collapse = ", "))

# =========================================================
# 🔄 CONVERT RASTER TO DATA MATRIX
# =========================================================
message("\n🔄 Converting raster to matrix...")

# Convert to matrix (each row = pixel, each column = band)
data_matrix <- terra::values(raster_obj, na.rm = FALSE)
message(sprintf("  Matrix dimensions: %d rows (pixels) × %d columns (bands)",
                nrow(data_matrix), ncol(data_matrix)))

# Create dataframe with band names
data_df <- as.data.frame(data_matrix)
colnames(data_df) <- band_names

# =========================================================
# 🧹 PREPROCESS DATA (matching training preprocessing)
# =========================================================
message("\n🧹 Preprocessing...")

# Check that all selected features are in the raster
missing_vars <- setdiff(selected_vars, colnames(data_df))
if (length(missing_vars) > 0) {
  stop("Missing variables in raster: ", paste(missing_vars, collapse = ", "))
}

# Extract only selected features
X_data <- data_df[, selected_vars]

# Handle missing values (fill with column mean)
n_missing_before <- sum(is.na(X_data))
if (n_missing_before > 0) {
  message(sprintf("  Found %d NaN values, filling with column means...", n_missing_before))
  for (col in colnames(X_data)) {
    X_data[[col]][is.na(X_data[[col]])] <- mean(X_data[[col]], na.rm = TRUE)
  }
}

# Apply min-max scaling (same as training)
message("  Applying min-max scaling...")
X_scaled <- X_data

for (var in selected_vars) {
  if (var %in% names(min_vals) && var %in% names(range_vals)) {
    X_scaled[[var]] <- (X_data[[var]] - min_vals[var]) / range_vals[var]
  } else {
    warning("Scaling parameters missing for: ", var)
  }
}

message("✓ Data preprocessed")

# =========================================================
# 🎯 MAKE PREDICTIONS
# =========================================================
message("\n🎯 Making predictions on all pixels...")
message(sprintf("  Predicting for %d pixels...", nrow(X_scaled)))

predictions <- tryCatch({
  predict(model, X_scaled)
}, error = function(e) {
  stop("Prediction failed: ", e$message)
})

message(sprintf("✓ Predictions complete: %d values", length(predictions)))

# Summary statistics
message("\n📈 Prediction Summary:")
message(sprintf("  Mean:     %.4f", mean(predictions, na.rm = TRUE)))
message(sprintf("  Median:   %.4f", median(predictions, na.rm = TRUE)))
message(sprintf("  Std Dev:  %.4f", sd(predictions, na.rm = TRUE)))
message(sprintf("  Min:      %.4f", min(predictions, na.rm = TRUE)))
message(sprintf("  Max:      %.4f", max(predictions, na.rm = TRUE)))
message(sprintf("  NaN:      %d", sum(is.nan(predictions))))

# =========================================================
# 🗺️ CREATE PREDICTION RASTER
# =========================================================
message("\n🗺️ Creating prediction raster...")

# Create raster from predictions
pred_raster <- raster_obj[[1]]  # Use first band as template
terra::values(pred_raster) <- predictions

# Set CRS and names
terra::crs(pred_raster) <- terra::crs(raster_obj)
names(pred_raster) <- paste0("LAI_", best_model_name)

message("✓ Prediction raster created")
print(pred_raster)

# =========================================================
# 💾 SAVE PREDICTION RASTER
# =========================================================
message("\n💾 Saving prediction raster...")
terra::writeRaster(pred_raster,
                   filename = OUTPUT_RASTER_PATH,
                   overwrite = TRUE)

message("✓ Saved: ", OUTPUT_RASTER_PATH)

# =========================================================
# 📊 EXPORT SUMMARY TO CSV
# =========================================================
# Also export predictions to CSV for reference
summary_csv <- data.frame(
  Statistic = c("Mean", "Median", "Std Dev", "Min", "Max", "NaN Count", "Model", "N_Pixels"),
  Value = c(
    mean(predictions, na.rm = TRUE),
    median(predictions, na.rm = TRUE),
    sd(predictions, na.rm = TRUE),
    min(predictions, na.rm = TRUE),
    max(predictions, na.rm = TRUE),
    sum(is.nan(predictions)),
    best_model_name,
    length(predictions)
  )
)

summary_path <- gsub("\\.tif$", "_summary.csv", OUTPUT_RASTER_PATH)
write.csv(summary_csv, summary_path, row.names = FALSE)
message("✓ Summary saved: ", summary_path)

# =========================================================
# ✅ COMPLETE
# =========================================================
message("\n", strrep("=", 50))
message("✅ PREDICTION COMPLETE!")
message(strrep("=", 50))
message("\nOutput files:")
message("  - Raster:  ", OUTPUT_RASTER_PATH)
message("  - Summary: ", summary_path)
message("\nNext steps:")
message("  1. Open prediction raster in QGIS/ArcGIS")
message("  2. Visualize LAI predictions")
message("  3. Create maps/reports")
message("\nNote: One NaN pixel means no prediction (usually masked/nodata)")
