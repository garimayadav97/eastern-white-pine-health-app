# =========================================================
# 🎯 R ML CLASSIFICATION PIPELINE — HEALTH CLASS PREDICTION
# =========================================================
# Train models to classify forest health (Healthy / Mod Healthy / Unhealthy)
# from remote sensing predictors.
# Output: Best model RDS + scaling parameters + selected features
#
# Classification equivalents of the regression pipeline:
#   AVP plot         → Confusion matrix heatmap (per model)
#   R² / RMSE        → Accuracy / Kappa / per-class F1
#   reg:squarederror → multi:softmax (XGB), MultiClass (CatBoost)
#   Boruta + corr    → unchanged (works for both regression + classification)
# =========================================================

library(readxl)
library(Boruta)
library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
library(scales)
library(xgboost)
library(catboost)
library(showtext)
library(sysfonts)
library(openxlsx)

# ── Font ──────────────────────────────────────────────────────
tnr_loaded <- tryCatch({
  font_add("TNR",
           regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf",
           bold    = "/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf")
  TRUE
}, error = function(e) {
  message("Times New Roman not found — falling back to 'serif'")
  FALSE
})
font_family <- if (tnr_loaded) "TNR" else "serif"

# =========================================================
# 🎨 Colour Palette
# =========================================================
pal <- list(
  point    = "#2C3E50",
  bar_fill = "#2C3E50",
  bar_best = "#E74C3C",
  text     = "#1A1A2E",
  ref_line = "#95A5A6"
)

HC_LEVELS  <- c("Healthy", "Mod_Healthy", "Unhealthy")   # underscore = valid R name
HC_LABELS  <- c("Healthy", "Mod Healthy", "Unhealthy")   # display labels (with space)
HC_FILL    <- c("Healthy"     = "#2D6A4F",
                "Mod_Healthy" = "#F39C12",
                "Unhealthy"   = "#C0392B")

# =========================================================
# 📊 PLOT FUNCTIONS
# =========================================================

# ---------------------------------------------------------
# Confusion Matrix Heatmap (replaces AVP plot)
# ---------------------------------------------------------
make_confusion_plot <- function(actual, predicted, model_name,
                                output_path, metrics = NULL) {
  
  cm_df <- as.data.frame(table(
    Predicted = factor(predicted, levels = HC_LEVELS),
    Actual    = factor(actual,    levels = HC_LEVELS)
  ))
  levels(cm_df$Predicted) <- HC_LABELS
  levels(cm_df$Actual)    <- HC_LABELS
  
  cm_df <- cm_df %>%
    group_by(Actual) %>%
    mutate(pct = ifelse(sum(Freq) > 0, Freq / sum(Freq) * 100, 0)) %>%
    ungroup()
  
  # Build annotation — include CIs if present
  ann_text <- if (!is.null(metrics)) {
    acc_ci  <- if (!is.null(metrics$Acc_CI_lo))
      sprintf(" [%.2f\u2013%.2f]", metrics$Acc_CI_lo, metrics$Acc_CI_hi) else ""
    kap_ci  <- if (!is.null(metrics$Kappa_CI_lo))
      sprintf(" [%.2f\u2013%.2f]", metrics$Kappa_CI_lo, metrics$Kappa_CI_hi) else ""
    paste0(
      "Accuracy = ", sprintf("%.2f", metrics$Accuracy), acc_ci, "\n",
      "Kappa    = ", sprintf("%.2f", metrics$Kappa),    kap_ci, "\n",
      "F1 macro = ", sprintf("%.2f", metrics$F1_macro)
    )
  } else ""
  
  p <- ggplot(cm_df, aes(Actual, Predicted, fill = pct)) +
    geom_tile(colour = "white", linewidth = 1.2) +
    geom_text(aes(label = sprintf("%d\n(%.0f%%)", Freq, pct)),
              size = 4.5, fontface = "bold", family = font_family,
              colour = ifelse(cm_df$pct > 45, "white", pal$text)) +
    scale_fill_gradient2(
      low = "white", mid = "#AED6F1", high = "#1A5276",
      midpoint = 50, limits = c(0, 100), name = "Recall %"
    ) +
    scale_x_discrete(position = "top") +
    annotate("text",
             x = -Inf, y = -Inf, label = ann_text,
             hjust = -0.08, vjust = -0.5,
             size = 3.8, fontface = "bold", family = font_family,
             colour = pal$text, lineheight = 1.4) +
    annotate("text",
             x = Inf, y = Inf, label = model_name,
             hjust = 1.1, vjust = 1.6,
             size = 5, fontface = "bold.italic",
             family = font_family, colour = pal$text) +
    labs(x = "Actual class", y = "Predicted class") +
    theme_classic(base_family = font_family, base_size = 14) +
    theme(
      text             = element_text(color = pal$text),
      axis.text        = element_text(size = 12, color = pal$text, face = "bold"),
      axis.title       = element_text(size = 13, color = pal$text, face = "bold"),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.7),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(12, 16, 12, 8)
    )
  
  jpeg(output_path, width = 5.5, height = 5, units = "in", res = 600)
  showtext_begin(); print(p); showtext_end()
  dev.off()
}


# ---------------------------------------------------------
# Variable Importance — same as regression version
# ---------------------------------------------------------
make_varimp_plot <- function(model_name, model_obj, selected_vars,
                              output_path) {

  if (model_name == "SVM") return(invisible(NULL))

  df_imp <- switch(model_name,
    RF  = {
      imp <- varImp(model_obj, scale = FALSE)$importance
      # For multi-class RF caret returns mean decrease gini per class — take mean
      data.frame(Predictor  = rownames(imp),
                 Importance = rowMeans(imp, na.rm = TRUE))
    },
    GBM = {
      imp <- summary(model_obj$finalModel, plotit = FALSE)
      data.frame(Predictor = imp$var, Importance = imp$rel.inf)
    },
    XGB = {
      imp <- xgb.importance(model = model_obj)
      data.frame(Predictor = imp$Feature, Importance = imp$Gain)
    },
    CAT = {
      imp <- catboost.get_feature_importance(model_obj)
      data.frame(Predictor = selected_vars, Importance = as.numeric(imp))
    }
  )

  df_imp <- df_imp %>%
    mutate(Importance = Importance / max(Importance, na.rm = TRUE)) %>%
    arrange(desc(Importance)) %>%
    slice_head(n = 20) %>%
    mutate(
      Predictor = factor(Predictor, levels = rev(Predictor)),
      Tier = case_when(Importance >= 0.66 ~ "High",
                       Importance >= 0.33 ~ "Mid",
                       TRUE               ~ "Low")
    )

  tier_cols <- c(High = pal$point, Mid = "#5D6D7E", Low = "#AEB6BF")

  p <- ggplot(df_imp, aes(y = Predictor, x = Importance, fill = Tier)) +
    geom_col(width = 0.65, color = NA) +
    geom_text(aes(label = sprintf("%.2f", Importance)),
              hjust = -0.1, size = 3.8,
              family = font_family, color = pal$text, fontface = "bold") +
    scale_fill_manual(values = tier_cols, guide = "none") +
    scale_x_continuous(limits = c(0, 1.18), expand = c(0, 0),
                       breaks = seq(0, 1, 0.25)) +
    labs(x = "Relative Importance", y = NULL, title = model_name) +
    theme_classic(base_family = font_family, base_size = 14) +
    theme(
      plot.title       = element_text(size = 15, face = "bold",
                                      hjust = 0.5, color = pal$text),
      axis.text.y      = element_text(size = 12, color = pal$text, face = "bold"),
      axis.text.x      = element_text(size = 12, color = pal$text),
      axis.title.x     = element_text(size = 13, color = pal$text, face = "bold"),
      axis.line        = element_line(color = pal$text, linewidth = 0.6),
      axis.ticks       = element_line(color = pal$text, linewidth = 0.5),
      panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.6),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(10, 20, 8, 8)
    )

  jpeg(output_path, width = 7, height = 6, units = "in", res = 600)
  showtext_begin(); print(p); showtext_end()
  dev.off()
}

# ---------------------------------------------------------
# Model Comparison Bar Chart (Accuracy / Kappa / F1_macro)
# ---------------------------------------------------------
plot_model_comparison <- function(metric_df, metric_name,
                                  output_path, best_model = NULL) {
  
  higher_better <- metric_name %in% c("Accuracy", "Kappa", "F1_macro",
                                      "F1_Healthy", "F1_ModHealthy", "F1_Unhealthy")
  arrow_label   <- if (higher_better) "\u2191 higher is better" else "\u2190 lower is better"
  has_ci        <- all(c("CI_lo", "CI_hi") %in% names(metric_df))
  
  metric_df <- metric_df %>%
    arrange(if (higher_better) desc(Value) else Value) %>%
    mutate(Model  = factor(Model, levels = Model),
           IsBest = !is.null(best_model) & Model == best_model)
  
  y_ceiling <- if (has_ci) max(metric_df$CI_hi, na.rm = TRUE) * 1.20
  else        max(metric_df$Value,  na.rm = TRUE) * 1.25
  y_ceiling <- min(y_ceiling, 1.15)
  
  p <- ggplot(metric_df, aes(Model, Value, fill = IsBest)) +
    geom_col(width = 0.6, color = NA) +
    
    { if (has_ci)
      geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                    width = 0.22, color = "#E74C3C", linewidth = 1.0)
    } +
    
    geom_text(aes(label = sprintf("%.2f", Value)),
              vjust = -0.5, size = 4.2,
              family = font_family, fontface = "bold", color = pal$text) +
    scale_fill_manual(values = c("FALSE" = pal$bar_fill, "TRUE" = pal$bar_best),
                      guide = "none") +
    scale_y_continuous(limits = c(0, y_ceiling),
                       expand = expansion(mult = c(0, 0))) +
    annotate("text", x = Inf, y = y_ceiling * 0.97,
             label = arrow_label, hjust = 1.05, vjust = 1,
             size = 3.8, color = "#7F8C8D",
             family = font_family, fontface = "italic") +
    labs(title = metric_name, x = NULL, y = metric_name) +
    theme_classic(base_family = font_family, base_size = 14) +
    theme(
      plot.title       = element_text(size = 15, face = "bold",
                                      hjust = 0.5, color = pal$text),
      axis.text.x      = element_text(size = 13, color = pal$text, face = "bold"),
      axis.text.y      = element_text(size = 12, color = pal$text),
      axis.title.y     = element_text(size = 13, face = "bold", color = pal$text),
      axis.line        = element_line(color = pal$text, linewidth = 0.6),
      axis.ticks       = element_line(color = pal$text, linewidth = 0.5),
      panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.6),
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(10, 16, 8, 8)
    )
  
  jpeg(output_path, width = 7, height = 5, units = "in", res = 600)
  showtext_begin(); print(p); showtext_end()
  dev.off()
}

# =========================================================
# 📊 Classification Metrics Helper
# =========================================================
compute_class_metrics <- function(pred, actual, n_boot = 1000) {
  
  pred_f   <- factor(pred,   levels = HC_LEVELS)
  actual_f <- factor(actual, levels = HC_LEVELS)
  
  # ── Point estimates ─────────────────────────────────────────────
  cm           <- confusionMatrix(pred_f, actual_f)          # was missing — BUG FIX
  f1_per_class <- cm$byClass[, "F1"]
  names(f1_per_class) <- sub("Class: ", "", names(f1_per_class))
  
  acc   <- cm$overall["Accuracy"]
  kappa <- cm$overall["Kappa"]
  f1_h  <- f1_per_class["Healthy"]
  f1_m  <- f1_per_class["Mod_Healthy"]
  f1_u  <- f1_per_class["Unhealthy"]
  f1_macro <- mean(c(f1_h, f1_m, f1_u), na.rm = TRUE)
  
  # ── Bootstrap CIs ───────────────────────────────────────────────
  n     <- length(actual)
  alpha <- 0.025
  acc_b <- kappa_b <- f1h_b <- f1m_b <- f1u_b <- numeric(n_boot)
  
  set.seed(42)
  for (i in seq_len(n_boot)) {
    idx_b  <- sample(n, n, replace = TRUE)
    cm_b   <- tryCatch(
      confusionMatrix(factor(pred_f[idx_b],   levels = HC_LEVELS),
                      factor(actual_f[idx_b], levels = HC_LEVELS)),
      error = function(e) NULL
    )
    if (is.null(cm_b)) { acc_b[i] <- NA; next }
    
    acc_b[i]   <- cm_b$overall["Accuracy"]
    kappa_b[i] <- cm_b$overall["Kappa"]
    f1b        <- cm_b$byClass[, "F1"]
    names(f1b) <- sub("Class: ", "", names(f1b))
    f1h_b[i]   <- f1b["Healthy"]
    f1m_b[i]   <- f1b["Mod_Healthy"]
    f1u_b[i]   <- f1b["Unhealthy"]
  }
  
  ci2 <- function(x) round(quantile(x, c(alpha, 1 - alpha),
                                    na.rm = TRUE, names = FALSE), 2)
  
  data.frame(
    Accuracy         = round(acc,      2),
    Acc_CI_lo        = ci2(acc_b)[1],
    Acc_CI_hi        = ci2(acc_b)[2],
    Kappa            = round(kappa,    2),
    Kappa_CI_lo      = ci2(kappa_b)[1],
    Kappa_CI_hi      = ci2(kappa_b)[2],
    F1_macro         = round(f1_macro, 2),
    F1_Healthy       = round(f1_h,     2),
    F1_Healthy_CI_lo = ci2(f1h_b)[1],
    F1_Healthy_CI_hi = ci2(f1h_b)[2],
    F1_ModHealthy    = round(f1_m,     2),
    F1_Mod_CI_lo     = ci2(f1m_b)[1],
    F1_Mod_CI_hi     = ci2(f1m_b)[2],
    F1_Unhealthy     = round(f1_u,     2),
    F1_Un_CI_lo      = ci2(f1u_b)[1],
    F1_Un_CI_hi      = ci2(f1u_b)[2],
    row.names = NULL
  )
}


# =========================================================
# ⭐ Correlation Prune → Boruta  (unchanged from regression)
# =========================================================
corr_before_boruta <- function(df_in, target, corr_cut = 0.75) {
  predictors  <- df_in %>% select(-all_of(target))
  all_vars    <- colnames(predictors)
  cor_mat     <- abs(cor(predictors, use = "pairwise"))
  diag(cor_mat) <- 0
  remove_list <- c()
  high_pairs  <- which(cor_mat >= corr_cut, arr.ind = TRUE)
  if (nrow(high_pairs) > 0) {
    for (i in seq_len(nrow(high_pairs))) {
      v1 <- rownames(cor_mat)[high_pairs[i, 1]]
      v2 <- colnames(cor_mat)[high_pairs[i, 2]]
      remove_list <- c(remove_list,
                       ifelse(var(predictors[[v1]], na.rm = TRUE) <
                                var(predictors[[v2]], na.rm = TRUE), v1, v2))
    }
  }
  pruned_vars <- setdiff(all_vars, unique(remove_list))
  message("Variables after correlation pruning: ", length(pruned_vars))

  # Boruta on the pruned set
  # Note: target must be a factor for classification mode
  bor <- Boruta(
    x       = predictors[, pruned_vars, drop = FALSE],
    y       = df_in[[target]],
    doTrace = 1
  )
  bor_stats  <- attStats(bor)
  final_vars <- rownames(bor_stats)[bor_stats$decision == "Confirmed"]
  if (length(final_vars) < 2)
    stop("Boruta confirmed only ", length(final_vars), " variable(s).")
  message("Variables confirmed by Boruta: ", length(final_vars))
  final_vars
}

# =========================================================
# 📂 Paths & Data
# =========================================================
setwd("/Users/garimayadav/Library/CloudStorage/OneDrive-UniversityofMaineSystem/research/chapter2_new/code/data/excels")

output_dir <- "results/health_class_clf"
plot_dir   <- file.path(output_dir, "plots")
model_dir  <- file.path(output_dir, "saved_models_r")
for (d in c(plot_dir, model_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

df <- read_excel("forest_indices_v5_all.xlsx",
                 sheet = "forest_indices_v5_all")

# =========================================================
# 🏷️ Target — Health Class
# =========================================================
# Auto-detect column; set HC_COL_NAME manually if needed.
HC_COL_NAME <- {
  candidates <- c("Health_Class", "HealthClass2", "Health Class",
                  "Class", "HC", "Health")
  found <- intersect(candidates, colnames(df))
  if (length(found) > 0) found[1] else
    stop("Health class column not found. Columns: ",
         paste(colnames(df), collapse = ", "))
}
message("Using health class column: '", HC_COL_NAME, "'")

df$HealthClass2 <- trimws(as.character(df[[HC_COL_NAME]]))
df$HealthClass2 <- dplyr::recode(df$HealthClass2,
  "Mod Healthy"   = "Mod_Healthy", "ModHealthy"   = "Mod_Healthy",
  "Moderate"      = "Mod_Healthy", "mod healthy"  = "Mod_Healthy",
  "mod_healthy"   = "Mod_Healthy",
  "unhealthy"     = "Unhealthy",   "healthy"      = "Healthy")
df$HealthClass2 <- factor(df$HealthClass2, levels = HC_LEVELS)

cat("\nHealth class distribution:\n")
print(table(df$HealthClass2, useNA = "ifany"))

# =========================================================
# 🔍 Predictor Variables (RS features only)
# =========================================================
# Exclude field-measured attributes, IDs, coordinates, and the target.
predictor_vars <- names(df)
predictor_vars <- predictor_vars[
  !grepl(paste0("LAI|ID|Slope|MEAN|CNN|Plot|LCR|CE|Sep|Nov|Oct|Apr",
                "|^B[0-9]|Height|Count|X$|Y$|LCR|CBH|slope|p_100",
                "|plog|ealt|mont|Stand|State|CH|Sites|Health|Class|Crown"),
         predictor_vars)
]
message("RS predictor candidates: ", length(predictor_vars))

# =========================================================
# 🔁 DATA PREPARATION
# =========================================================
TARGET <- "HealthClass2"
seed   <- 111
set.seed(seed)

df_sub <- df %>%
  select(all_of(c(TARGET, predictor_vars))) %>%
  mutate(across(-all_of(TARGET), as.numeric)) %>%
  filter(!is.na(HealthClass2)) %>%
  na.omit()

message("Rows after NA removal: ", nrow(df_sub))
if (nrow(df_sub) < 30)
  stop("Too few complete rows (", nrow(df_sub), ")")

# ── NZV removal ───────────────────────────────────────────
nzv_idx       <- nearZeroVar(df_sub[, predictor_vars])
pred_filtered <- if (length(nzv_idx) > 0)
  predictor_vars[-nzv_idx] else predictor_vars
message("Predictors after NZV removal: ", length(pred_filtered))

y <- df_sub[[TARGET]]   # factor

# ── Stratified train / test split ─────────────────────────
# createDataPartition respects class proportions for factors
idx     <- createDataPartition(y, p = 0.8, list = FALSE)
train_x_raw <- df_sub[idx,  pred_filtered]
val_x_raw   <- df_sub[-idx, pred_filtered]
train_y <- y[idx]
val_y   <- y[-idx]

cat("\nTrain class counts:\n"); print(table(train_y))
cat("\nTest  class counts:\n"); print(table(val_y))

# ── Min-max scaling (fit on train only) ───────────────────
min_vals   <- sapply(train_x_raw, min,  na.rm = TRUE)
max_vals   <- sapply(train_x_raw, max,  na.rm = TRUE)
range_vals <- pmax(max_vals - min_vals, .Machine$double.eps)

scale_minmax <- function(df_in)
  as.data.frame(sweep(sweep(as.matrix(df_in), 2, min_vals, "-"),
                      2, range_vals, "/"))

train_x <- scale_minmax(train_x_raw)
val_x   <- scale_minmax(val_x_raw)
all_x   <- scale_minmax(df_sub[, pred_filtered])

# ── Feature selection (Boruta + correlation prune) ────────
df_fs        <- data.frame(HealthClass2 = train_y, train_x)
selected_vars <- corr_before_boruta(df_fs, "HealthClass2", corr_cut = 0.75)
train_x
train_top <- train_x[, selected_vars, drop = FALSE]
val_top   <- val_x[,   selected_vars, drop = FALSE]
all_top   <- all_x[,   selected_vars, drop = FALSE]

# =========================================================
# 🚀 MODEL TRAINING
# =========================================================
model_preds   <- list()
model_metrics <- list()
model_objects <- list()

ctrl_cv10 <- trainControl(method = "cv", number = 10,
                           classProbs = TRUE,
                           summaryFunction = multiClassSummary)

# ── Random Forest ─────────────────────────────────────────
message("\nTraining RF...")
rf_model <- train(
  x         = train_top,
  y         = train_y,
  method    = "rf",
  trControl = ctrl_cv10,
  tuneGrid  = expand.grid(mtry = seq(2, min(5, length(selected_vars)), 1)),
  ntree     = 800,
  metric    = "Accuracy"
)
model_preds$RF   <- predict(rf_model, val_top)
model_objects$RF <- rf_model

# ── SVM ───────────────────────────────────────────────────
message("Training SVM...")
svm_model <- train(
  x          = train_top,
  y          = train_y,
  method     = "svmRadial",
  trControl  = ctrl_cv10,
  tuneLength = 5,
  metric     = "Accuracy"
)
model_preds$SVM   <- predict(svm_model, val_top)
model_objects$SVM <- svm_model

# ── GBM ───────────────────────────────────────────────────
message("Training GBM...")
gbm_model <- train(
  x          = train_top,
  y          = train_y,
  method     = "gbm",
  trControl  = ctrl_cv10,
  tuneLength = 5,
  verbose    = FALSE,
  metric     = "Accuracy"
)
model_preds$GBM   <- predict(gbm_model, val_top)
model_objects$GBM <- gbm_model

# ── XGBoost (multi-class) ─────────────────────────────────
message("Training XGB...")
# XGBoost needs 0-indexed integer labels
label_map   <- setNames(0:(length(HC_LEVELS) - 1), HC_LEVELS)
train_y_xgb <- as.integer(label_map[as.character(train_y)])
val_y_xgb   <- as.integer(label_map[as.character(val_y)])

dtrain <- xgb.DMatrix(as.matrix(train_top), label = train_y_xgb)
dvalid <- xgb.DMatrix(as.matrix(val_top),   label = val_y_xgb)

xgb_model <- xgb.train(
  params = list(
    objective        = "multi:softmax",
    num_class        = length(HC_LEVELS),
    eta              = 0.1,
    max_depth        = 4,
    subsample        = 0.9,
    colsample_bytree = 0.9,
    eval_metric      = "merror"        # multi-class error rate
  ),
  data                  = dtrain,
  nrounds               = 300,
  early_stopping_rounds = 20,
  watchlist             = list(train = dtrain, val = dvalid),
  verbose               = 0
)
# Convert 0-indexed predictions back to factor levels
xgb_pred_idx          <- predict(xgb_model, as.matrix(val_top))
model_preds$XGB       <- HC_LEVELS[xgb_pred_idx + 1]
model_objects$XGB     <- xgb_model

# ── CatBoost (multi-class) ────────────────────────────────
message("Training CatBoost...")
pool_train <- catboost.load_pool(train_top, label = as.integer(train_y) - 1L)
pool_val   <- catboost.load_pool(val_top)

cat_model <- catboost.train(
  learn_pool = pool_train,
  test_pool  = pool_val,
  params     = list(
    loss_function  = "MultiClass",
    classes_count  = length(HC_LEVELS),
    iterations     = 400,
    learning_rate  = 0.05,
    depth          = 6,
    task_type      = "CPU",
    logging_level  = "Silent"
  )
)
cat_pred_idx      <- catboost.predict(cat_model, pool_val,
                                       prediction_type = "Class")
model_preds$CAT   <- HC_LEVELS[as.integer(cat_pred_idx) + 1L]
model_objects$CAT <- cat_model

# =========================================================
# 📊 METRICS
# =========================================================
for (model in names(model_preds)) {
  model_metrics[[model]] <- compute_class_metrics(model_preds[[model]], val_y)
}

# Best model by Accuracy
acc_values      <- sapply(model_metrics, function(x) x$Accuracy)
best_model_name <- names(which.max(acc_values))
best_model_obj  <- model_objects[[best_model_name]]

message("\n========================================")
message("Best model: ", best_model_name,
        " (Accuracy = ", round(max(acc_values), 3), ")")
message("========================================")

# Print all metrics
message("\nValidation Metrics:")
for (m in names(model_metrics)) {
  mt <- model_metrics[[m]]
  message(sprintf(
    "%-5s | Acc=%.3f  Kappa=%.3f  F1_macro=%.3f  F1_H=%.3f  F1_Mod=%.3f  F1_Un=%.3f",
    m, mt$Accuracy, mt$Kappa, mt$F1_macro,
    mt$F1_Healthy, mt$F1_ModHealthy, mt$F1_Unhealthy
  ))
}

# =========================================================
# 💾 SAVE BEST MODEL + METADATA
# =========================================================
message("\n💾 Saving best model and metadata...")

model_path <- file.path(model_dir,
  paste0("best_model_HealthClass2_", best_model_name, ".rds"))
saveRDS(best_model_obj, model_path)
message("✓ Model saved: ", model_path)

metadata <- list(
  best_model_name = best_model_name,
  selected_vars   = selected_vars,
  min_vals        = min_vals,
  max_vals        = max_vals,
  range_vals      = range_vals,
  pred_filtered   = pred_filtered,
  target          = "HealthClass2",
  class_levels    = HC_LEVELS,
  label_map       = label_map,       # for XGBoost decoding
  metrics         = model_metrics
)
metadata_path <- file.path(model_dir, "metadata_HealthClass2.rds")
saveRDS(metadata, metadata_path)
message("✓ Metadata saved: ", metadata_path)

write.csv(
  data.frame(key   = c("best_model", "n_selected_vars", "n_predictors",
                        "accuracy", "kappa"),
             value = c(best_model_name, length(selected_vars),
                       length(pred_filtered),
                       round(max(acc_values), 3),
                       round(model_metrics[[best_model_name]]$Kappa, 3))),
  file.path(model_dir, "summary.csv"), row.names = FALSE
)

# =========================================================
# 📈 PLOTS
# =========================================================

# ── Confusion matrix per model ────────────────────────────
for (model_name in names(model_preds)) {
  make_confusion_plot(
    actual      = val_y,
    predicted   = model_preds[[model_name]],
    model_name  = model_name,
    metrics     = model_metrics[[model_name]],
    output_path = file.path(plot_dir,
      paste0("ConfMatrix_HealthClass2_", model_name, ".jpeg"))
  )
}

# ── Variable importance per model ─────────────────────────
make_varimp_plot("RF",  rf_model,  selected_vars,
  file.path(plot_dir, "VarImp_HealthClass2_RF.jpeg"))
make_varimp_plot("GBM", gbm_model, selected_vars,
  file.path(plot_dir, "VarImp_HealthClass2_GBM.jpeg"))
make_varimp_plot("XGB", xgb_model, selected_vars,
  file.path(plot_dir, "VarImp_HealthClass2_XGB.jpeg"))
make_varimp_plot("CAT", cat_model, selected_vars,
  file.path(plot_dir, "VarImp_HealthClass2_CAT.jpeg"))

# ── Model comparison: Accuracy, Kappa, F1_macro ──────────
for (met in c("Accuracy", "Kappa", "F1_macro")) {
  vals <- sapply(model_metrics, function(x) x[[met]])
  df_c <- data.frame(Model = names(vals), Value = as.numeric(vals),
                     stringsAsFactors = FALSE)
  
  # Attach CI columns for Accuracy and Kappa only
  if (met == "Accuracy") {
    df_c$CI_lo <- sapply(model_metrics, function(x) x$Acc_CI_lo)
    df_c$CI_hi <- sapply(model_metrics, function(x) x$Acc_CI_hi)
  } else if (met == "Kappa") {
    df_c$CI_lo <- sapply(model_metrics, function(x) x$Kappa_CI_lo)
    df_c$CI_hi <- sapply(model_metrics, function(x) x$Kappa_CI_hi)
  }
  # F1_macro: no CI columns → error bars skipped automatically
  
  df_c <- df_c[is.finite(df_c$Value), ]
  plot_model_comparison(
    metric_df   = df_c,
    metric_name = met,
    best_model  = best_model_name,
    output_path = file.path(plot_dir,
                            paste0("ModelComparison_HealthClass2_", met, ".jpeg"))
  )
}

# ── Per-class F1 comparison (faceted) ────────────────────
f1_long <- lapply(names(model_metrics), function(m) {
  mt <- model_metrics[[m]]
  data.frame(
    Model = m,
    Class = HC_LEVELS,    # "Healthy", "Mod_Healthy", "Unhealthy"
    F1    = c(mt$F1_Healthy, mt$F1_ModHealthy, mt$F1_Unhealthy)
  )
})
f1_df <- do.call(rbind, f1_long)
f1_df$Class <- factor(f1_df$Class, levels = HC_LEVELS)
f1_df$IsBest <- f1_df$Model == best_model_name

p_f1 <- ggplot(f1_df, aes(Model, F1, fill = Class)) +
  geom_col(position = position_dodge(0.72), width = 0.62,
           colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", F1)),
            position = position_dodge(0.72),
            vjust = -0.4, size = 3.6, fontface = "bold",
            family = font_family, color = pal$text) +
  scale_fill_manual(values = HC_FILL, labels = HC_LABELS, name = "Class") +
  scale_y_continuous(limits = c(0, 1.15),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "F1 Score",
       title = "Per-class F1 by model",
       subtitle = "Higher is better") +
  theme_classic(base_family = font_family, base_size = 14) +
  theme(
    text             = element_text(color = pal$text),
    axis.text        = element_text(size = 12, color = pal$text, face = "bold"),
    axis.title.y     = element_text(size = 13, color = pal$text, face = "bold"),
    axis.line        = element_line(color = pal$text, linewidth = 0.6),
    axis.ticks       = element_line(color = pal$text, linewidth = 0.5),
    panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.6),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "right",
    plot.margin      = margin(10, 16, 8, 8)
  )
jpeg(file.path(plot_dir, "PerClass_F1_Comparison.jpeg"),
     width = 8, height = 5, units = "in", res = 600)
showtext_begin(); print(p_f1); showtext_end()
dev.off()

# =========================================================
# 💾 SAVE METRICS + PREDICTIONS TO EXCEL
# =========================================================
# Full dataset predictions (all models)
all_pool_cat <- catboost.load_pool(all_top[, selected_vars])
full_preds <- data.frame(
  Actual = y,
  Split  = ifelse(seq_along(y) %in% idx, "Train", "Test"),
  RF     = predict(rf_model,  all_top[, selected_vars]),
  SVM    = predict(svm_model, all_top[, selected_vars]),
  GBM    = predict(gbm_model, all_top[, selected_vars]),
  XGB    = HC_LEVELS[predict(xgb_model,
                              as.matrix(all_top[, selected_vars])) + 1L],
  CAT    = HC_LEVELS[as.integer(catboost.predict(
                      cat_model, all_pool_cat,
                      prediction_type = "Class")) + 1L]
)

wb <- createWorkbook()
for (model in names(model_metrics)) {
  addWorksheet(wb, model)
  writeData(wb, model, model_metrics[[model]])
}
addWorksheet(wb, "Full_Predictions")
writeData(wb, "Full_Predictions", full_preds)

out_xlsx <- file.path(output_dir, "Results_HealthClass2.xlsx")
saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("✓ Results saved: ", out_xlsx)

# =========================================================
# 📋 SUMMARY
# =========================================================
message("\n✅ PIPELINE COMPLETE")
message("Output directory: ", output_dir)
message("\nSelected RS features (", length(selected_vars), "):")
message(paste0("  ['", paste(selected_vars, collapse = "', '"), "']"))
message("\nNext steps:")
message("  1. Paste selected_vars into GEE export script")
message("  2. Export feature raster from GEE for your AOI")
message("  3. Apply saved model + scaling to raster for spatial health class map")