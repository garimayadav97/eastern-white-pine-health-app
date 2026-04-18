  # =========================================================
  # 🎯 R ML PIPELINE — SAVE BEST MODEL FOR PREDICTIONS
  # =========================================================
  library(readxl)
  library(Boruta)
  library(randomForest)
  library(dplyr)
  library(caret)
  library(Metrics)
  library(xgboost)
  library(catboost)
  library(showtext)
  library(ggplot2)
  library(openxlsx)
  library(scales)
  
  # ── Font setup ────────────────────────────────────────────
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
    fit_line = "#E74C3C",
    ref_line = "#95A5A6",
    bar_fill = "#2C3E50",
    bar_best = "#E74C3C",
    strip_bg = "#ECF0F1",
    text     = "#1A1A2E"
  )
  
  # =========================================================
  # 📊 PLOT FUNCTIONS
  # =========================================================
  make_avp_plot <- function(actual, predicted, model_name, output_path,
                            lai_name = "LAI", precomp_metrics = NULL) {
    
    val_df   <- data.frame(Actual = actual, Predicted = predicted)
    q_actual <- quantile(val_df$Actual,    1, na.rm = TRUE)
    q_pred   <- quantile(val_df$Predicted, 1, na.rm = TRUE)
    val_df   <- val_df %>% filter(Actual <= q_actual, Predicted <= q_pred)
    
    if (!is.null(precomp_metrics)) {
      r2        <- precomp_metrics$R2
      rmse_val  <- precomp_metrics$RMSE
      nrmse_val <- precomp_metrics$nRMSE
      bias_val  <- precomp_metrics$Bias
    } else {
      r2        <- R2(val_df$Predicted, val_df$Actual)
      rmse_val  <- RMSE(val_df$Predicted, val_df$Actual)
      nrmse_val <- rmse_val / mean(val_df$Actual)
      bias_val  <- mean(val_df$Predicted) - mean(val_df$Actual)
    }
    
    ax_min   <- 0
    data_max <- ceiling(max(val_df$Actual, val_df$Predicted, na.rm = TRUE) * 10) / 10
    pad      <- data_max * 0.05
    ax_max   <- data_max + pad
    
    raw_step  <- ax_max / 10
    step      <- if (raw_step <= 0.2) 0.2 else
      if (raw_step <= 0.5) 0.5 else
        if (raw_step <= 1.0) 1.0 else
          if (raw_step <= 2.0) 2.0 else 5.0
    ax_breaks <- seq(0, floor(ax_max / step) * step, by = step)
    
    stats_x <- ax_max * 0.03
    stats_y <- ax_max - ax_max * 0.03
    r2_ci   <- if (!is.null(precomp_metrics) &&
                   !is.null(precomp_metrics$R2_CI_lo))
      paste0(" [", sprintf("%.2f", precomp_metrics$R2_CI_lo), "\u2013", sprintf("%.2f", precomp_metrics$R2_CI_hi), "]")
    else ""
    
    rmse_ci <- if (!is.null(precomp_metrics) &&
                   !is.null(precomp_metrics$RMSE_CI_lo))
      paste0(" [", sprintf("%.2f", precomp_metrics$RMSE_CI_lo), "\u2013", sprintf("%.2f", precomp_metrics$RMSE_CI_hi), "]")
    else ""
    
    avp_plot <- ggplot(val_df, aes(x = Actual, y = Predicted)) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = pal$ref_line, linewidth = 0.6) +
      geom_smooth(method = "lm", color = pal$fit_line,
                  fill = scales::alpha(pal$fit_line, 0.12),
                  se = TRUE, linewidth = 1.1) +
      geom_point(shape = 21, color = pal$point,
                 fill = scales::alpha(pal$point, 0.82),
                 size = 2.8, stroke = 0.4) +
      annotate("text", x = stats_x, y = stats_y,
               label = paste0(
                 "R\u00b2 = ", sprintf("%.2f", r2),     r2_ci,   "\n",
                 "RMSE = ",    sprintf("%.2f", rmse_val), rmse_ci, "\n",
                 "nRMSE = ",   sprintf("%.2f", nrmse_val),          "\n",
                 "Bias = ",    sprintf("%.4f", bias_val)
               ),
               hjust = 0, vjust = 1, size = 6.5, fontface = "bold",
               family = font_family, color = pal$text, lineheight = 1) +
      annotate("text", x = Inf, y = -Inf, label = model_name,
               hjust = 1.1, vjust = -1.1, size = 5.5, fontface = "bold.italic",
               family = font_family, color = pal$text) +
      scale_x_continuous(limits = c(ax_min, ax_max), breaks = seq(0,4,1),
                         expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(limits = c(ax_min, ax_max), breaks = seq(0,4,1),
                         expand = expansion(mult = c(0, 0))) +
      labs(x = bquote(.(paste0("Observed ",  lai_name))),
           y = bquote(.(paste0("Predicted ", lai_name)))) +
      coord_fixed(ratio = 1) +
      theme_classic(base_family = font_family, base_size = 20) +
      theme(
        text             = element_text(color = pal$text),
        axis.text        = element_text(size = 20, color = pal$text),
        axis.title       = element_text(size = 20, color = pal$text ),
        axis.line        = element_line(color = pal$text, linewidth = 0.7),
        axis.ticks       = element_line(color = pal$text, linewidth = 0.6),
        panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.7),
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    jpeg(output_path, width = 5, height = 5, units = "in", res = 600)
    showtext_begin(); print(avp_plot); showtext_end()
    dev.off()
  }
  
  make_varimp_plot <- function(model_name, model_obj, selected_vars, output_path) {
    
    if (model_name == "SVM") return(invisible(NULL))
    
    df <- switch(model_name,
                 RF  = { imp <- varImp(model_obj, scale = FALSE)$importance
                 data.frame(Predictor = rownames(imp), Importance = imp$Overall) },
                 GBM = { imp <- summary(model_obj$finalModel, plotit = FALSE)
                 data.frame(Predictor = imp$var, Importance = imp$rel.inf) },
                 XGB = { imp <- xgb.importance(model = model_obj)
                 data.frame(Predictor = imp$Feature, Importance = imp$Gain) },
                 CAT = { imp <- catboost.get_feature_importance(model_obj)
                 data.frame(Predictor = selected_vars, Importance = as.numeric(imp)) }
    )
    
    df <- df %>%
      mutate(Importance = Importance / max(Importance, na.rm = TRUE)) %>%
      arrange(desc(Importance)) %>%
      slice_head(n = 20) %>%
      mutate(
        Predictor = factor(Predictor, levels = rev(Predictor)),
        Tier = case_when(
          Importance >= 0.66 ~ "High",
          Importance >= 0.33 ~ "Mid",
          TRUE               ~ "Low"
        )
      )
    
    tier_cols <- c(High = pal$point, Mid = "#5D6D7E", Low = "#AEB6BF")
    
    vip_plot <- ggplot(df, aes(y = Predictor, x = Importance, fill = Tier)) +
      geom_col(width = 0.65, color = NA) +
      geom_text(aes(label = sprintf("%.2f", Importance)),
                hjust = -0.1, size = 7.5,
                family = font_family, color = pal$text, fontface = "bold") +
      scale_fill_manual(values = tier_cols, guide = "none") +
      scale_x_continuous(limits = c(0, 1.12), expand = c(0, 0),
                         breaks = seq(0, 1, 0.25)) +
      labs(x = "Relative Importance", y = NULL, title = model_name) +
      theme_classic(base_family = font_family, base_size = 20) +
      theme(
        plot.title       = element_text(size = 20, face = "bold",
                                        hjust = 0.5, color = pal$text),
        axis.text.y      = element_text(size = 20, color = pal$text, face = "bold"),
        axis.text.x      = element_text(size = 20, color = pal$text),
        axis.title.x     = element_text(size = 20, color = pal$text, face = "bold"),
        axis.line        = element_line(color = pal$text, linewidth = 0.6),
        axis.ticks       = element_line(color = pal$text, linewidth = 0.5),
        panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.6),
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    jpeg(output_path, width = 8, height = 6, units = "in", res = 600)
    showtext_begin(); print(vip_plot); showtext_end()
    dev.off()
  }
  
  plot_model_comparison <- function(metric_df, metric_name,
                                    output_path, best_model = NULL) {
    lower_better <- metric_name %in% c("RMSE", "nRMSE", "Bias")
    
    metric_df <- metric_df %>%
      arrange(if (lower_better) Value else desc(Value)) %>%
      mutate(
        Model  = factor(Model, levels = Model),
        IsBest = !is.null(best_model) & Model == best_model
      )
    
    p <- ggplot(metric_df, aes(x = Model, y = Value, fill = IsBest)) +
      geom_col(width = 0.6, color = NA) +
      geom_text(aes(label = sprintf("%.3f", Value)),
                vjust = -0.5, size = 5.5,
                family = font_family, fontface = "bold", color = pal$text) +
      scale_fill_manual(values = c("FALSE" = pal$bar_fill, "TRUE" = pal$bar_best),
                        guide = "none") +
      scale_y_continuous(limits = c(0, 1),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(title = metric_name, x = NULL, y = metric_name) +
      theme_classic(base_family = font_family, base_size = 20) +
      theme(
        plot.title       = element_text(size = 20, face = "bold",
                                        hjust = 0.5, color = pal$text),
        axis.text.x      = element_text(size = 20, color = pal$text, face = "bold"),
        axis.text.y      = element_text(size = 20, color = pal$text),
        axis.title.y     = element_text(size = 20, face = "bold", color = pal$text),
        axis.line        = element_line(color = pal$text, linewidth = 0.6),
        axis.ticks       = element_line(color = pal$text, linewidth = 0.5),
        panel.border     = element_rect(color = pal$text, fill = NA, linewidth = 0.6),
        panel.background = element_rect(fill = "white"),
        plot.background  = element_rect(fill = "white", color = NA)
      )
    
    jpeg(output_path, width = 7, height = 5, units = "in", res = 600)
    showtext_begin(); print(p); showtext_end()
    dev.off()
  }
  
  # =========================================================
  # 📂 Setup Paths
  # =========================================================
  setwd("/Users/garimayadav/Library/CloudStorage/OneDrive-UniversityofMaineSystem/research/chapter2_new/code/data/excels")
  
  output_dir <- "results/lai_reg_old2"
  plot_dir   <- file.path(output_dir, "lai")
  model_dir  <- file.path(output_dir, "saved_models_r")
  
  dir.create(plot_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- read_excel("forest_indices_v5_all.xlsx", sheet = "forest_indices_v5_all")
  
  # =========================================================
  # Filter Predictors
  # =========================================================
  predictor_vars <- names(df)
  predictor_vars <- predictor_vars[
    !grepl("LAI|ID|Slope|MEAN|CNN|Plot|LCR|CE|Sep|Crown|Nov|Oct|Apr|B|Height|Count|X|Y|LCR|CBH|slope|p_100|plog|ealt|mont|Stand|State|CH|Sites|Health",
           predictor_vars)
  ]
  # 
  # predictor_vars <- predictor_vars[
  #   grepl("NDII11_Jul|TVI_Jul|CRI1_Aug|IRECI_Jul|NDII12_Jul|ARVI_Jul|CRI2_May|CRI1_Jun|VH_S1_May", predictor_vars)
  # ]
  predictor_vars <- predictor_vars[
    grepl("VV|VH|elev|IRECI|S2REP|NDVIRE|TVI|PSRI|MSR|NDII|Clre|EVI", predictor_vars) #LAI
  ]
  message("Predictors identified: ", length(predictor_vars))
  
  bootstrap_ci <- function(pred, actual, n_boot = 1000, conf = 0.95) {
    # Resample validation predictions/observations with replacement,
    # compute R² and RMSE each time, return percentile-based 95% CI.
    n     <- length(actual)
    alpha <- (1 - conf) / 2
    
    r2_boot   <- numeric(n_boot)
    rmse_boot <- numeric(n_boot)
    
    for (i in seq_len(n_boot)) {
      idx_b        <- sample(n, n, replace = TRUE)
      r2_boot[i]   <- tryCatch(R2(pred[idx_b], actual[idx_b]),   error = function(e) NA)
      rmse_boot[i] <- tryCatch(RMSE(pred[idx_b], actual[idx_b]), error = function(e) NA)
    }
    
    list(
      R2   = round(quantile(r2_boot,   c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE), 3),
      RMSE = round(quantile(rmse_boot, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE), 3)
    )
  }
  
  # =========================================================
  # 📊 Metrics helper
  # =========================================================
  # ────────────────────────────────────────────────────────────────
  # 2.  REPLACE your existing compute_metrics() with this version
  # ────────────────────────────────────────────────────────────────
  
  compute_metrics <- function(pred, actual, n_boot = 1000) {
    
    df_m   <- data.frame(actual = actual, pred = pred)
    q_act  <- quantile(df_m$actual, 1, na.rm = TRUE)
    q_pred <- quantile(df_m$pred,   1, na.rm = TRUE)
    df_m   <- df_m %>% filter(actual <= q_act, pred <= q_pred)
    
    r2_val   <- R2(df_m$pred, df_m$actual)
    rmse_val <- RMSE(df_m$pred, df_m$actual)
    
    # Bootstrap CIs on validation residuals
    ci <- bootstrap_ci(df_m$pred, df_m$actual, n_boot = n_boot)
    
    data.frame(
      R2         = round(r2_val,   3),
      R2_CI_lo   = ci$R2[1],
      R2_CI_hi   = ci$R2[2],
      RMSE       = round(rmse_val, 3),
      RMSE_CI_lo = ci$RMSE[1],
      RMSE_CI_hi = ci$RMSE[2],
      nRMSE      = round(rmse_val / mean(df_m$actual), 3),
      Bias       = round(mean(df_m$pred) - mean(df_m$actual), 4),
      p_value    = signif(cor.test(df_m$pred, df_m$actual)$p.value, 3)
    )
  }
  
  
  # =========================================================
  # ⭐ Correlation Prune → Boruta
  # =========================================================
  corr_before_boruta <- function(df, target, corr_cut = 1) {
    
    predictors <- df %>% select(-all_of(target))
    all_vars   <- colnames(predictors)
    cor_mat    <- abs(cor(predictors, use = "pairwise"))
    diag(cor_mat) <- 0
    
    remove_list <- c()
    high_pairs  <- which(cor_mat >= corr_cut, arr.ind = TRUE)
    
    if (nrow(high_pairs) > 0) {
      for (i in seq_len(nrow(high_pairs))) {
        v1 <- rownames(cor_mat)[high_pairs[i, 1]]
        v2 <- colnames(cor_mat)[high_pairs[i, 2]]
        var1 <- var(predictors[[v1]], na.rm = TRUE)
        var2 <- var(predictors[[v2]], na.rm = TRUE)
        remove_list <- c(remove_list, ifelse(var1 < var2, v1, v2))
      }
    }
    
    pruned_vars <- setdiff(all_vars, unique(remove_list))
    message("Variables after correlation pruning: ", length(pruned_vars))
    
    bor <- Boruta(
      x       = predictors[, pruned_vars, drop = FALSE],
      y       = df[[target]],
      doTrace = 1
    )
    
    bor_stats  <- attStats(bor)
    final_vars <- rownames(bor_stats)[bor_stats$decision == "Confirmed"]
    
    if (length(final_vars) < 2)
      stop("Not enough variables confirmed by Boruta (", length(final_vars), ")")
    
    message("Variables confirmed by Boruta: ", length(final_vars))
    final_vars
  }
  
  # =========================================================
  # 🔁 MAIN PIPELINE
  lai  <- "LAI"
  seed <- 111
  #seed 
  #seed 42 tc
  #seed <- 99
  #seed <- 42 th
  #seed <- ch 111
  #seed 111 - LAI, CL #uncomment 
  set.seed(seed)
  
  # ── KEY FIX: initialize accumulators FRESH before pipeline ───
  metrics_list   <- list()   # was never initialized — caused stale Excel output
  full_pred_list <- list()   # same issue
  
  message("\n========== Processing: ", lai, " ==========")
  
  # ── Data prep ─────────────────────────────────────────────
  df_sub <- df %>%
    select(all_of(c(lai, predictor_vars))) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit()
  
  if (nrow(df_sub) < 25) stop("Too few rows (", nrow(df_sub), ")")
  
  nzv           <- nearZeroVar(df_sub[, predictor_vars])
  pred_filtered <- predictor_vars
  message("Predictors after NZV removal: ", length(pred_filtered))
  
  y <- df_sub[[lai]]
  
  # ── Train / val split ─────────────────────────────────────
  idx         <- createDataPartition(y, p = 0.8, list = FALSE)
  train_x_raw <- df_sub[idx,  pred_filtered]
  val_x_raw   <- df_sub[-idx, pred_filtered]
  train_y     <- y[idx]
  val_y       <- y[-idx]
  
  # ── Min-max scaling ───────────────────────────────────────
  min_vals   <- sapply(train_x_raw, min,  na.rm = TRUE)
  max_vals   <- sapply(train_x_raw, max,  na.rm = TRUE)
  range_vals <- pmax(max_vals - min_vals, .Machine$double.eps)
  
  scale_minmax <- function(df_in) {
    as.data.frame(sweep(sweep(as.matrix(df_in), 2, min_vals, "-"),
                        2, range_vals, "/"))
  }
  
  train_x <- scale_minmax(train_x_raw)
  val_x   <- scale_minmax(val_x_raw)
  all_x   <- scale_minmax(df_sub[, pred_filtered])
  
  # ── Feature selection ─────────────────────────────────────
  df_fs         <- data.frame(y = train_y, train_x)
  colnames(df_fs)[1] <- lai
  selected_vars <- corr_before_boruta(df_fs, lai, corr_cut = 0.75)
  
  train_top <- train_x[, selected_vars, drop = FALSE]
  val_top   <- val_x[,   selected_vars, drop = FALSE]
  all_top   <- all_x[,   selected_vars, drop = FALSE]
  
  # =========================================================
  # 🚀 MODEL TRAINING
  # =========================================================
  model_preds   <- list()
  model_metrics <- list()
  model_objects <- list()
  
  # ── RF ────────────────────────────────────────────────────
  message("Training RF...")
  rf_model <- train(
    x = train_top, y = train_y,
    method    = "rf",
    trControl = trainControl(method = "cv", number = 10),
    tuneGrid  = expand.grid(mtry = seq(2, min(5, length(selected_vars)), 1)),
    ntree     = 1200
  )
  model_preds$RF   <- predict(rf_model, val_top)
  model_objects$RF <- rf_model
  
  # ── SVM ───────────────────────────────────────────────────
  message("Training SVM...")
  svm_model <- train(
    x = train_top, y = train_y,
    method     = "svmRadial",
    trControl  = trainControl(method = "cv", number = 5),
    tuneLength = 5
  )
  model_preds$SVM   <- predict(svm_model, val_top)
  model_objects$SVM <- svm_model
  
  # ── GBM ───────────────────────────────────────────────────
  message("Training GBM...")
  gbm_model <- train(
    x = train_top, y = train_y,
    method    = "gbm",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 5,
    verbose   = FALSE
  )
  model_preds$GBM   <- predict(gbm_model, val_top)
  model_objects$GBM <- gbm_model
  
  # ── XGBoost ───────────────────────────────────────────────
  message("Training XGB...")
  dtrain <- xgb.DMatrix(as.matrix(train_top), label = train_y)
  dvalid <- xgb.DMatrix(as.matrix(val_top),   label = val_y)
  
  xgb_model <- xgb.train(
    params = list(
      objective        = "reg:squarederror",
      eta              = 0.1,
      max_depth        = 4,
      subsample        = 0.9,
      colsample_bytree = 0.9
    ),
    data                  = dtrain,
    nrounds               = 300,
    early_stopping_rounds = 20,
    watchlist             = list(train = dtrain, val = dvalid),
    verbose               = 0
  )
  model_preds$XGB   <- predict(xgb_model, as.matrix(val_top))
  model_objects$XGB <- xgb_model
  
  # ── CatBoost ──────────────────────────────────────────────
  message("Training CatBoost...")
  pool_train <- catboost.load_pool(train_top, label = train_y)
  pool_val   <- catboost.load_pool(val_top)
  
  cat_model <- catboost.train(
    learn_pool = pool_train,
    test_pool  = pool_val,
    params     = list(
      loss_function = "RMSE",
      iterations    = 400,
      learning_rate = 0.05,
      depth         = 6,
      task_type     = "CPU",
      logging_level = "Silent"
    )
  )
  model_preds$CAT   <- catboost.predict(cat_model, pool_val)
  model_objects$CAT <- cat_model
  
  # =========================================================
  # 📊 METRICS
  # =========================================================
  for (model in names(model_preds)) {
    model_metrics[[model]] <- compute_metrics(model_preds[[model]], val_y)
  }
  
  # ── Best model by R² ──────────────────────────────────────
  r2_values       <- sapply(model_metrics, function(x) x$R2)
  best_model_name <- names(which.max(r2_values))
  best_model_obj  <- model_objects[[best_model_name]]
  
  message("\n========================================")
  message("Best model: ", best_model_name,
          " (R² = ", round(max(r2_values), 3), ")")
  message("========================================")
  
  # ── Print all metrics ─────────────────────────────────────
  message("\nValidation Metrics:")
  for (m in names(model_metrics)) {
    met <- model_metrics[[m]]
    message(sprintf("%-5s | R²=%.2f  RMSE=%.2f  nRMSE=%.2f  Bias=%.2f  p=%.4f",
                    m, met$R2, met$RMSE, met$nRMSE, met$Bias, met$p_value))
  }
  
  # ── KEY FIX: store metrics BEFORE saving Excel ───────────
  metrics_list[[lai]] <- model_metrics
  
  # =========================================================
  # 💾 SAVE BEST MODEL + METADATA
  # =========================================================
  message("\n💾 Saving best model and metadata...")
  
  model_path <- file.path(model_dir,
                          paste0("best_model_", lai, "_", best_model_name, ".rds"))
  #saveRDS(best_model_obj, model_path)
  message("✓ Model saved: ", model_path)
  
  metadata <- list(
    best_model_name = best_model_name,
    selected_vars   = selected_vars,
    min_vals        = min_vals,
    max_vals        = max_vals,
    range_vals      = range_vals,
    pred_filtered   = pred_filtered,
    lai_name        = lai,
    metrics         = model_metrics
  )
  metadata_path <- file.path(model_dir, paste0("metadata_", lai, ".rds"))
  #saveRDS(metadata, metadata_path)
  message("✓ Metadata saved: ", metadata_path)
  
  write.csv(
    data.frame(
      key   = c("best_model_name", "n_selected_vars", "n_total_predictors"),
      value = c(best_model_name, length(selected_vars), length(pred_filtered))
    ),
    file.path(model_dir, "summary.csv"),
    row.names = FALSE
  )
  
  # =========================================================
  # 📈 MODEL COMPARISON PLOTS
  # =========================================================
  metric_names <- c("R2", "RMSE", "nRMSE", "Bias")
  
  for (met in metric_names) {
    vals <- sapply(model_metrics, function(x) x[[met]])
    df_c <- data.frame(Model = names(vals), Value = as.numeric(vals),
                       stringsAsFactors = FALSE)
    df_c <- df_c[is.finite(df_c$Value), ]
    plot_model_comparison(
      metric_df   = df_c,
      metric_name = met,
      best_model  = best_model_name,
      output_path = file.path(plot_dir,
                              paste0("Model_Comparison_", lai, "_", met, ".jpeg"))
    )
  }
  
  # =========================================================
  # 📈 AVP & VARIABLE IMPORTANCE PLOTS
  # =========================================================
  for (model_name in names(model_preds)) {
    
    make_avp_plot(
      actual          = val_y,
      predicted       = model_preds[[model_name]],
      model_name      = model_name,
      lai_name        = lai,
      precomp_metrics = model_metrics[[model_name]],
      output_path     = file.path(plot_dir,
                                  paste0("AVP_", lai, "_", model_name, ".jpeg"))
    )
    
    if (model_name == "RF")
      make_varimp_plot("RF",  rf_model,  selected_vars,
                       file.path(plot_dir, paste0("VarImp_", lai, "_RF.jpeg")))
    if (model_name == "GBM")
      make_varimp_plot("GBM", gbm_model, selected_vars,
                       file.path(plot_dir, paste0("VarImp_", lai, "_GBM.jpeg")))
    if (model_name == "XGB")
      make_varimp_plot("XGB", xgb_model, selected_vars,
                       file.path(plot_dir, paste0("VarImp_", lai, "_XGB.jpeg")))
    if (model_name == "CAT")
      make_varimp_plot("CAT", cat_model, selected_vars,
                       file.path(plot_dir, paste0("VarImp_", lai, "_CAT.jpeg")))
  }
  
  # =========================================================
  # 💾 Full predictions (train + test)
  # =========================================================
  
  # ── KEY FIX: store full predictions with CURRENT selected_vars
  full_pred_list[[lai]] <- data.frame(
    Actual = y,
    Split  = ifelse(seq_along(y) %in% idx, "Train", "Test"),
    RF     = predict(rf_model,  all_top[, selected_vars, drop = FALSE]),
    SVM    = predict(svm_model, all_top[, selected_vars, drop = FALSE]),
    GBM    = predict(gbm_model, all_top[, selected_vars, drop = FALSE]),
    XGB    = predict(xgb_model, as.matrix(all_top[, selected_vars, drop = FALSE])),
    CAT    = catboost.predict(cat_model,
                              catboost.load_pool(all_top[, selected_vars,
                                                         drop = FALSE]))
  )
  
  # =========================================================
  # 💾 Save Metrics + Predictions to Excel
  # =========================================================
  # ── KEY FIX: save Excel AFTER both metrics_list and
  #    full_pred_list are populated with current run results
  for (lai_name in names(metrics_list)) {
    
    wb <- createWorkbook()
    
    # One sheet per model with its metrics
    for (model in names(metrics_list[[lai_name]])) {
      addWorksheet(wb, model)
      writeData(wb, model, metrics_list[[lai_name]][[model]])
    }
    
    # Summary sheet with all model metrics side by side
    addWorksheet(wb, "Metrics_Summary")
    metrics_summary <- do.call(rbind, lapply(names(metrics_list[[lai_name]]),
                                             function(m) cbind(Model = m, metrics_list[[lai_name]][[m]])
    ))
    writeData(wb, "Metrics_Summary", metrics_summary)
    
    # Full predictions sheet
    addWorksheet(wb, "Full_Predictions")
    writeData(wb, "Full_Predictions", full_pred_list[[lai_name]])
    
    # Selected variables sheet
    addWorksheet(wb, "Selected_Vars")
    writeData(wb, "Selected_Vars",
              data.frame(Variable = selected_vars,
                         stringsAsFactors = FALSE))
    
    out_path <- file.path(output_dir, paste0("Results_", lai_name, ".xlsx"))
    saveWorkbook(wb, out_path, overwrite = TRUE)
    message("✓ Saved: ", out_path)
  }
  model_metrics
  # =========================================================
  # 📋 SUMMARY
  # =========================================================
  message("\n✅ PIPELINE COMPLETE")
  message("Files saved to: ", model_dir)
  message("\nSelected variables (", length(selected_vars), "):")
  message(paste0("['", paste(selected_vars, collapse = "', '"), "']"))