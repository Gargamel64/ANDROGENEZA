# =========================================================
# 🔥 MODULE: NEURAL NETWORK → XGBOOST (SCIENTIFIC FINAL)
# =========================================================

library(xgboost)
library(future)
library(promises)
library(digest)
library(plotly)

plan(multisession)

# =========================================================
# 🔥 HASH (CACHE)
# =========================================================

create_model_hash <- function(X, y, params) {
  digest(list(
    X_dim = dim(X),
    y_dim = dim(y),
    params = params
  ))
}

get_model_path <- function(hash) {
  file.path("ANDROGENESIS_AI", "models", paste0("model_", hash, ".rds"))
}

# =========================================================
# 🔥 METRICS
# =========================================================

calc_metrics <- function(y_true, y_pred){

  rmse <- sqrt(mean((y_true - y_pred)^2))

  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)

  r2 <- 1 - ss_res/ss_tot

  list(
    RMSE = rmse,
    R2 = r2
  )
}

# =========================================================
# 🔥 TRAIN EVENT
# =========================================================

observeEvent(input$train_model, {

  cat("\n================ TRAIN START ================\n")

  req(rv$X)
  req(rv$y)

  X_mat <- rv$X
  y_mat <- rv$y

  cat("X dim:", dim(X_mat), "\n")
  cat("Y dim:", dim(y_mat), "\n")

  validate(
    need(!is.null(y_mat), "Brak Y"),
    need(ncol(y_mat) > 0, "Brak zmiennych Y"),
    need(nrow(X_mat) > 10, "Za mało danych")
  )

  # -------------------------------------------------------
  # 🔥 CLEAN NUMERIC
  # -------------------------------------------------------

  X_mat <- apply(X_mat, 2, function(x) as.numeric(x))
  y_mat <- apply(y_mat, 2, function(x) as.numeric(x))

  X_mat <- as.matrix(X_mat)
  y_mat <- as.matrix(y_mat)

  # -------------------------------------------------------
  # 🔥 TARGET NAMES
  # -------------------------------------------------------

  targets <- colnames(y_mat)

  if (is.null(targets)) {
    targets <- paste0("Y", seq_len(ncol(y_mat)))
    colnames(y_mat) <- targets
  }

  cat("Targets:\n")
  print(targets)

  # -------------------------------------------------------
  # 🔥 PARAMS (NOWA SKŁADNIA XGBOOST)
  # -------------------------------------------------------

  params <- list(
    objective = "reg:squarederror",
    learning_rate = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  nrounds <- input$epochs %||% 200

  # -------------------------------------------------------
  # 🔥 CACHE
  # -------------------------------------------------------

  hash <- create_model_hash(X_mat, y_mat, params)
  model_path <- get_model_path(hash)

  if (file.exists(model_path)) {

    cat("✔ CACHE HIT\n")

    rv$model <- readRDS(model_path)

    showNotification("✔ Model z cache", type="message")
    return()
  }

  showNotification("⏳ Trening modelu...", type="message")

  # -------------------------------------------------------
  # 🔥 FUTURE TRAINING
  # -------------------------------------------------------

  X_local <- X_mat
  y_local <- y_mat
  targets_local <- targets

  future({

    model_list <- list()
    metrics_list <- list()
    pred_train <- list()

    for (i in seq_len(ncol(y_local))) {

      cat("TRAIN:", targets_local[i], "\n")

      y <- y_local[, i]

      dtrain <- xgboost::xgb.DMatrix(
        data = X_local,
        label = y
      )

      model <- xgboost::xgb.train(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        verbose = 0
      )

      pred <- predict(model, X_local)

      metrics <- calc_metrics(y, pred)

      model_list[[ targets_local[i] ]] <- model
      metrics_list[[ targets_local[i] ]] <- metrics
      pred_train[[ targets_local[i] ]] <- pred
    }

    list(
      model = model_list,
      metrics = metrics_list,
      pred = pred_train
    )

  }) %...>% (function(res) {

    cat(">> TRAIN DONE\n")

    model_list <- res$model
    metrics_list <- res$metrics
    pred_train <- res$pred

    dir.create("ANDROGENESIS_AI/models",
               recursive = TRUE,
               showWarnings = FALSE)

    saveRDS(model_list, model_path)

    rv$model <- model_list
    rv$model_metrics <- metrics_list
    rv$train_pred <- pred_train

    # -------------------------------------------------------
    # 🔥 METRICS PRINT
    # -------------------------------------------------------

    cat("\n===== METRICS =====\n")
    print(metrics_list)

    # -------------------------------------------------------
    # 🔥 PREDICTION PLOT
    # -------------------------------------------------------

    output$prediction_plot <- renderPlotly({

      target <- names(pred_train)[1]

      plot_ly(
        y = pred_train[[target]],
        type = "scatter",
        mode = "lines",
        name = "Prediction"
      ) %>%
        add_trace(
          y = rv$y[,1],
          name = "Real"
        )
    })

    # -------------------------------------------------------
    # 🔥 PRED TABLE
    # -------------------------------------------------------

    output$prediction_table <- renderDT({

      target <- names(pred_train)[1]

      data.frame(
        REAL = rv$y[,1],
        PRED = pred_train[[target]]
      )
    })

    showNotification("✅ Model gotowy (SCIENTIFIC)", type="message")

  }) %...!% (function(e) {

    cat("\n❌ TRAIN ERROR:\n")
    print(e)

    showNotification(
      paste("Błąd:", e$message),
      type = "error"
    )

  })

})