# =========================================================
# 🔥 MODULE: PREDICTION (FINAL FIX – SINGLE MODEL COMPATIBLE)
# =========================================================

observe({

  cat("\n===== PREDICTION START =====\n")

  req(rv$model)
  req(rv$X)
  req(rv$y)

  # -------------------------------------------------------
  # 🔥 TARGETY
  # -------------------------------------------------------

  targets <- unique(c(
    input$target_vars_max,
    input$target_vars_min
  ))

  targets <- targets[targets != ""]

  cat("Targets:\n")
  print(targets)

  if(length(targets) == 0){
    cat("❌ NO TARGETS\n")
    return()
  }

  # -------------------------------------------------------
  # 🔥 MODEL
  # -------------------------------------------------------

  model <- rv$model

  # -------------------------------------------------------
  # 🔥 BASELINE (oryginalne dane)
  # -------------------------------------------------------

  X <- as.matrix(rv$X)

  baseline_preds <- tryCatch({
    predict(model, X)
  }, error=function(e){
    cat("❌ BASELINE ERROR:", e$message, "\n")
    return(NULL)
  })

  if(is.null(baseline_preds)){
    return()
  }

  baseline_df <- data.frame(pred = as.numeric(baseline_preds))

  cat("Baseline predictions:", nrow(baseline_df), "\n")

  # -------------------------------------------------------
  # 🔥 OPTIMIZED (jeśli istnieje)
  # -------------------------------------------------------

  optimized_df <- NULL

  if(!is.null(rv$optimization)){

    cat("Using optimization results\n")

    opt <- rv$optimization

    pred_list <- list()

    for(i in 1:nrow(opt)){

      tmp <- colMeans(X)

      for(col in colnames(opt)){
        if(col %in% colnames(X)){
          tmp[colnames(X) == col] <- opt[[col]][i]
        }
      }

      tmp_mat <- matrix(tmp, nrow=1)

      p <- tryCatch({
        predict(model, tmp_mat)
      }, error=function(e){
        cat("❌ OPT PRED ERROR:", e$message, "\n")
        return(NULL)
      })

      if(!is.null(p)){
        pred_list[[i]] <- as.numeric(p)
      }
    }

    if(length(pred_list) > 0){
      optimized_df <- data.frame(pred = unlist(pred_list))
    }
  }

  # -------------------------------------------------------
  # 🔥 INTERPRETACJA
  # -------------------------------------------------------

  result_df <- baseline_df

  if(!is.null(optimized_df)){

    baseline_mean <- mean(baseline_df$pred, na.rm=TRUE)

    optimized_df$delta <- optimized_df$pred - baseline_mean

    result_df <- optimized_df
  }

  rv$predictions <- result_df

  cat("✅ PREDICTIONS READY:", dim(result_df), "\n")

  # -------------------------------------------------------
  # 📋 TABLE
  # -------------------------------------------------------

  output$prediction_table <- renderDT({
    datatable(result_df)
  })

  # -------------------------------------------------------
  # 📊 PLOT
  # -------------------------------------------------------

  output$prediction_plot <- renderPlotly({

    plot_ly(
      result_df,
      y = ~pred,
      type = "scatter",
      mode = "markers"
    )
  })

})