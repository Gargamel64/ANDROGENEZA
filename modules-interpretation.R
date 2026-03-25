# =========================================================
# 🔥 MODULE: INTERPRETATION (SCIENTIFIC FINAL VERSION)
# =========================================================

observe({

  cat("\n================ INTERPRETATION START ================\n")

  req(rv$model)
  req(rv$X)
  req(rv$y)

  X <- as.data.frame(rv$X)
  X_mat <- as.matrix(rv$X)

  model_list <- rv$model

  targets <- names(model_list)

  validate(
    need(nrow(X) > 10, "Za mało danych"),
    need(length(targets) > 0, "Brak modeli")
  )

  cat("Targets:\n")
  print(targets)

  # =====================================================
  # 🔥 FEATURE IMPORTANCE
  # =====================================================

  output$importance_plot <- renderPlotly({

    imp_list <- list()

    for (target in targets){

      model <- model_list[[target]]

      imp <- tryCatch({
        xgboost::xgb.importance(model = model)
      }, error = function(e) NULL)

      if (!is.null(imp)){
        imp$target <- target
        imp_list[[target]] <- imp
      }
    }

    validate(need(length(imp_list) > 0, "Brak importance"))

    imp_df <- do.call(rbind, imp_list)

    agg <- aggregate(Gain ~ Feature, data = imp_df, mean)
    agg <- agg[order(-agg$Gain), ]

    plot_ly(
      agg,
      x = ~Gain,
      y = ~Feature,
      type = "bar",
      orientation = "h"
    )
  })

  # =====================================================
  # 🔥 SHAP
  # =====================================================

  output$shap_plot <- renderPlotly({

    shap_all <- list()

    for (target in targets){

      model <- model_list[[target]]

      shap <- tryCatch({
        predict(model, X_mat, predcontrib = TRUE)
      }, error = function(e) NULL)

      if (is.null(shap)) next

      shap_df <- as.data.frame(shap)

      if ("BIAS" %in% colnames(shap_df)){
        shap_df <- shap_df[, colnames(shap_df) != "BIAS", drop=FALSE]
      }

      shap_all[[target]] <- shap_df
    }

    validate(need(length(shap_all) > 0, "Brak SHAP"))

    shap_df <- do.call(rbind, shap_all)

    shap_imp <- colMeans(abs(shap_df))

    shap_imp_df <- data.frame(
      feature = names(shap_imp),
      importance = shap_imp
    )

    shap_imp_df <- shap_imp_df[order(-shap_imp_df$importance), ]

    plot_ly(
      shap_imp_df,
      x = ~importance,
      y = ~feature,
      type = "bar",
      orientation = "h"
    )
  })

  # =====================================================
  # 🔥 PARTIAL DEPENDENCE (NAUKA!)
  # =====================================================

  output$pdp_plot <- renderPlotly({

    target <- targets[1]
    model <- model_list[[target]]

    feature <- colnames(X)[1]

    values <- seq(
      min(X[[feature]], na.rm=TRUE),
      max(X[[feature]], na.rm=TRUE),
      length.out = 50
    )

    preds <- numeric(length(values))

    base <- colMeans(X, na.rm=TRUE)

    for(i in seq_along(values)){
      tmp <- base
      tmp[feature] <- values[i]
      preds[i] <- predict(model, matrix(tmp, nrow=1))
    }

    plot_ly(
      x = values,
      y = preds,
      type = "scatter",
      mode = "lines"
    )
  })

  # =====================================================
  # 🔥 AUTOMATYCZNE WNIOSKI (KLUCZ!)
  # =====================================================

  output$interpretation_text <- renderPrint({

    cat("\n===== INTERPRETACJA NAUKOWA =====\n")

    # importance
    imp <- tryCatch({
      xgboost::xgb.importance(model = model_list[[targets[1]]])
    }, error=function(e) NULL)

    if (!is.null(imp)){

      top <- head(imp$Feature, 5)

      cat("\nNajważniejsze zmienne:\n")
      print(top)

      cat("\nInterpretacja:\n")

      for(f in top){
        cat("-", f, "ma istotny wpływ na GPRE\n")
      }
    }

    # PDP wniosek
    feature <- colnames(X)[1]

    cat("\nAnaliza wpływu zmiennej:", feature, "\n")

    vals <- seq(min(X[[feature]]), max(X[[feature]]), length.out=50)
    base <- colMeans(X)

    preds <- sapply(vals, function(v){
      tmp <- base
      tmp[feature] <- v
      predict(model_list[[targets[1]]], matrix(tmp, nrow=1))
    })

    best_idx <- which.max(preds)

    cat("\nOptimum:\n")
    cat(feature, "≈", round(vals[best_idx],3), "\n")

    cat("\nWniosek biologiczny:\n")
    cat("Ten zakres maksymalizuje efektywność androgenezy (GPRE).\n")

  })

})