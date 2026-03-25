# =========================================================
# 🔥 MODULE: RANKING (FINAL FIX – SINGLE MODEL + PARETO)
# =========================================================

observe({

  cat("\n===== RANKING START =====\n")

  req(rv$model)
  req(rv$X)
  req(rv$y)
  req(rv$optimization_config)

  X <- as.matrix(rv$X)
  model <- rv$model
  config <- rv$optimization_config

  targets <- config$variable
  n_targets <- length(targets)

  cat("Targets:\n")
  print(targets)

  # -------------------------------------------------------
  # 🔥 PREDYKCJA NA CAŁYM X
  # -------------------------------------------------------

  preds <- tryCatch({
    predict(model, X)
  }, error=function(e){
    cat("❌ PRED ERROR:", e$message, "\n")
    return(NULL)
  })

  if(is.null(preds)){
    return()
  }

  preds <- as.numeric(preds)

  # -------------------------------------------------------
  # 🔥 DATAFRAME
  # -------------------------------------------------------

  ranking <- data.frame(
    ID = 1:length(preds),
    prediction = preds
  )

  # -------------------------------------------------------
  # 🔥 NORMALIZACJA
  # -------------------------------------------------------

  ranking$score <- scale(ranking$prediction)

  # -------------------------------------------------------
  # 🔥 KIERUNEK (max / min)
  # -------------------------------------------------------

  if(config$direction[1] == "min"){
    ranking$score <- -ranking$score
  }

  # -------------------------------------------------------
  # 🔥 SORTOWANIE
  # -------------------------------------------------------

  ranking <- ranking[order(-ranking$score), ]

  # -------------------------------------------------------
  # 🔥 TOP FLAG
  # -------------------------------------------------------

  ranking$TOP10 <- FALSE
  ranking$TOP10[1:min(10, nrow(ranking))] <- TRUE

  rv$ranking <- ranking

  cat("✅ RANKING READY:", dim(ranking), "\n")

  # -------------------------------------------------------
  # 📋 TABLE
  # -------------------------------------------------------

  output$ranking_table <- renderDT({
    datatable(ranking)
  })

  # -------------------------------------------------------
  # 📊 PLOT
  # -------------------------------------------------------

  output$ranking_plot <- renderPlotly({

    plot_ly(
      ranking,
      x = ~ID,
      y = ~score,
      type = "bar",
      color = ~TOP10
    )
  })

})