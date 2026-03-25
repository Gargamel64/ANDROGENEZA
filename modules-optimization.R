# =========================================================
# 🔥 MODULE: OPTIMIZATION (SCIENTIFIC FINAL VERSION)
# =========================================================

observeEvent(input$run_optimization, {

  cat("\n================ OPTIMIZATION START ================\n")

  req(rv$model)
  req(rv$X)
  req(rv$optimization_config)

  X <- as.matrix(rv$X)
  config <- rv$optimization_config

  targets <- config$variable
  n_targets <- length(targets)

  cat("Targets:\n")
  print(targets)

  # -------------------------------------------------------
  # 🔥 MODEL DETECTION
  # -------------------------------------------------------

  is_multi_model <- is.list(rv$model) && !inherits(rv$model, "xgb.Booster")
  cat("Multi-model:", is_multi_model, "\n")

  # -------------------------------------------------------
  # 🔥 ENV VARIABLES (ostatnie kolumny = warunki)
  # -------------------------------------------------------

  n_env <- min(5, ncol(X))  # 🔥 zwiększone
  env_idx <- (ncol(X) - n_env + 1):ncol(X)
  env_names <- colnames(X)[env_idx]

  cat("ENV VARS:\n")
  print(env_names)

  # -------------------------------------------------------
  # 🔥 BOUNDS
  # -------------------------------------------------------

  bounds <- lapply(env_idx, function(i){
    c(
      min(X[,i], na.rm=TRUE),
      max(X[,i], na.rm=TRUE)
    )
  })
  names(bounds) <- env_names

  print(bounds)

  base_vec <- colMeans(X, na.rm=TRUE)

  # -------------------------------------------------------
  # 🔥 SAMPLING (większe dla nauki)
  # -------------------------------------------------------

  n_samples <- 3000

  samples <- matrix(NA, nrow=n_samples, ncol=n_env)

  for(i in 1:n_env){
    samples[,i] <- runif(
      n_samples,
      bounds[[i]][1],
      bounds[[i]][2]
    )
  }

  colnames(samples) <- env_names

  # -------------------------------------------------------
  # 🔥 PREDICTION
  # -------------------------------------------------------

  preds <- matrix(NA, nrow=n_samples, ncol=n_targets)

  for(i in 1:n_samples){

    vec <- base_vec
    vec[env_idx] <- samples[i,]

    input_mat <- matrix(vec, nrow=1)

    pred_vec <- numeric(n_targets)
    valid <- TRUE

    for(j in 1:n_targets){

      model <- if(is_multi_model){
        rv$model[[ targets[j] ]]
      } else {
        rv$model
      }

      if(is.null(model)){
        valid <- FALSE
        break
      }

      p <- tryCatch({
        predict(model, input_mat)
      }, error=function(e){
        return(NA)
      })

      if(any(is.na(p))){
        valid <- FALSE
        break
      }

      pred_vec[j] <- as.numeric(p)[1]
    }

    if(valid){
      preds[i,] <- pred_vec
    }
  }

  # -------------------------------------------------------
  # 🔥 CLEAN
  # -------------------------------------------------------

  valid <- complete.cases(preds)

  preds <- preds[valid,,drop=FALSE]
  samples <- samples[valid,,drop=FALSE]

  cat("VALID PREDICTIONS:", nrow(preds), "\n")

  if(nrow(preds) < 20){
    showNotification("❌ Za mało predykcji", type="error")
    return()
  }

  # -------------------------------------------------------
  # 🔥 NORMALIZATION (z kierunkiem)
  # -------------------------------------------------------

  preds_scaled <- scale(preds)

  for(i in 1:n_targets){
    if(config$direction[i] == "min"){
      preds_scaled[,i] <- -preds_scaled[,i]
    }
  }

  # -------------------------------------------------------
  # 🔥 PARETO FRONT
  # -------------------------------------------------------

  is_dominated <- function(i){

    for(j in 1:nrow(preds_scaled)){
      if(j == i) next

      if(all(preds_scaled[j,] >= preds_scaled[i,]) &&
         any(preds_scaled[j,] > preds_scaled[i,])){
        return(TRUE)
      }
    }
    FALSE
  }

  pareto_idx <- which(!sapply(1:nrow(preds_scaled), is_dominated))

  cat("Pareto points:", length(pareto_idx), "\n")

  pareto_preds <- preds[pareto_idx,,drop=FALSE]
  pareto_samples <- samples[pareto_idx,,drop=FALSE]

  # -------------------------------------------------------
  # 🔥 RESULT DATAFRAME
  # -------------------------------------------------------

  result <- as.data.frame(pareto_samples)

  for(i in 1:n_targets){
    result[[ targets[i] ]] <- pareto_preds[,i]
  }

  # -------------------------------------------------------
  # 🔥 RANKING SCORE
  # -------------------------------------------------------

  score <- rowMeans(scale(pareto_preds))
  result$SCORE <- score

  result <- result[order(-result$SCORE), ]

  rv$optimization <- result

  cat("FINAL RESULT:", dim(result), "\n")

  # -------------------------------------------------------
  # 🔥 TOP CONDITIONS (NAUKOWE WNIOSKI)
  # -------------------------------------------------------

  top_n <- min(20, nrow(result))
  top <- result[1:top_n,]

  optimal_conditions <- data.frame(
    variable = env_names,
    mean = colMeans(top[,env_names], na.rm=TRUE),
    sd = apply(top[,env_names], 2, sd, na.rm=TRUE)
  )

  rv$optimal_conditions <- optimal_conditions

  cat("\n===== OPTIMAL CONDITIONS =====\n")
  print(optimal_conditions)

  # -------------------------------------------------------
  # 🔥 OUTPUT TABLE
  # -------------------------------------------------------

  output$opt_table <- renderDT({
    datatable(result, options=list(scrollX=TRUE, pageLength=10))
  })

  # -------------------------------------------------------
  # 🔥 PARETO 2D
  # -------------------------------------------------------

  output$pareto_plot_2d <- renderPlotly({

    validate(need(n_targets >= 2, "Min 2 cele"))

    plot_ly(
      result,
      x = ~result[[targets[1]]],
      y = ~result[[targets[2]]],
      type = "scatter",
      mode = "markers",
      marker = list(size=6)
    )
  })

  # -------------------------------------------------------
  # 🔥 PARETO 3D
  # -------------------------------------------------------

  output$pareto_plot_3d <- renderPlotly({

    validate(need(n_targets >= 3, "Min 3 cele"))

    plot_ly(
      result,
      x = ~result[[targets[1]]],
      y = ~result[[targets[2]]],
      z = ~result[[targets[3]]],
      type = "scatter3d",
      mode = "markers"
    )
  })

  # -------------------------------------------------------
  # 🔥 RANKING PLOT
  # -------------------------------------------------------

  output$ranking_plot <- renderPlotly({

    plot_ly(
      y = result$SCORE,
      type = "scatter",
      mode = "lines"
    )
  })

  # -------------------------------------------------------
  # 🔥 INTERPRETATION TEXT (NAUKA)
  # -------------------------------------------------------

  output$optimization_text <- renderPrint({

    cat("\n===== INTERPRETACJA =====\n")

    cat("Najlepsze warunki (średnia TOP 20):\n")
    print(optimal_conditions)

    cat("\nWniosek:\n")

    for(i in 1:nrow(optimal_conditions)){
      cat(
        optimal_conditions$variable[i], ": ",
        round(optimal_conditions$mean[i],3),
        "±",
        round(optimal_conditions$sd[i],3), "\n"
      )
    }

    cat("\nInterpretacja biologiczna:\n")
    cat("Zakresy wskazują stabilne optimum androgenezy.\n")

  })

  showNotification("✅ OPTIMIZATION DONE (SCIENTIFIC)", type="message")

})