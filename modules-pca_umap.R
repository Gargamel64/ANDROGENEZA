# =========================================================
# 🔥 MODULE: PCA + UMAP (PARETO CONSISTENT)
# =========================================================

library(plotly)

observe({

  # -------------------------------------------------------
  # 🔒 WALIDACJA
  # -------------------------------------------------------
  req(rv$phenotypes)
  req(rv$preprocess)

  df <- rv$phenotypes
  prep <- rv$preprocess

  validate(
    need(is.data.frame(df), "Dane nie są data.frame"),
    need(nrow(df) > 5, "Za mało danych do PCA/UMAP")
  )

  # -------------------------------------------------------
  # 🔥 USUŃ LIST-COLUMNS
  # -------------------------------------------------------
  df <- df[, unlist(lapply(df, function(x) !is.list(x))), drop = FALSE]

  # -------------------------------------------------------
  # 🔥 DEFINICJE Z PREPROCESSINGU
  # -------------------------------------------------------
  condition_cols <- prep$condition_cols
  marker_cols <- prep$marker_cols

  validate(
    need(length(condition_cols) > 0, "Brak condition_cols"),
    need(length(marker_cols) >= 0, "Brak marker_cols")
  )

  # -------------------------------------------------------
  # 🔧 X (ZGODNY Z MODELEM)
  # -------------------------------------------------------
  X <- df[, c(condition_cols, marker_cols), drop = FALSE]

  # -------------------------------------------------------
  # 🔥 SAFE NUMERIC FILTER
  # -------------------------------------------------------
  X <- X[, unlist(lapply(X, is.numeric)), drop = FALSE]

  validate(
    need(ncol(X) >= 2, "Za mało kolumn numerycznych do PCA/UMAP")
  )

  # -------------------------------------------------------
  # 🔧 CLEAN NA
  # -------------------------------------------------------
  X <- na.omit(X)

  validate(
    need(nrow(X) > 5, "Za mało danych po usunięciu NA")
  )

  # -------------------------------------------------------
  # 🔥 SCALING (ZGODNY Z PIPELINE)
  # -------------------------------------------------------
  scaler <- prep$scaler

  X_scaled <- as.data.frame(
    scale(X, center = scaler$mean, scale = scaler$sd)
  )

  # -------------------------------------------------------
  # 🔥 PCA
  # -------------------------------------------------------
  pca_res <- tryCatch({
    prcomp(X_scaled, scale. = FALSE)
  }, error = function(e) NULL)

  output$pca_plot <- renderPlotly({

    validate(
      need(!is.null(pca_res), "Błąd PCA")
    )

    df_pca <- data.frame(
      PC1 = pca_res$x[, 1],
      PC2 = pca_res$x[, 2]
    )

    plot_ly(
      df_pca,
      x = ~PC1,
      y = ~PC2,
      type = "scatter",
      mode = "markers"
    )
  })

  # -------------------------------------------------------
  # 🔥 UMAP (OPTIONAL)
  # -------------------------------------------------------
  umap_available <- requireNamespace("umap", quietly = TRUE)

  output$umap_plot <- renderPlotly({

    if (!umap_available) {

      validate(
        need(FALSE, "UMAP niedostępny (brak pakietu 'umap')")
      )

    } else {

      umap_res <- tryCatch({
        umap::umap(X_scaled)
      }, error = function(e) NULL)

      validate(
        need(!is.null(umap_res), "Błąd UMAP")
      )

      df_umap <- data.frame(
        UMAP1 = umap_res$layout[, 1],
        UMAP2 = umap_res$layout[, 2]
      )

      plot_ly(
        df_umap,
        x = ~UMAP1,
        y = ~UMAP2,
        type = "scatter",
        mode = "markers"
      )
    }

  })

})