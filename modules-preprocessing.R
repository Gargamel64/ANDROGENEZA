# =========================================================
# 🔥 MODULE: PREPROCESSING (FINAL FIX – ZERO Y=NULL + NAME MAP)
# =========================================================

# =========================================================
# 🔥 STEP 1: MERGE
# =========================================================

observe({

  req(rv$phenotypes_raw)

  cat("\n===== STEP 1: START MERGE =====\n")

  df <- rv$phenotypes_raw

  cat("Phenotypes dim:", dim(df), "\n")
  print(head(df,2))

  # ---------------- VARIABILITY ----------------
  if (!is.null(rv$variability_raw) &&
      "NAZWA_REGENERANTA" %in% colnames(rv$variability_raw)) {

    cat("\n-- MERGE VARIABILITY --\n")

    df <- merge(df, rv$variability_raw,
                by = "NAZWA_REGENERANTA", all.x = TRUE)

    cat("After merge dim:", dim(df), "\n")
  }

  # ---------------- MARKERS ----------------
  if (!is.null(rv$markers_raw) &&
      "MARKER" %in% colnames(rv$markers_raw)) {

    cat("\n-- PROCESS MARKERS --\n")

    m <- rv$markers_raw

    cat("Markers dim:", dim(m), "\n")

    long <- tryCatch({
      tidyr::pivot_longer(
        m,
        cols = -MARKER,
        names_to = "NAZWA_REGENERANTA",
        values_to = "value"
      )
    }, error=function(e){
      cat("❌ pivot_longer FAILED\n")
      return(NULL)
    })

    if(!is.null(long)){

      long$NAZWA_REGENERANTA <- gsub("[AK]$", "", long$NAZWA_REGENERANTA)

      wide <- tidyr::pivot_wider(
        long,
        names_from = MARKER,
        values_from = value,
        values_fn = mean
      )

      cat("Wide markers dim:", dim(wide), "\n")

      if ("NAZWA_REGENERANTA" %in% colnames(wide)) {

        df <- merge(df, wide,
                    by = "NAZWA_REGENERANTA", all.x = TRUE)

        cat("After markers merge dim:", dim(df), "\n")
      }
    }
  }

  rv$data_full <- df

  cat("===== STEP 1 DONE =====\n")

})

# =========================================================
# 🔥 STEP 2: CLEAN + SCALANIE .x/.y (KLUCZOWE)
# =========================================================

observe({

  req(rv$data_full)

  cat("\n===== STEP 2: CLEAN =====\n")

  df <- rv$data_full

  cols <- colnames(df)

  # 🔥 SCALANIE .x / .y
  base_names <- unique(gsub("\\.(x|y)$", "", cols))

  for (name in base_names) {

    x_col <- paste0(name, ".x")
    y_col <- paste0(name, ".y")

    if (x_col %in% cols) {
      df[[name]] <- df[[x_col]]
    } else if (y_col %in% cols) {
      df[[name]] <- df[[y_col]]
    }
  }

  # usuń .x .y
  df <- df[, !grepl("\\.(x|y)$", colnames(df)), drop = FALSE]

  # usuń puste
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # numeric
  df[] <- lapply(df, function(x){
    if (is.character(x)) {
      x <- gsub(",", ".", x)
      num <- suppressWarnings(as.numeric(x))
      if(sum(!is.na(num)) > 0) return(num)
      return(x)
    }
    x
  })

  cat("FINAL COLS AFTER CLEAN:\n")
  print(colnames(df))

  rv$data_clean <- df

  cat("===== STEP 2 DONE =====\n")

})

# =========================================================
# 🔥 STEP 3: BUILD Y (FINAL FIX)
# =========================================================

observeEvent(
  list(input$target_vars_max, input$target_vars_min, rv$data_clean),
{

  req(rv$data_clean)

  cat("\n===== STEP 3: BUILD Y =====\n")

  df <- rv$data_clean

  all_cols <- colnames(df)

  cat("AVAILABLE COLS:\n")
  print(all_cols)

  targets_raw <- unique(c(input$target_vars_max, input$target_vars_min))

  cat("INPUT TARGETS:\n")
  print(targets_raw)

  # 🔥 MATCH PO CZĘŚCI NAZWY
  targets_real <- sapply(targets_raw, function(t){
    match <- all_cols[grepl(paste0("^", t, "$"), all_cols)]
    if(length(match)==0){
      match <- all_cols[grepl(t, all_cols)][1]
    }
    return(match)
  })

  targets_real <- unique(targets_real)
  targets_real <- targets_real[!is.na(targets_real)]

  cat("MAPPED TARGETS:\n")
  print(targets_real)

  if(length(targets_real)==0){
    rv$y <- NULL
    cat("❌ Y NULL\n")
    return()
  }

  y_df <- df[, targets_real, drop=FALSE]

  y_df[] <- lapply(y_df, function(x) suppressWarnings(as.numeric(x)))

  keep <- rowSums(!is.na(y_df)) > 0
  y_df <- y_df[keep,,drop=FALSE]

  if(nrow(y_df)==0){
    rv$y <- NULL
    cat("❌ Y NULL (all NA)\n")
    return()
  }

  rv$y <- as.matrix(y_df)

  cat("✅ FINAL Y:", dim(rv$y), "\n")

})

# =========================================================
# 🔥 STEP 4: BUILD X (FINAL FIX)
# =========================================================

observeEvent(
  list(input$condition_vars, rv$data_clean),
{

  req(rv$data_clean)

  cat("\n===== STEP 4: BUILD X =====\n")

  df <- rv$data_clean
  all_cols <- colnames(df)

  x_raw <- input$condition_vars

  cat("INPUT X:\n")
  print(x_raw)

  x_real <- sapply(x_raw, function(t){
    match <- all_cols[grepl(paste0("^", t, "$"), all_cols)]
    if(length(match)==0){
      match <- all_cols[grepl(t, all_cols)][1]
    }
    return(match)
  })

  x_real <- unique(x_real)
  x_real <- x_real[!is.na(x_real)]

  cat("MAPPED X:\n")
  print(x_real)

  if(length(x_real)==0){
    rv$X <- NULL
    cat("❌ X NULL\n")
    return()
  }

  X <- df[, x_real, drop=FALSE]

  X[] <- lapply(X, function(x) suppressWarnings(as.numeric(x)))

  keep <- rowSums(!is.na(X)) > 0
  X <- X[keep,,drop=FALSE]

  if(nrow(X)==0){
    rv$X <- NULL
    cat("❌ X NULL (all NA)\n")
    return()
  }

  rv$X <- as.matrix(X)

  cat("✅ FINAL X:", dim(rv$X), "\n")

})

# =========================================================
# 🔥 STEP 5: CONFIG
# =========================================================

observe({

  vars <- c(input$target_vars_max, input$target_vars_min)

  if(length(vars)==0){
    rv$optimization_config <- NULL
    return()
  }

  rv$optimization_config <- data.frame(
    variable = vars,
    direction = c(rep("max", length(input$target_vars_max)),
                  rep("min", length(input$target_vars_min))),
    weight = 1
  )

})

# =========================================================
# 🔥 FINAL DEBUG
# =========================================================

observe({

  cat("\n================ FINAL STATE ================\n")

  if(!is.null(rv$X)) cat("X:", dim(rv$X), "\n") else cat("X: NULL\n")
  if(!is.null(rv$y)) cat("Y:", dim(rv$y), "\n") else cat("Y: NULL\n")

  print(rv$optimization_config)

  cat("============================================\n")

})