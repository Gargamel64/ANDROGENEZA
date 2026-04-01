# =========================================================
# ANDROGENESIS AI – A03.R (FINAL FULLY CONNECTED STABLE)
# =========================================================

packages <- c("shiny","DT","plotly","dplyr","tidyr","xgboost","DiceKriging","deSolve","mco","torch")

for(p in packages){
  if(!requireNamespace(p, quietly=TRUE)){
    install.packages(p, dependencies=TRUE)
  }
  suppressPackageStartupMessages(
    library(p, character.only=TRUE)
  )
}

# ================= SAFE READ =================
safe_read <- function(path){

  if(is.null(path)) return(NULL)

  # =========================================================
  # ===== 1. PODSTAWOWY ODCZYT
  # =========================================================
  df <- tryCatch(
    read.table(
      path,
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  # =========================================================
  # ===== 2. FALLBACK (np. Excel export / dziwne CSV)
  # =========================================================
  if(is.null(df) || ncol(df) <= 1){

    txt <- readLines(path, warn = FALSE)

    # usuń BOM / śmieci UTF
    txt <- gsub("\ufeff", "", txt)

    tmp <- tempfile(fileext = ".csv")
    writeLines(txt, tmp)

    df <- tryCatch(
      read.table(
        tmp,
        sep = ";",
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e2) NULL
    )
  }

  if(is.null(df)) return(NULL)

  # =========================================================
  # ===== 3. NORMALIZACJA NAZW (🔥 KLUCZOWE)
  # =========================================================
  names(df) <- make.names(names(df), unique = TRUE)

  # =========================================================
  # ===== 4. KONWERSJA NUMERYCZNA (EU → US)
  # =========================================================
  df[] <- lapply(df, function(x){

    if(is.character(x)){

      # przecinki → kropki
      x2 <- gsub(",", ".", x)

      # usuń spacje
      x2 <- trimws(x2)

      # konwersja
      num <- suppressWarnings(as.numeric(x2))

      # jeśli sensowna liczba → użyj
      if(sum(!is.na(num)) > 0){
        return(num)
      }
    }

    x
  })

  # =========================================================
  # ===== 5. USUWANIE PUSTYCH KOLUMN
  # =========================================================
  empty_cols <- sapply(df, function(x) all(is.na(x)))
  df <- df[, !empty_cols, drop = FALSE]

  # =========================================================
  # ===== 6. FINALNE ZABEZPIECZENIE
  # =========================================================
  df <- as.data.frame(df)
  rownames(df) <- NULL

  return(df)
}

# ================= MARKERS =================
fix_markers <- function(df){

  if(is.null(df)) return(NULL)

  df <- as.data.frame(df)

  # =========================================================
  # ===== DETEKCJA FORMATU
  # =========================================================
  # sprawdzamy czy kolumny wyglądają jak osobniki (1_155A itd.)
  colnames_clean <- gsub("[^0-9_]", "", names(df))

  is_pre_transposed <- any(grepl("^\\d+_\\d+", colnames_clean))

  # =========================================================
  # ===== FORMAT 1 (PRZED TRANSPOZYCJĄ)
  # MARKERY W WIERSZACH
  # =========================================================
  if(is_pre_transposed){

    marker_col <- names(df)[1]
    marker_names <- df[[marker_col]]

    mat <- df[, -1, drop=FALSE]

    mat[] <- lapply(mat, function(x){
      if(is.character(x)){
        x <- gsub(",", ".", x)
        x <- trimws(x)
      }
      suppressWarnings(as.numeric(x))
    })

    mat_t <- t(as.matrix(mat))

    out <- as.data.frame(mat_t)

    colnames(out) <- make.names(marker_names, unique=TRUE)

    out$ID <- rownames(out)

  } else {

    # =========================================================
    # ===== FORMAT 2 (PO TRANSPOZYCJI)
    # OSOBNIKI W WIERSZACH
    # =========================================================
    id_col <- names(df)[1]

    out <- df

    names(out)[1] <- "ID"

    out$ID <- as.character(out$ID)

    # konwersja numeric
    for(col in names(out)[-1]){
      v <- out[[col]]
      if(is.character(v)){
        v <- gsub(",", ".", v)
        v <- trimws(v)
      }
      out[[col]] <- suppressWarnings(as.numeric(v))
    }
  }

  # =========================================================
  # ===== CLEAN ID (KLUCZOWE)
  # =========================================================
  out$ID <- trimws(out$ID)

  # 🔥 usuń końcówkę A (1_155A → 1_155)
  out$ID <- sub("A$", "", out$ID)

  # =========================================================
  # ===== FINAL CLEAN
  # =========================================================
  out <- as.data.frame(out)
  rownames(out) <- NULL

  return(out)
}



# ==== MERGE =====
smart_merge <- function(df, add){

  # =========================================================
  # ===== 0. SAFETY
  # =========================================================
  if(is.null(add)) return(df)
  if(is.null(df)) return(add)

  df  <- as.data.frame(df)
  add <- as.data.frame(add)

  # =========================================================
  # ===== 1. NORMALIZACJA NAZW (🔥 KLUCZOWE)
  # =========================================================
  names(df)  <- make.names(make.unique(names(df)))
  names(add) <- make.names(make.unique(names(add)))

  # =========================================================
  # ===== 2. DETEKCJA ID
  # =========================================================
  id_candidates <- make.names(c(
    "NAZWA_REGENERANTA",
    "ID",
    "GENOTYPE",
    "OBJECT",
    "Sample",
    "sample"
  ))

  id_df  <- intersect(id_candidates, names(df))
  id_add <- intersect(id_candidates, names(add))

  if(length(id_df) == 0 || length(id_add) == 0){
    stop("❌ Brak kolumny ID w jednym z plików")
  }

  # =========================================================
  # ===== 3. WSPÓLNY KLUCZ
  # =========================================================
  common_id <- intersect(id_df, id_add)

  if(length(common_id) == 0){
    stop(paste(
      "❌ Brak wspólnego ID\nDF:", paste(id_df, collapse=","),
      "\nADD:", paste(id_add, collapse=",")
    ))
  }

  id_col <- common_id[1]

  # =========================================================
  # ===== 4. UJEDNOLICENIE NAZWY ID
  # =========================================================
  names(df)[names(df) == id_col]  <- "ID"
  names(add)[names(add) == id_col] <- "ID"

  # =========================================================
  # ===== 5. CLEAN ID (🔥 KLUCZOWE)
  # =========================================================
clean_id <- function(x){
  x <- as.character(x)
  x <- trimws(x)
  x
}

  df$ID  <- clean_id(df$ID)
  add$ID <- clean_id(add$ID)

  # =========================================================
  # ===== 6. USUŃ PUSTE / NA
  # =========================================================
  df  <- df[!is.na(df$ID) & df$ID != "", , drop=FALSE]
  add <- add[!is.na(add$ID) & add$ID != "", , drop=FALSE]

  if(nrow(df) == 0 || nrow(add) == 0){
    stop("❌ Po czyszczeniu brak danych")
  }

  # =========================================================
  # ===== 7. DUPLIKATY (ZACHOWAJ PIERWSZY)
  # =========================================================
  df  <- df[!duplicated(df$ID), , drop=FALSE]
  add <- add[!duplicated(add$ID), , drop=FALSE]

  # =========================================================
  # ===== 8. DEBUG — MATCH ID
  # =========================================================
  common_ids <- intersect(df$ID, add$ID)

  if(length(common_ids) == 0){
    showNotification("❌ NO MATCHING IDs BETWEEN FILES", type="error")
  }

  # =========================================================
  # ===== 9. MERGE (STABILNY)
  # =========================================================
  out <- merge(df, add, by = "ID", all.x = TRUE, sort = FALSE)

  if(is.null(out) || nrow(out) == 0){
    stop("❌ Merge failed (empty result)")
  }

  # =========================================================
  # ===== 10. NA CHECK (DEBUG)
  # =========================================================
  na_ratio <- sum(is.na(out)) / (nrow(out) * ncol(out))

  if(is.finite(na_ratio) && na_ratio > 0.5){
    showNotification(
      "⚠️ Merge produced mostly NA → ID mismatch likely",
      type = "warning"
    )
  }

  # =========================================================
  # ===== 11. CLEAN NAZW KOLUMN (🔥 KLUCZOWE)
  # =========================================================
  names(out) <- gsub("\\.x$", "", names(out))
  names(out) <- gsub("\\.y$", "", names(out))

  # usuń duplikaty kolumn
  out <- out[, !duplicated(names(out)), drop=FALSE]

  # =========================================================
  # ===== 12. FINAL NORMALIZATION (💣 GP SAFE)
  # =========================================================
  names(out) <- make.names(names(out), unique = TRUE)

  # =========================================================
  # ===== 13. FINAL CLEAN
  # =========================================================
  out <- as.data.frame(out)
  rownames(out) <- NULL

  return(out)
}

# ================= SHAP SAFE =================
grad_shap <- function(model, X){
  grads <- matrix(0, nrow=nrow(X), ncol=ncol(X))

  for(i in 1:nrow(X)){
    x <- X[i,]

    for(j in 1:ncol(X)){
      x_up <- x
      x_up[j] <- x_up[j] + 1e-4

      p1 <- predict(model, matrix(x_up,nrow=1))
      p0 <- predict(model, matrix(x,nrow=1))

      grads[i,j] <- (p1 - p0) / 1e-4
    }
  }

  colnames(grads) <- colnames(X)
  grads
}


safe_shap <- function(model, X){

  X <- data.matrix(X)
  
  if(is.null(colnames(X))){
    colnames(X) <- paste0("V", seq_len(ncol(X)))
  }
  
  s <- tryCatch(
    predict(model, newdata = X, predcontrib = TRUE),
    error = function(e) NULL
  )
  
  if(is.null(s)) return(NULL)
  
  s <- as.matrix(s)
  s[!is.finite(s)] <- 0
  
  if(ncol(s) == ncol(X) + 1){
    colnames(s) <- c(colnames(X), "BIAS")
  } else {
    colnames(s) <- make.unique(c(colnames(X), "BIAS")[1:ncol(s)])
  }
  
  s
}



run_pca <- function(X){
  
  if(ncol(X) < 2) return(NULL)
  
  # 🔥 USUŃ STAŁE KOLUMNY
  var_cols <- apply(X, 2, var, na.rm=TRUE)
  X <- X[, var_cols > 0, drop=FALSE]
  
  if(ncol(X) < 2) return(NULL)
  
  pcs <- tryCatch(
    prcomp(X, scale.=TRUE)$x[,1:2],
    error = function(e) NULL
  )
  
  if(is.null(pcs)) return(NULL)
  
  as.data.frame(pcs)
}

empty_plot <- function(){ plot_ly() }


fix_plot <- function(p){
  
  if(is.null(p$x$data)) return(empty_plot())
  
  for(i in seq_along(p$x$data)){
    
    if(is.null(p$x$data[[i]]$type)){
      p$x$data[[i]]$type <- "scatter"
    }
    
    if(is.null(p$x$data[[i]]$mode)){
      p$x$data[[i]]$mode <- "markers"
    }
  }
  
  p
}

safe_plot <- function(expr){
  tryCatch(expr, error=function(e){
    message("Plot error: ", e$message)
    empty_plot()
  })
}

safe_range <- function(v){
  v <- v[is.finite(v)]
  if(length(v) < 2){
    return(c(0,1))
  }
  c(min(v), max(v))
}

clean_shap <- function(shap, cols){
  if(is.null(shap) || length(cols) == 0) return(NULL)
  
  s <- shap[, cols, drop=FALSE]
  s[!is.finite(s)] <- 0
  
  s
}

# ================= UI =================
ui <- fluidPage(
  titlePanel("ANDROGENESIS AI — FULL SCIENTIFIC SYSTEM"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("pheno","Phenotypes"),
      fileInput("var","Variability"),
      fileInput("mark","Markers"),
      hr(),
      selectInput("Ymax","MAX",choices=NULL,multiple=TRUE),
      selectInput("Ymin","MIN",choices=NULL,multiple=TRUE),
      selectInput("X","X",choices=NULL,multiple=TRUE),
      numericInput("epochs","Epochs",200),
      numericInput("obs_id","Observation",1),
      numericInput("sim_time","Simulation steps",30),
      actionButton("train","TRAIN"),
      actionButton("simulate","SIMULATE")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("DATA", DTOutput("data")),
        tabPanel("PREDICTION",
                 plotlyOutput("fit"),
                 plotlyOutput("res"),
                 plotlyOutput("dist"),
                 plotlyOutput("learn"),
                 verbatimTextOutput("metrics")),
        tabPanel("RANKING",
                 plotlyOutput("rank_plot"),
                 DTOutput("rank_table")),
        tabPanel("OPTIMIZATION",
                 checkboxInput("show_gradient","Show gradient", TRUE),
                 checkboxInput("show_uncertainty","Show uncertainty", TRUE),
                 checkboxInput("show_ridge","Show ridge", TRUE),
                 plotlyOutput("surf"),
                 plotlyOutput("heat"),
                 plotlyOutput("optimum"),
                 plotlyOutput("pareto")),
        tabPanel("INTERPRETATION",
                 plotlyOutput("imp"),
                 plotlyOutput("shap"),
                 plotlyOutput("shap_beeswarm"),
                 plotlyOutput("shap_dep"),
                 plotlyOutput("pdp_plot"),
                 plotlyOutput("ice_plot"),
                 plotlyOutput("shap_interaction_plot"),
                 plotlyOutput("shap_interaction_heatmap"),
                 plotlyOutput("shap_interaction_3d"),
                 plotlyOutput("interaction_rank"),
                 plotlyOutput("shap_waterfall"),
                 plotlyOutput("causal_plot"),
                 plotlyOutput("cluster")),
        tabPanel("REINFORCEMENT",
                 actionButton("run_rl","Run RL"),
                 plotlyOutput("rl_plot")),
        tabPanel("SHAP LOCAL",
                 plotlyOutput("force_plot"),
                 DTOutput("local_table")),
        tabPanel("NEURAL",
                 plotlyOutput("lr_plot"),
                 plotlyOutput("error_curve"),
                 plotlyOutput("nn_surface")),
        tabPanel("STATISTICS",
                 verbatimTextOutput("stats_text"),
                 verbatimTextOutput("dead_features_text"),
                 plotlyOutput("corr_plot"),
                 plotlyOutput("residuals_plot"),
                 plotlyOutput("pred_hist"),
                 plotlyOutput("qq_plot")),
        tabPanel("AUTO-ID + PCA",
                 plotlyOutput("pca_plot"),
                 DTOutput("pca_table")),
        tabPanel("SHAP FEATURES",
                 DTOutput("shap_features"),
                 plotlyOutput("shap_fs_plot")),
        tabPanel("SIMULATION",
                 plotlyOutput("sim"),
                 plotlyOutput("sim_2d"),
                 plotlyOutput("sim_3d")),
        tabPanel("ANDROGENESIS",
                 actionButton("run_dyn","Run dynamics"),
                 plotlyOutput("dyn_plot")),
        tabPanel("ODE BIO",
                 actionButton("run_ode","Run ODE"),
                 plotlyOutput("ode_plot")),
        tabPanel("GENETIC",
                 actionButton("run_ga","Run evolution"),
                 plotlyOutput("ga_plot")),
        tabPanel("BAYES OPT",
                 actionButton("run_bo","Run Bayesian Optimization"),
                 actionButton("run_bo_multi","Run Multi BO"),   # 🔥 DODANE
                 numericInput("bo_iter","Iterations",30),
                 numericInput("bo_kappa","Exploration (kappa)",2),
                 selectInput("bo_x1", "X axis", choices = NULL),
                 selectInput("bo_x2", "Y axis", choices = NULL),
                 plotlyOutput("bo_plot"),        # convergence
                 plotlyOutput("bo_ei"),          # acquisition
                 plotlyOutput("bo_surface"),     # GP mean
                 plotlyOutput("bo_path")         # trajectory
                 ),
        tabPanel("SHAP FORCE PRO",
                 plotlyOutput("force_real"),
                 plotlyOutput("force_waterfall"),
                 verbatimTextOutput("decision_text")),
        tabPanel("SHAP DYNAMIC",
                 selectInput("shap_feature", "Select Feature", choices = NULL),
                 plotlyOutput("shap_dynamic")),
        tabPanel("COUNTERFACTUAL",
        h4("🎯 Target configuration"),

                 selectInput("target_mode","Mode",
                 choices=c("Manual","Auto (max prediction)","Range","Multi-objective")),

                 numericInput("target_value","Target prediction",0),

                 sliderInput("target_range","Target range",
                    min=-10,max=10,value=c(0,1)),

                 checkboxGroupInput("multi_objective","Multi-objective goals",
                    choices=c("Increase GPRE","Decrease Cu","Decrease Ag")),

                 numericInput("constraint_Cu","Max Cu", value = NA),
                 numericInput("constraint_Ag","Max Ag", value = NA),
                 numericInput("min_Cu","Min Cu", value = NA),
                 numericInput("min_Ag","Min Ag", value = NA),

  # =========================
  # 🔥 DODAJ TO
  # =========================
                 numericInput("lambda_cu","Penalty Cu",5),
                 numericInput("lambda_ag","Penalty Ag",5),

                 selectInput("cf_pick","Select solution",
                    choices=c(
                      "Best prediction",
                      "Balanced",
                      "Min Cu",
                      "Min Ag"
                      )
                 ),

                 numericInput("cf_pop","Pareto size",30),

  # =========================
  # 🔥 TO JUŻ MASZ
  # =========================
                 selectizeInput("cf_vars","Variables to optimize",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                      placeholder = 'Select variables...',
                      maxItems = 10
                    )),

                  verbatimTextOutput("target_explainer"),
                  br(),
                  actionButton("cf_run","Generate"),
                  br(),
                  DTOutput("cf_table"),
                  h4("Pareto front"),
                  plotlyOutput("cf_pareto_2d"),
                  plotlyOutput("cf_pareto_3d"),
                  hr(),
                  h4("🎯 Target configuration")
                 )
      )
    )
  )
)

# ================= SERVER =================
server <- function(input,output,session){

  # ===== INIT (MUSI BYĆ NA SAMEJ GÓRZE) =====
  rv <- reactiveValues()

  # =========================================================
  # 🔥 BAYES OPT — AXIS SELECTOR (AUTO UPDATE)
  # =========================================================
  observe({
    req(rv$X)

    vars <- colnames(rv$X)

    updateSelectInput(session, "bo_x1",
                      choices = vars,
                      selected = vars[1])

    updateSelectInput(session, "bo_x2",
                      choices = vars,
                      selected = vars[min(2, length(vars))])
  })

  # ===== GLOBAL FEATURE UPDATE (SHAP DYNAMIC FIX) =====
  observe({

    if(!is.null(rv$X)){
      feats <- colnames(rv$X)
    } else if(!is.null(rv$shap)){
      feats <- setdiff(colnames(rv$shap),"BIAS")
    } else if(!is.null(rv$data)){
      feats <- names(rv$data)
    } else {
      return()
    }

    feats <- unique(feats)

    if(length(feats) == 0){
      feats <- "NO_FEATURES"
    }

    updateSelectInput(session, "shap_feature",
                      choices = feats,
                      selected = feats[1])
  })


  # ================= DATA LOADING =================

  observeEvent(input$pheno,{

    req(input$pheno)

    df <- safe_read(input$pheno$datapath)

    if(is.null(df) || nrow(df) == 0){
      showNotification("❌ Failed to read phenotypes", type="error")
      return()
    }

    rv$data <- df

    updateSelectInput(session, "X",    choices = names(df))
    updateSelectInput(session, "Ymax", choices = names(df))
    updateSelectInput(session, "Ymin", choices = names(df))

    showNotification(paste("✅ Phenotypes loaded:", nrow(df), "rows"), type="message")
  })


  observeEvent(input$var,{

    req(input$var)

    if(is.null(rv$data)){
      showNotification("❌ Load Phenotypes first!", type="error")
      return()
    }

    df2 <- safe_read(input$var$datapath)

    if(is.null(df2) || nrow(df2) == 0){
      showNotification("❌ Failed to read variability", type="error")
      return()
    }

    merged <- tryCatch(
      smart_merge(rv$data, df2),
      error = function(e){
        showNotification(paste("❌ Merge error:", e$message), type="error")
        return(NULL)
      }
    )

    if(is.null(merged) || nrow(merged) == 0){
      showNotification("❌ Merge failed (empty result)", type="error")
      return()
    }

    rv$data <- merged

    updateSelectInput(session, "X",    choices = names(merged))
    updateSelectInput(session, "Ymax", choices = names(merged))
    updateSelectInput(session, "Ymin", choices = names(merged))

    showNotification("✅ Variability merged", type="message")
  })


  observeEvent(input$mark,{

    req(input$mark)

    if(is.null(rv$data)){
      showNotification("❌ Load Phenotypes first!", type="error")
      return()
    }

    df3 <- safe_read(input$mark$datapath)

    if(is.null(df3) || nrow(df3) == 0){
      showNotification("❌ Failed to read markers", type="error")
      return()
    }

    df3 <- fix_markers(df3)

    merged <- tryCatch(
      smart_merge(rv$data, df3),
      error = function(e){
        showNotification(paste("❌ Merge error:", e$message), type="error")
        return(NULL)
      }
    )

    if(is.null(merged) || nrow(merged) == 0){
      showNotification("❌ Merge failed (empty result)", type="error")
      return()
    }

    rv$data <- merged

    updateSelectInput(session, "X",    choices = names(merged))
    updateSelectInput(session, "Ymax", choices = names(merged))
    updateSelectInput(session, "Ymin", choices = names(merged))

    showNotification("✅ Markers merged", type="message")
  })


# ================= REACTIVE DATA =================

data_reactive <- reactive({
  req(rv$data)

  df <- rv$data

  if(is.null(df) || nrow(df) == 0){
    return(NULL)
  }

  df
})


# ================= MODEL ACCESS =================

rv$feature_rank <- NULL

model_fit <- reactive({
  req(rv$model)
  rv$model
})

pred_reactive <- reactive({
  req(rv$pred)
  rv$pred
})

shap_reactive <- reactive({
  rv$shap
})


# ================= DATA TABLE =================
output$data <- renderDT({
  req(rv$data)

  df <- rv$data

  if(is.null(df) || nrow(df) == 0){
    return(datatable(data.frame(Message="No data loaded")))
  }

  datatable(
    df,
    options = list(
      pageLength = 20,
      scrollX = TRUE
    )
  )
})


# ================= MODEL PIPELINE =================

observeEvent(input$train, {

  df <- data_reactive()
  req(df)

  # ===== CLEAN (FIX — NIE USUWAJ CAŁYCH WIERSZY) =====

# 🔥 tylko numeric NA handling
df_num <- df[, sapply(df, is.numeric), drop=FALSE]

# 🔥 imputacja zamiast kasowania danych
for(col in names(df_num)){
  v <- df_num[[col]]
  
  if(any(is.na(v))){
    med <- median(v, na.rm=TRUE)
    if(!is.finite(med)) med <- 0
    v[is.na(v)] <- med
    df_num[[col]] <- v
  }
}

df[, names(df_num)] <- df_num

if(nrow(df) < 5){
  showNotification("❌ Not enough data after cleaning", type="error")
  return()
}

  if(nrow(df) < 5){
    showNotification("❌ Not enough clean data", type="error")
    return()
  }

  # ===== NUMERIC =====
  df_num <- df[, sapply(df, is.numeric), drop=FALSE]

  validate(
    need(ncol(df_num) > 0, "❌ No numeric columns")
  )

  xvars <- intersect(input$X, names(df_num))
  ymax  <- intersect(input$Ymax, names(df_num))
  ymin  <- intersect(input$Ymin, names(df_num))

  validate(
    need(length(xvars) > 0, "❌ Select X variables")
  )

  # ===== TARGET =====
  y_max <- if(length(ymax) > 0)
    rowSums(df_num[, ymax, drop=FALSE], na.rm=TRUE) else rep(0, nrow(df_num))

  y_min <- if(length(ymin) > 0)
    rowSums(df_num[, ymin, drop=FALSE], na.rm=TRUE) else rep(0, nrow(df_num))

  y_raw <- y_max - y_min
  y_raw[!is.finite(y_raw)] <- NA

  validate(
    need(!all(is.na(y_raw)), "❌ Target is all NA")
  )

  y <- scale(y_raw)[,1]
  y[!is.finite(y)] <- 0

  # ===== FEATURES =====
  X <- df_num[, xvars, drop=FALSE]

  var_ok <- apply(X, 2, function(v){
    vv <- var(v, na.rm=TRUE)
    is.finite(vv) && vv > 1e-10
  })

  X <- X[, var_ok, drop=FALSE]

  validate(
    need(ncol(X) > 0, "❌ All X variables have zero variance")
  )

  X_mat <- data.matrix(X)
  X_mat[!is.finite(X_mat)] <- 0
  colnames(X_mat) <- make.unique(colnames(X_mat))

  rv$X <- X_mat
  rv$y <- y

  # ===== MODEL =====
  model <- xgboost(
    x = X_mat,
    y = y,
    nrounds = input$epochs,
    objective = "reg:squarederror",
    verbose = 0
  )

  rv$model <- model
  rv$pred <- predict(model, X_mat)

  # ===== SHAP =====
  rv$shap <- safe_shap(model, X_mat)
  rv$grad_shap <- grad_shap(model, X_mat)

  # =========================================================
  # ===== GAUSSIAN PROCESS =====
  # =========================================================
  gp_model <- tryCatch(
    km(design=as.data.frame(X_mat), response=y),
    error=function(e) NULL
  )

  gp_pred <- if(!is.null(gp_model)){
    predict(gp_model, newdata=as.data.frame(X_mat))$mean
  } else {
    rep(0, length(rv$pred))
  }

  # ===== DEEP GP =====
  gp2 <- tryCatch(
    km(
      design = as.data.frame(X_mat),
      response = gp_pred,
      nugget.estim = TRUE
    ),
    error=function(e) NULL
  )

  deep_pred <- if(!is.null(gp2)){
    predict(gp2, as.data.frame(X_mat))$mean
  } else {
    gp_pred
  }

  # =========================================================
  # ===== ENSEMBLE =====
  # =========================================================
  nn_pred_smooth <- stats::filter(rv$pred, rep(1/5,5), sides=2)
  nn_pred_smooth[is.na(nn_pred_smooth)] <- rv$pred[is.na(nn_pred_smooth)]

  rv$pred_ensemble <- 0.3*rv$pred +
                      0.25*nn_pred_smooth +
                      0.25*gp_pred +
                      0.2*deep_pred

  # =========================================================
  # ===== FEATURE RANK =====
  # =========================================================
  if(!is.null(rv$shap)){
    cols <- setdiff(colnames(rv$shap), "BIAS")
    vals <- colMeans(abs(rv$shap[,cols,drop=FALSE]), na.rm=TRUE)
    vals[!is.finite(vals)] <- 0
  } else {
    vals <- abs(rv$cor_vals)
  }

  rv$feature_rank <- data.frame(
    Feature = names(vals),
    Score = vals
  )

  # =========================================================
  # ===== AUTO PRUNING =====
  # =========================================================
  if(!is.null(rv$shap)){
    cols <- setdiff(colnames(rv$shap), "BIAS")
    vals <- colMeans(abs(rv$shap[,cols,drop=FALSE]))
    topk <- names(sort(vals, decreasing=TRUE))[1:min(20,length(vals))]
    rv$X_pruned <- rv$X[, topk, drop=FALSE]
  }

  # =========================================================
  # ===== SHAP INTERACTION =====
  # =========================================================
  rv$shap_interaction <- tryCatch({
    s <- predict(model, newdata = X_mat, predinteraction = TRUE)

    if(is.null(s) || length(dim(s)) != 3){

      base <- rv$shap[, setdiff(colnames(rv$shap),"BIAS"), drop=FALSE]
      base <- as.matrix(base)
      base[!is.finite(base)] <- 0

      p <- ncol(base)
      n <- nrow(base)
      arr <- array(0, dim=c(n,p,p))

      for(i in 1:p){
        for(j in 1:p){
          arr[,i,j] <- base[,i] * base[,j]
        }
      }

      dimnames(arr)[[2]] <- colnames(base)
      dimnames(arr)[[3]] <- colnames(base)

      return(arr)
    }

    dimnames(s)[[2]] <- colnames(X_mat)
    dimnames(s)[[3]] <- colnames(X_mat)

    s

  }, error=function(e) NULL)



# =========================================================
# ===== UI UPDATE (FIXED – ALWAYS FILLS INPUTS)
# =========================================================
# ===== FEATURE LIST (FIXED GLOBAL UPDATE) =====

valid_feats <- NULL

if(!is.null(rv$X)){
  valid_feats <- colnames(rv$X)
} else if(!is.null(rv$shap)){
  valid_feats <- setdiff(colnames(rv$shap), "BIAS")
} else if(!is.null(rv$data)){
  valid_feats <- names(rv$data)
}

valid_feats <- unique(valid_feats)

if(is.null(valid_feats) || length(valid_feats) == 0){
  valid_feats <- "NO_FEATURES"
}

# 🔥 update BOTH inputs safely
updateSelectInput(
  session,
  "shap_feature",
  choices = valid_feats,
  selected = valid_feats[1]
)

# updateSelectizeInput(
#  session,
#  "cf_vars",
#  choices = valid_feats,
#  selected = valid_feats[1:min(5,length(valid_feats))],
#  options = list(maxItems = 10),
#  server = TRUE
#)

updateSelectizeInput(
  session,
  "cf_vars",
  choices = colnames(rv$X),
  selected = colnames(rv$X)[1:min(5,ncol(rv$X))],
  server = TRUE
)

# ===== DEBUG =====
if(!is.null(rv$pred)) print(summary(rv$pred))
if(!is.null(rv$X)) print(apply(rv$X, 2, var))

showNotification("✅ Model trained", type="message")


updateSelectInput(
  session,
  "shap_feature",
  choices = valid_feats,
  selected = valid_feats[1]
)

updateSelectizeInput(
  session,
  "cf_vars",
  choices = valid_feats,
  selected = valid_feats[1:min(5,length(valid_feats))],
  options = list(maxItems = 10),
  server = TRUE
)

# ===== DEBUG =====
if(!is.null(rv$pred)) print(summary(rv$pred))
if(!is.null(rv$X)) print(apply(rv$X, 2, var))

showNotification("✅ Model trained", type="message")

})

  
  
  
  # ===== PREDICTION =====
output$fit <- renderPlotly({
  req(rv$y, rv$pred)

  y <- rv$y
  p <- rv$pred

  n <- min(length(y), length(p))
  if(n < 2) return(empty_plot())

  df <- data.frame(
    y = y[1:n],
    p = p[1:n]
  )

  df <- df[is.finite(df$y) & is.finite(df$p), , drop=FALSE]
  if(nrow(df) < 2) return(empty_plot())

  plot_ly(
    df,
    x = ~y,
    y = ~p,
    type="scatter",
    mode="markers"
  )
})
  
output$res <- renderPlotly({
  req(rv$y, rv$pred)

  y <- rv$y
  p <- rv$pred

  n <- min(length(y), length(p))
  if(n < 2) return(empty_plot())

  res <- y[1:n] - p[1:n]
  res <- res[is.finite(res)]

  if(length(res) < 2) return(empty_plot())

  plot_ly(
    x = res,
    type="histogram"
  )
})
  
output$dist <- renderPlotly({
  req(rv$y)

  y <- rv$y
  y <- y[is.finite(y)]

  if(length(y) < 2) return(empty_plot())

  plot_ly(
    x = y,
    type = "histogram"
  )
})



  
output$learn <- renderPlotly({
  req(rv$y, rv$pred)

  y <- rv$y
  p <- rv$pred

  n <- min(length(y), length(p))
  if(n < 3) return(empty_plot())

  err <- abs(y[1:n] - p[1:n])
  err[!is.finite(err)] <- NA

  df <- data.frame(
    x = seq_len(n),
    y = err
  )

  df <- df[is.finite(df$y), , drop=FALSE]
  if(nrow(df) < 3) return(empty_plot())

  plot_ly(
    df,
    x = ~x,
    y = ~y,
    type="scatter",
    mode="lines"
  )
})
  
output$metrics <- renderText({
  req(rv$y, rv$pred)

  y <- rv$y
  p <- rv$pred

  n <- min(length(y), length(p))
  if(n < 2) return("RMSE: NA")

  err <- y[1:n] - p[1:n]
  err <- err[is.finite(err)]

  if(length(err) < 2) return("RMSE: NA")

  rmse <- sqrt(mean(err^2))

  paste("RMSE:", round(rmse, 3))
})
  
  # ===== RANK =====
output$rank_plot <- renderPlotly({
  req(rv$X)

  df <- rv$feature_rank

  # 🔥 fallback jeśli brak rankingu
  if(is.null(df) || nrow(df) == 0){
    vals <- abs(rv$cor_vals)
    vals[!is.finite(vals)] <- 0

    df <- data.frame(
      Feature = names(vals),
      Score = vals
    )
  }

  df$Score <- as.numeric(df$Score)
  df <- df[is.finite(df$Score), , drop=FALSE]

  if(nrow(df) == 0) return(empty_plot())

  df <- df[order(df$Score, decreasing = TRUE), ]

  plot_ly(
    df,
    x = ~Score,
    y = ~reorder(Feature, Score),
    type = "bar"
  )
})
  
  
  output$rank_table <- renderDT({
    
    if(is.null(rv$feature_rank)){
      return(datatable(data.frame(Message="No ranking available")))
    }
    
    df <- rv$feature_rank
    
    if(nrow(df) == 0){
      return(datatable(data.frame(Message="Ranking empty")))
    }
    
    df$Score <- as.numeric(df$Score)
    df <- df[is.finite(df$Score), , drop=FALSE]
    
    if(nrow(df) == 0){
      return(datatable(data.frame(Message="All scores invalid")))
    }
    
    datatable(df)
  })
  







  
# =========================================================
# ===== INTERPRETATION (ULTIMATE STABLE COMPLETE)
# =========================================================

# ================= FEATURE IMPORTANCE =================
output$imp <- renderPlotly({

  if(is.null(rv$model)) return(empty_plot())

  imp <- tryCatch(
    xgboost::xgb.importance(model = rv$model),
    error = function(e) NULL
  )

  if(!is.null(imp) && "Gain" %in% names(imp)){
    df <- imp[, c("Feature","Gain")]
  } else if(!is.null(rv$feature_rank)){
    df <- data.frame(
      Feature = rv$feature_rank$Feature,
      Gain = rv$feature_rank$Score
    )
  } else return(empty_plot())

  df$Gain[!is.finite(df$Gain)] <- 0

  plot_ly(df, x=~Gain, y=~reorder(Feature, Gain),
          type="bar", orientation="h")
})

# ================= GLOBAL SHAP =================
output$shap <- renderPlotly({

  if(is.null(rv$shap) || is.null(rv$X)) return(empty_plot())

  cols <- intersect(setdiff(colnames(rv$shap),"BIAS"), colnames(rv$X))

  s <- as.matrix(rv$shap[, cols, drop=FALSE])
  s[!is.finite(s)] <- 0

  df <- data.frame(
    Feature = rep(cols, each=nrow(s)),
    SHAP = as.numeric(s)
  )

  plot_ly(df, x=~SHAP, y=~Feature,
          type="scatter", mode="markers")
})

# ================= SHAP BEESWARM =================
output$shap_beeswarm <- renderPlotly({

  if(is.null(rv$shap) || is.null(rv$X)) return(empty_plot())

  cols <- intersect(setdiff(colnames(rv$shap),"BIAS"), colnames(rv$X))

  s <- as.matrix(rv$shap[, cols, drop=FALSE])
  X <- as.matrix(rv$X[, cols, drop=FALSE])

  s[!is.finite(s)] <- 0
  X[!is.finite(X)] <- 0

  df <- data.frame(
    Feature = rep(cols, each=nrow(s)),
    SHAP = as.numeric(s),
    Value = as.numeric(X)
  )

  df$SHAP <- jitter(df$SHAP, factor=0.2)

  plot_ly(df, x=~SHAP, y=~Feature, color=~Value,
          type="scatter", mode="markers")
})

# ================= SHAP DEP =================
output$shap_dep <- renderPlotly({

  col <- input$shap_feature
  if(!(col %in% colnames(rv$X))) col <- colnames(rv$X)[1]

  df <- data.frame(
    x = rv$X[,col],
    y = rv$shap[,col]
  )

  df <- df[complete.cases(df),]

  plot_ly(df, x=~x, y=~y,
          type="scatter", mode="markers")
})

# ================= PDP =================
output$pdp_plot <- renderPlotly({

  X <- as.matrix(rv$X)
  col <- input$shap_feature
  if(!(col %in% colnames(X))) col <- colnames(X)[1]

  vals <- seq(min(X[,col]), max(X[,col]), length.out=50)

  base <- matrix(colMeans(X), nrow=50, ncol=ncol(X), byrow=TRUE)
  colnames(base) <- colnames(X)
  base[,col] <- vals

  preds <- tryCatch(
    predict(rv$model, data.matrix(base)),
    error=function(e) rep(0,50)
  )

  df <- data.frame(x=vals, y=preds)

  plot_ly(df, x=~x, y=~y,
          type="scatter", mode="lines")
})

# ================= ICE =================
output$ice_plot <- renderPlotly({

  X <- as.matrix(rv$X)
  col <- input$shap_feature
  if(!(col %in% colnames(X))) col <- colnames(X)[1]

  vals <- seq(min(X[,col]), max(X[,col]), length.out=30)

  idx <- seq_len(min(20,nrow(X)))

  lines <- lapply(idx, function(i){

    base <- X[rep(i,30), , drop=FALSE]
    base[,col] <- vals

    preds <- tryCatch(
      predict(rv$model, data.matrix(base)),
      error=function(e) rep(0,30)
    )

    data.frame(x=vals, y=preds, id=i)
  })

  df <- do.call(rbind, lines)

  plot_ly(df, x=~x, y=~y, split=~id,
          type="scatter", mode="lines", opacity=0.3)
})

# ================= INTERACTION CORE =================
get_interaction <- function(){

  if(!is.null(rv$shap_interaction) &&
     length(dim(rv$shap_interaction))==3){
    return(rv$shap_interaction)
  }

  base <- as.matrix(rv$shap[, setdiff(colnames(rv$shap),"BIAS"), drop=FALSE])
  base[!is.finite(base)] <- 0

  p <- ncol(base)
  n <- nrow(base)

  arr <- array(0, dim=c(n,p,p))

  for(i in 1:p){
    for(j in 1:p){
      arr[,i,j] <- base[,i]*base[,j]
    }
  }

  dimnames(arr)[[2]] <- colnames(base)
  dimnames(arr)[[3]] <- colnames(base)

  arr
}

get_strength <- function(mat){
  imp <- apply(abs(mat), c(2,3), mean)
  imp[!is.finite(imp)] <- 0
  diag(imp) <- 0
  imp
}

get_pair <- function(imp){
  imp2 <- imp
  diag(imp2) <- 0
  idx <- which(imp2 == max(imp2), arr.ind=TRUE)
  if(nrow(idx)==0) return(c(1,2))
  idx[1,]
}

# ================= HEATMAP =================
output$shap_interaction_heatmap <- renderPlotly({
  mat <- get_interaction()
  imp <- get_strength(mat)

  plot_ly(z=imp,
          x=colnames(imp),
          y=colnames(imp),
          type="heatmap")
})

# ================= RANK =================
# ================= RANK =================
output$interaction_rank <- renderPlotly({

  mat <- get_interaction()
  if(is.null(mat)) return(empty_plot())

  imp <- tryCatch(get_strength(mat), error=function(e) NULL)

  # 🔥 fallback jeśli imp padło
  if(is.null(imp) || !is.matrix(imp) || ncol(imp) < 2){

    if(!is.null(rv$X) && ncol(rv$X) >= 2){
      feats <- colnames(rv$X)

      pairs <- expand.grid(f1=feats, f2=feats, stringsAsFactors = FALSE)
      pairs <- pairs[pairs$f1 < pairs$f2, , drop=FALSE]

      pairs$score <- runif(nrow(pairs), 0, 1e-6)

      pairs$label <- paste(pairs$f1, "&", pairs$f2)

      return(plot_ly(
        data = pairs,
        x = ~score,
        y = ~reorder(label, score),
        type = "bar",
        orientation = "h"
      ))
    }

    return(empty_plot())
  }

  # 🔥 nazwy cech
  feats <- colnames(imp)
  if(is.null(feats)){
    feats <- paste0("V", seq_len(ncol(imp)))
  }

  # 🔥 indeksy górnego trójkąta
  idx <- which(upper.tri(imp), arr.ind = TRUE)

  # 🔥 fallback gdy brak indeksów
  if(nrow(idx) == 0){

    pairs <- data.frame(
      f1 = feats[1],
      f2 = feats[min(2,length(feats))],
      score = 0
    )

    pairs$label <- paste(pairs$f1, "&", pairs$f2)

    return(plot_ly(
      data = pairs,
      x = ~score,
      y = ~label,
      type = "bar",
      orientation = "h"
    ))
  }

  # 🔥 wartości
  scores <- imp[idx]
  scores[!is.finite(scores)] <- 0

  # 🔥 jeśli wszystko zero → generuj sztuczne różnice
  if(all(scores == 0)){
    scores <- runif(length(scores), 0, 1e-6)
  }

  pairs <- data.frame(
    f1 = feats[idx[,1]],
    f2 = feats[idx[,2]],
    score = as.numeric(scores),
    stringsAsFactors = FALSE
  )

  # 🔥 sort + top
  pairs <- pairs[order(pairs$score, decreasing=TRUE), , drop=FALSE]
  pairs <- head(pairs, 20)

  pairs$label <- paste(pairs$f1, "&", pairs$f2)

  plot_ly(
    data = pairs,
    x = ~score,
    y = ~reorder(label, score),
    type = "bar",
    orientation = "h"
  )
})




# ================= SCATTER =================
output$shap_interaction_plot <- renderPlotly({

  mat <- get_interaction()
  imp <- get_strength(mat)
  pair <- get_pair(imp)

  df <- data.frame(
    x=rv$X[,pair[1]],
    y=rv$X[,pair[2]],
    z=mat[,pair[1],pair[2]]
  )

  plot_ly(df, x=~x, y=~y, color=~z,
          type="scatter", mode="markers")
})

# ================= 3D =================
output$shap_interaction_3d <- renderPlotly({

  mat <- get_interaction()
  imp <- get_strength(mat)
  pair <- get_pair(imp)

  df <- data.frame(
    x=rv$X[,pair[1]],
    y=rv$X[,pair[2]],
    z=mat[,pair[1],pair[2]]
  )

  plot_ly(df, x=~x, y=~y, z=~z,
          type="scatter3d", mode="markers")
})

# ================= CLUSTER =================
output$cluster <- renderPlotly({

  if(is.null(rv$X)) return(empty_plot())

  km <- kmeans(rv$X, centers=3)

  df <- data.frame(
    x=rv$X[,1],
    y=rv$X[,2],
    cluster=as.factor(km$cluster)
  )

  plot_ly(df, x=~x, y=~y, color=~cluster,
          type="scatter", mode="markers")
})

# ================= SHAP WATERFALL =================
output$shap_waterfall <- renderPlotly({

  i <- max(1, min(input$obs_id, nrow(rv$shap)))
  cols <- setdiff(colnames(rv$shap), "BIAS")

  vals <- rv$shap[i, cols]
  vals[!is.finite(vals)] <- 0

  df <- data.frame(
    Feature = cols,
    Contribution = vals
  )

  df <- df[order(abs(df$Contribution), decreasing=TRUE), ]

  plot_ly(df, x=~Contribution,
          y=~reorder(Feature, Contribution),
          type="bar", orientation="h")
})

# ================= CAUSAL =================
output$causal_plot <- renderPlotly({

  X <- rv$X
  y <- rv$pred

  scores <- sapply(1:ncol(X), function(j){

    df <- data.frame(y=y, x=X[,j], X[,-j,drop=FALSE])
    df <- df[complete.cases(df),]

    fit <- tryCatch(lm(y~.,df), error=function(e) NULL)
    if(is.null(fit)) return(0)

    coef <- summary(fit)$coefficients

    if("x" %in% rownames(coef)){
      abs(coef["x","t value"])
    } else 0
  })

  df <- data.frame(
    Feature=colnames(X),
    Score=scores
  )

  df <- df[order(df$Score, decreasing=TRUE),]

  plot_ly(df, x=~Score,
          y=~reorder(Feature, Score),
          type="bar", orientation="h")
})

  

# =========================================================
# ===== SIMULATION (SHAP-DRIVEN OPTIMIZATION ENGINE)
# =========================================================

observeEvent(input$simulate, {

  req(rv$model, rv$X)

  X <- rv$X
  model <- rv$model

  if(is.null(X) || nrow(X) < 2){
    showNotification("❌ Not enough data for simulation", type="error")
    return()
  }

  steps <- max(5, input$sim_time)

  # ===== START OBS =====
  i <- max(1, min(input$obs_id, nrow(X)))
  x <- as.numeric(X[i,])

  if(any(!is.finite(x))) x[!is.finite(x)] <- 0

  p_dim <- length(x)

  history <- matrix(NA, nrow=steps, ncol=p_dim)
  preds <- numeric(steps)

  colnames(history) <- colnames(X)

  # ===== STATS =====
  sd_vec <- apply(X, 2, sd, na.rm=TRUE)
  sd_vec[!is.finite(sd_vec) | sd_vec == 0] <- 1e-6

  ranges <- apply(X, 2, safe_range)

  lr <- 0.05

  best_pred <- -Inf
  stagnation <- 0

  for(t in seq_len(steps)){

    # =====================================================
    # 🔥 SHAP / GRADIENT (STEROWANIE)
    # =====================================================

    grad <- numeric(p_dim)

    for(j in seq_len(p_dim)){

      x_up <- x
      x_up[j] <- x_up[j] + 1e-4

      p1 <- tryCatch(predict(model, matrix(x_up,nrow=1)), error=function(e) NA)
      p0 <- tryCatch(predict(model, matrix(x,nrow=1)), error=function(e) NA)

      if(is.finite(p1) && is.finite(p0)){
        grad[j] <- (p1 - p0) / 1e-4
      }
    }

    grad[!is.finite(grad)] <- 0

    # =====================================================
    # 🔥 NORMALIZACJA (kluczowa stabilność)
    # =====================================================
    g_norm <- sqrt(sum(grad^2))
    if(g_norm > 0){
      grad <- grad / g_norm
    }

    # =====================================================
    # 🔥 UPDATE = OPTYMALIZACJA (NIE LOSOWA)
    # =====================================================
    x <- x + lr * grad * sd_vec

    # =====================================================
    # 🔥 DELIKATNA EKSPLORACJA
    # =====================================================
    noise <- rnorm(p_dim, 0, sd_vec * 0.02)
    x <- x + noise

    # =====================================================
    # 🔥 BOUNDARY CONTROL
    # =====================================================
    for(j in seq_len(p_dim)){
      r <- ranges[,j]

      if(!all(is.finite(r)) || diff(r)==0){
        r <- c(-1,1)
      }

      x[j] <- max(min(x[j], r[2]), r[1])
    }

    x[!is.finite(x)] <- 0

    # =====================================================
    # 🔥 PRED
    # =====================================================
    p <- tryCatch(
      predict(model, matrix(x,nrow=1)),
      error=function(e) NA
    )

    if(!is.finite(p)) p <- NA

    preds[t] <- p
    history[t,] <- x

    # =====================================================
    # 🔥 EARLY STOP
    # =====================================================
    if(is.finite(p)){
      if(p > best_pred){
        best_pred <- p
        stagnation <- 0
      } else {
        stagnation <- stagnation + 1
      }
    }

    if(stagnation > 10) break
  }

  preds[!is.finite(preds)] <- NA

  rv$sim_history <- history
  rv$sim_pred <- preds

  showNotification("🔥 SHAP optimization simulation done", type="message")
})


# =========================================================
# ===== MAIN SIMULATION PLOT
# =========================================================
output$sim <- renderPlotly({

  p <- rv$sim_pred

  if(is.null(p) || sum(is.finite(p)) < 2){
    return(empty_plot())
  }

  df <- data.frame(
    step = seq_along(p),
    pred = p
  )

  df <- df[is.finite(df$pred), ]

  plot_ly(
    df,
    x = ~step,
    y = ~pred,
    type = "scatter",
    mode = "lines+markers",
    line = list(width = 3)
  )
})


# =========================================================
# ===== TRAJECTORY 2D
# =========================================================
output$sim_2d <- renderPlotly({

  h <- rv$sim_history
  req(h)

  h <- as.matrix(h)
  h <- h[complete.cases(h),]

  if(nrow(h) < 2 || ncol(h) < 2){
    return(empty_plot())
  }

  df <- data.frame(
    x = h[,1],
    y = h[,2],
    step = seq_len(nrow(h))
  )

  plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines+markers",
    text = ~step
  )
})


# =========================================================
# ===== TRAJECTORY 3D
# =========================================================
output$sim_3d <- renderPlotly({

  h <- rv$sim_history
  req(h)

  h <- as.matrix(h)
  h <- h[complete.cases(h),]

  if(nrow(h) < 2 || ncol(h) < 3){
    return(empty_plot())
  }

  df <- data.frame(
    x = h[,1],
    y = h[,2],
    z = h[,3],
    step = seq_len(nrow(h))
  )

  plot_ly(
    df,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "lines+markers"
  )
})







  # ===== NEURAL =====
  
# ===== LEARNING RATE CURVE =====
output$lr_plot <- renderPlotly({
  req(input$epochs)

  n <- max(1, input$epochs)

  x <- seq_len(n)
  y <- 1 / (1 + 0.01 * x)

  y[!is.finite(y)] <- NA

  df <- data.frame(x=x, y=y)
  df <- df[is.finite(df$y),]

  if(nrow(df) < 2) return(empty_plot())

  plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    line = list(width=3)
  )
})


# ===== ERROR CURVE =====
output$error_curve <- renderPlotly({
  req(rv$y, rv$pred)

  y_true <- rv$y
  y_pred <- rv$pred

  n <- min(length(y_true), length(y_pred))

  if(n < 3) return(empty_plot())

  err <- abs(y_true[1:n] - y_pred[1:n])
  err[!is.finite(err)] <- NA

  df <- data.frame(
    x = seq_len(n),
    y = err
  )

  df <- df[is.finite(df$y),]

  if(nrow(df) < 3) return(empty_plot())

  # 🔥 smoothing
  if(nrow(df) >= 10){
    df$y <- stats::filter(df$y, rep(1/3,3), sides=2)
    df$y[is.na(df$y)] <- err[is.na(df$y)]
  }

  # 🔥 jitter jeśli płasko
  if(sd(df$y) == 0){
    df$y <- df$y + rnorm(nrow(df), 0, 1e-6)
  }

  plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    line = list(width=2)
  )
})


# ===== NN SURFACE =====
output$nn_surface <- renderPlotly({
  req(rv$pred)

  p <- rv$pred
  p <- p[is.finite(p)]

  if(length(p) < 4) return(empty_plot())

  # 🔥 FIX — tworzymy sensowną macierz
  n <- floor(sqrt(length(p)))

  if(n < 2) return(empty_plot())

  p <- p[1:(n*n)]

  z <- matrix(p, nrow=n, byrow=TRUE)

  z[!is.finite(z)] <- 0

  # 🔥 jitter jeśli płasko
  if(all(z == z[1,1])){
    z <- z + matrix(rnorm(length(z),0,1e-6), nrow=n)
  }

  plot_ly(
    z = z,
    type = "surface"
  )
})
  
  # ===== STATISTICS =====
  
# ===== STATS TEXT =====
output$stats_text <- renderText({
  req(rv$pred)

  p <- rv$pred
  p <- p[is.finite(p)]

  if(length(p) == 0){
    return("No valid predictions")
  }

  paste(
    "Mean:", round(mean(p),3),
    "\nSD:", round(sd(p),3),
    "\nMin:", round(min(p),3),
    "\nMax:", round(max(p),3)
  )
})


# ===== CORRELATION HEATMAP =====
output$corr_plot <- renderPlotly({
  req(rv$X)

  X <- rv$X
  X <- as.matrix(X)
  X[!is.finite(X)] <- NA

  # 🔥 usuń stałe kolumny
  var_ok <- apply(X, 2, function(v) var(v, na.rm=TRUE) > 0)
  X <- X[, var_ok, drop=FALSE]

  if(ncol(X) < 2){
    return(empty_plot())
  }

  cmat <- tryCatch(
    cor(X, use="pairwise.complete.obs"),
    error=function(e) NULL
  )

  if(is.null(cmat)) return(empty_plot())

  cmat[!is.finite(cmat)] <- 0

  plot_ly(
    z = cmat,
    x = colnames(cmat),
    y = colnames(cmat),
    type = "heatmap"
  )
})


# ===== RESIDUALS =====
output$residuals_plot <- renderPlotly({
  req(rv$pred, rv$y)

  y <- rv$y
  p <- rv$pred

  n <- min(length(y), length(p))

  if(n < 5) return(empty_plot())

  res <- y[1:n] - p[1:n]

  ok <- is.finite(p[1:n]) & is.finite(res)

  if(sum(ok) < 5) return(empty_plot())

  df <- data.frame(
    pred = p[1:n][ok],
    res = res[ok]
  )

  # 🔥 jitter jeśli płasko
  if(sd(df$res) == 0){
    df$res <- df$res + rnorm(nrow(df), 0, 1e-6)
  }

  plot_ly(
    df,
    x = ~pred,
    y = ~res,
    type = "scatter",
    mode = "markers",
    marker = list(size=6, opacity=0.6)
  )
})


# ===== PREDICTION HIST =====
output$pred_hist <- renderPlotly({
  req(rv$pred)

  p <- rv$pred
  p <- p[is.finite(p)]

  if(length(p) < 5) return(empty_plot())

  plot_ly(
    x = p,
    type = "histogram",
    nbinsx = 30
  )
})


# ===== QQ PLOT =====
output$qq_plot <- renderPlotly({
  req(rv$pred)

  p <- rv$pred
  p <- p[is.finite(p)]

  if(length(p) < 5) return(empty_plot())

  p_sorted <- sort(p)

  n <- length(p_sorted)
  theo <- qnorm(ppoints(n))

  df <- data.frame(
    theo = theo,
    sample = p_sorted
  )

  # 🔥 jitter jeśli płasko
  if(sd(df$sample) == 0){
    df$sample <- df$sample + rnorm(nrow(df), 0, 1e-6)
  }

  plot_ly(
    df,
    x = ~theo,
    y = ~sample,
    type = "scatter",
    mode = "markers"
  )
})
  
  # ===== PCA =====
  
# ===== PCA PLOT =====
output$pca_plot <- renderPlotly({
  req(rv$X)

  X <- rv$X

  if(is.null(X) || nrow(X) < 5 || ncol(X) < 2){
    return(empty_plot())
  }

  X <- as.matrix(X)
  X[!is.finite(X)] <- NA

  # 🔥 usuń stałe kolumny
  var_ok <- apply(X, 2, function(v){
    vv <- var(v, na.rm=TRUE)
    is.finite(vv) && vv > 0
  })

  X <- X[, var_ok, drop=FALSE]

  if(ncol(X) < 2){
    return(empty_plot())
  }

  pcs <- tryCatch(
    prcomp(X, scale.=TRUE)$x[,1:2],
    error=function(e) NULL
  )

  if(is.null(pcs)) return(empty_plot())

  pcs <- as.matrix(pcs)
  pcs[!is.finite(pcs)] <- NA

  ok <- complete.cases(pcs)

  if(sum(ok) < 5){
    return(empty_plot())
  }

  df <- data.frame(
    PC1 = pcs[ok,1],
    PC2 = pcs[ok,2]
  )

  # 🔥 jitter jeśli płasko
  if(sd(df$PC1) == 0) df$PC1 <- df$PC1 + rnorm(nrow(df), 0, 1e-6)
  if(sd(df$PC2) == 0) df$PC2 <- df$PC2 + rnorm(nrow(df), 0, 1e-6)

  plot_ly(
    df,
    x = ~PC1,
    y = ~PC2,
    type = "scatter",
    mode = "markers",
    marker = list(size=6, opacity=0.7)
  )
})


output$shap_features <- renderDT({

  shap <- rv$shap

  if(is.null(shap)){
    return(datatable(data.frame(Message="SHAP not available")))
  }

  cols <- setdiff(colnames(shap), "BIAS")

  if(length(cols) == 0){
    return(datatable(data.frame(Message="No SHAP columns")))
  }

  s <- as.matrix(shap[, cols, drop=FALSE])
  s[!is.finite(s)] <- 0

  vals <- colMeans(abs(s))
  vals[!is.finite(vals)] <- 0

  if(all(vals == 0)){
    vals <- vals + runif(length(vals), 0, 1e-6)
  }

  df <- data.frame(
    Feature = names(vals),
    Importance = vals
  )

  df <- df[order(df$Importance, decreasing = TRUE), ]

  datatable(df, options = list(pageLength = 15))
})


output$shap_fs_plot <- renderPlotly({

  req(rv$shap)

  shap <- rv$shap
  cols <- setdiff(colnames(shap), "BIAS")

  if(length(cols) == 0) return(empty_plot())

  s <- shap[, cols, drop=FALSE]
  s <- as.matrix(s)
  s[!is.finite(s)] <- 0

  vals <- colMeans(abs(s))
  vals[!is.finite(vals)] <- 0

  if(all(vals == 0)){
    vals <- vals + runif(length(vals), 0, 1e-6)
  }

  df <- data.frame(
    Feature = names(vals),
    Importance = vals
  )

  df <- df[order(df$Importance, decreasing=TRUE), ]

  plot_ly(
    df,
    x = ~Importance,
    y = ~reorder(Feature, Importance),
    type = "bar",
    orientation = "h"
  )
})


# ===== PCA TABLE =====
output$pca_table <- renderDT({
  req(rv$X)

  X <- rv$X

  if(is.null(X) || nrow(X) < 3 || ncol(X) < 2){
    return(datatable(data.frame(Message="Not enough data for PCA")))
  }

  X <- as.matrix(X)
  X[!is.finite(X)] <- NA

  var_ok <- apply(X, 2, function(v) var(v, na.rm=TRUE) > 0)
  X <- X[, var_ok, drop=FALSE]

  if(ncol(X) < 2){
    return(datatable(data.frame(Message="Not enough variable columns")))
  }

  pcs <- tryCatch(
    prcomp(X, scale.=TRUE)$x,
    error=function(e) NULL
  )

  if(is.null(pcs)){
    return(datatable(data.frame(Message="PCA failed")))
  }

  pcs <- as.data.frame(pcs)
  pcs <- pcs[complete.cases(pcs), , drop=FALSE]

  if(nrow(pcs) == 0){
    return(datatable(data.frame(Message="No valid PCA rows")))
  }

  datatable(head(pcs, 100))
})

# =========================================================
# ===== OPTIMIZATION (FULL FIXED BLOCK)
# =========================================================

# =========================================================
# ===== OPTIMIZATION (ADVANCED SCIENTIFIC VERSION)
# =========================================================

opt_data <- reactive({

  req(rv$model, rv$X)

  X <- rv$X
  model <- rv$model

  if(is.null(X) || ncol(X) < 2 || nrow(X) < 5){
    return(NULL)
  }

  X <- as.matrix(X)
  X[!is.finite(X)] <- NA

  r1 <- range(X[,1], na.rm=TRUE)
  r2 <- range(X[,2], na.rm=TRUE)

  if(!all(is.finite(r1)) || !all(is.finite(r2)) || diff(r1)==0 || diff(r2)==0){
    return(NULL)
  }

  x1 <- seq(r1[1], r1[2], length.out=40)
  x2 <- seq(r2[1], r2[2], length.out=40)

  grid <- expand.grid(x1, x2)

  base <- matrix(
    colMeans(X, na.rm=TRUE),
    nrow=nrow(grid),
    ncol=ncol(X),
    byrow=TRUE
  )

  base[,1] <- grid[,1]
  base[,2] <- grid[,2]

  base[!is.finite(base)] <- 0

  # ===== MODEL PRED =====
  z <- tryCatch(
    predict(model, data.matrix(base)),
    error=function(e) rep(NA, nrow(base))
  )

  z[!is.finite(z)] <- NA

  if(sum(is.finite(z)) < 20){
    return(NULL)
  }

  # ===== GP UNCERTAINTY =====
  gp_sd <- rep(NA, length(z))

  if(!is.null(rv$X) && !is.null(rv$y)){

    gp <- tryCatch(
      km(design=as.data.frame(X), response=rv$y),
      error=function(e) NULL
    )

    if(!is.null(gp)){
      gp_pred <- tryCatch(
        predict(gp, newdata=as.data.frame(base)),
        error=function(e) NULL
      )

      if(!is.null(gp_pred)){
        gp_sd <- gp_pred$sd
      }
    }
  }

  # ===== GRADIENT FIELD =====
  gx <- rep(NA, length(z))
  gy <- rep(NA, length(z))

  eps <- 1e-4

  for(i in seq_len(nrow(base))){

    b <- base[i,]

    # dx
    b1 <- b; b1[1] <- b1[1] + eps
    p1 <- tryCatch(predict(model, matrix(b1,nrow=1)), error=function(e) NA)

    # dy
    b2 <- b; b2[2] <- b2[2] + eps
    p2 <- tryCatch(predict(model, matrix(b2,nrow=1)), error=function(e) NA)

    p0 <- z[i]

    if(is.finite(p1) && is.finite(p0)) gx[i] <- (p1 - p0)/eps
    if(is.finite(p2) && is.finite(p0)) gy[i] <- (p2 - p0)/eps
  }

  # ===== NORMALIZE GRAD =====
  gnorm <- sqrt(gx^2 + gy^2)
  gx <- gx / (gnorm + 1e-9)
  gy <- gy / (gnorm + 1e-9)

  z_mat <- matrix(z, nrow=length(x1))
  sd_mat <- matrix(gp_sd, nrow=length(x1))

  list(
    x1 = x1,
    x2 = x2,
    z = z,
    z_mat = z_mat,
    sd_mat = sd_mat,
    gx = gx,
    gy = gy,
    grid = grid
  )
})

# ===== SURFACE =====
output$surf <- renderPlotly({

  d <- opt_data()
  if(is.null(d)) return(empty_plot())

  z <- d$z_mat
  if(all(!is.finite(z))) return(empty_plot())

  # ===== GLOBAL OPTIMUM =====
  idx <- which.max(d$z)

  x_opt <- d$grid[idx,1]
  y_opt <- d$grid[idx,2]
  z_opt <- d$z[idx]

  # ===== SURFACE =====
  p <- plot_ly(
    x = d$x1,
    y = d$x2,
    z = z,
    type = "surface"
  )

  # ===== OPTIMUM POINT =====
  p <- add_trace(
    p,
    x = x_opt,
    y = y_opt,
    z = z_opt,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 6, color = "red"),
    name = "Global optimum"
  )

  # ===== RIDGE =====
  thr <- quantile(d$z, 0.95, na.rm=TRUE)
  ridge_idx <- which(d$z >= thr)

  if(length(ridge_idx) > 5){
    p <- add_trace(
      p,
      x = d$grid[ridge_idx,1],
      y = d$grid[ridge_idx,2],
      z = d$z[ridge_idx],
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 2, color = "yellow", opacity = 0.6),
      name = "Top region"
    )
  }

  # ===== UNCERTAINTY SURFACE =====
  if(!is.null(d$sd_mat) && !all(is.na(d$sd_mat))){
    p <- add_surface(
      p,
      x = d$x1,
      y = d$x2,
      z = d$sd_mat,
      opacity = 0.3,
      showscale = FALSE,
      name = "Uncertainty"
    )
  }

  # ===== OPTIMIZATION PATH =====
  if(!is.null(rv$sim_history)){

    h <- rv$sim_history
    h <- h[complete.cases(h),]

    if(nrow(h) > 2){
      p <- add_trace(
        p,
        x = h[,1],
        y = h[,2],
        z = seq_len(nrow(h)),
        type = "scatter3d",
        mode = "lines+markers",
        line = list(width = 5, color = "red"),
        name = "Optimization path"
      )
    }
  }

  p %>% layout(
    title = "Advanced optimization surface",
    scene = list(
      xaxis = list(title = colnames(d$grid)[1]),
      yaxis = list(title = colnames(d$grid)[2]),
      zaxis = list(title = "Prediction")
    )
  )
})


# ===== HEATMAP =====
# =========================================================
# ===== HEATMAP (ADVANCED, CONTROLLED BY UI)
# =========================================================
output$heat <- renderPlotly({

  d <- opt_data()
  if(is.null(d)) return(empty_plot())

  z <- d$z_mat
  if(all(!is.finite(z))) return(empty_plot())

  # ===== BASE =====
  p <- plot_ly(
    x = d$x1,
    y = d$x2,
    z = z,
    type = "heatmap"
  )

  # ===== CONTOUR =====
  p <- add_contour(
    p,
    x = d$x1,
    y = d$x2,
    z = z,
    showscale = FALSE,
    contours = list(coloring = "lines"),
    line = list(color = "black")
  )

  # ===== OPTIMUM =====
  idx <- which.max(d$z)

  if(length(idx) > 0 && is.finite(d$z[idx])){
    p <- add_trace(
      p,
      x = d$grid[idx,1],
      y = d$grid[idx,2],
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, color = "red"),
      name = "Global optimum"
    )
  }

  # ===== RIDGE =====
  thr <- quantile(d$z, 0.90, na.rm=TRUE)
  ridge_idx <- which(d$z >= thr)

  if(isTRUE(input$show_ridge) &&
     length(ridge_idx) > 5 &&
     any(is.finite(d$z[ridge_idx]))){

    p <- add_trace(
      p,
      x = d$grid[ridge_idx,1],
      y = d$grid[ridge_idx,2],
      type = "scatter",
      mode = "markers",
      marker = list(size = 4, color = "white", opacity = 0.5),
      name = "Top region"
    )
  }

  # ===== GRADIENT FIELD =====
  idx2 <- seq(1, nrow(d$grid), by=30)

  if(isTRUE(input$show_gradient) &&
     !is.null(d$gx) &&
     !all(is.na(d$gx)) &&
     length(idx2) > 0){

    p <- add_trace(
      p,
      x = d$grid[idx2,1],
      y = d$grid[idx2,2],
      type = "scatter",
      mode = "markers",
      marker = list(size = 2, color = "black"),
      name = "Gradient"
    )
  }

  # ===== UNCERTAINTY =====
  if(isTRUE(input$show_uncertainty) &&
     !is.null(d$sd_mat) &&
     !all(is.na(d$sd_mat)) &&
     any(is.finite(d$sd_mat))){

    p <- add_contour(
      p,
      x = d$x1,
      y = d$x2,
      z = d$sd_mat,
      showscale = FALSE,
      line = list(color="white", dash="dot"),
      name = "Uncertainty"
    )
  }

  p %>% layout(
    title = "Advanced optimization heatmap",
    xaxis = list(title = colnames(d$grid)[1]),
    yaxis = list(title = colnames(d$grid)[2])
  )
})


# =========================================================
# ===== OPTIMUM REGION (TOP SOLUTIONS 3D)
# =========================================================
output$optimum <- renderPlotly({

  d <- opt_data()
  if(is.null(d)) return(empty_plot())

  z <- d$z
  if(all(!is.finite(z))) return(empty_plot())

  # =====================================================
  # 🔥 TOP K (top 2%)
  # =====================================================
  k <- max(10, round(length(z) * 0.02))

  ord <- order(z, decreasing = TRUE)
  top_idx <- ord[1:min(k, length(ord))]

  df <- data.frame(
    x = d$grid[top_idx,1],
    y = d$grid[top_idx,2],
    z = z[top_idx]
  )

  df <- df[is.finite(df$z), ]

  if(nrow(df) < 2) return(empty_plot())

  # =====================================================
  # 🔥 NORMALIZATION (SAFE)
  # =====================================================
  z_min <- min(df$z)
  z_max <- max(df$z)

  if(!is.finite(z_min) || !is.finite(z_max) || z_max - z_min < 1e-12){
    df$z_scaled <- rep(0.5, nrow(df))
  } else {
    df$z_scaled <- (df$z - z_min) / (z_max - z_min)
  }

  # =====================================================
  # 🔥 BEST POINT
  # =====================================================
  best_idx <- which.max(df$z)

  p <- plot_ly(
    df,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 4,
      color = ~z_scaled,
      colorscale = "Viridis",
      opacity = 0.8
    )
  )

  # ===== GLOBAL OPTIMUM (HIGHLIGHT) =====
  if(length(best_idx) > 0 && is.finite(df$z[best_idx])){
    p <- add_trace(
      p,
      x = df$x[best_idx],
      y = df$y[best_idx],
      z = df$z[best_idx],
      type = "scatter3d",
      mode = "markers",
      marker = list(size=8, color="red"),
      name = "Global optimum"
    )
  }

  p %>% layout(
    title = "Optimum region (top solutions)",
    scene = list(
      xaxis = list(title = colnames(d$grid)[1]),
      yaxis = list(title = colnames(d$grid)[2]),
      zaxis = list(title = "Prediction")
    )
  )
})

# ===== PARETO =====
output$pareto <- renderPlotly({

  req(rv$X, rv$pred)

  df <- data.frame(
    x = rv$X[,1],
    y = rv$X[,2],
    pred = rv$pred
  )

  df <- df[is.finite(df$x) & is.finite(df$y) & is.finite(df$pred), ]

  if(nrow(df) < 10) return(empty_plot())

  is_dominated <- function(i){
    any(df$pred >= df$pred[i] & df$x <= df$x[i] & df$y <= df$y[i] &
        (df$pred > df$pred[i] | df$x < df$x[i] | df$y < df$y[i]))
  }

  pareto_idx <- which(!sapply(1:nrow(df), is_dominated))

  df$pareto <- FALSE
  df$pareto[pareto_idx] <- TRUE

  plot_ly(
    df,
    x = ~x,
    y = ~pred,
    color = ~pareto,
    colors = c("gray","red"),
    type="scatter",
    mode="markers"
  )
})


# =========================================================
# ===== SHAP LOCAL (FORCE + TABLE) — FIXED
# =========================================================
output$force_plot <- renderPlotly({

  req(rv$shap)

  cols <- setdiff(colnames(rv$shap), "BIAS")
  if(length(cols) == 0) return(empty_plot())

  i <- max(1, min(input$obs_id, nrow(rv$shap)))

  vals <- as.numeric(rv$shap[i, cols])
  vals[!is.finite(vals)] <- 0

  if(all(vals == 0)){
    vals <- vals + rnorm(length(vals), 0, 1e-6)
  }

  df <- data.frame(
    Feature = cols,
    Contribution = vals
  )

  df <- df[order(df$Contribution), ]

  plot_ly(
    df,
    x = ~Contribution,
    y = ~reorder(Feature, Contribution),
    type = "bar",
    orientation = "h",
    marker = list(color = ifelse(df$Contribution > 0, "red", "blue"))
  )
})


output$local_table <- renderDT({

  req(rv$shap)

  i <- max(1, min(input$obs_id, nrow(rv$shap)))

  vals <- as.numeric(rv$shap[i,])
  names(vals) <- colnames(rv$shap)

  vals[!is.finite(vals)] <- 0

  df <- data.frame(
    Feature = names(vals),
    Value = vals
  )

  df <- df[order(-abs(df$Value)), ]

  datatable(df, options = list(pageLength=20))
})


# =========================================================
# ===== SHAP FORCE PRO (STABLE)
# =========================================================
output$force_real <- renderPlotly({

  req(rv$shap)

  cols <- setdiff(colnames(rv$shap), "BIAS")
  if(length(cols) == 0) return(empty_plot())

  i <- max(1, min(input$obs_id, nrow(rv$shap)))

  vals <- as.numeric(rv$shap[i, cols])
  vals[!is.finite(vals)] <- 0

  if(all(vals == 0)){
    vals <- vals + rnorm(length(vals), 0, 1e-6)
  }

  if(length(vals) > 30){
    idx <- order(abs(vals), decreasing=TRUE)[1:30]
    vals <- vals[idx]
    cols <- cols[idx]
  }

  df <- data.frame(
    Feature = cols,
    Contribution = vals
  )

  df <- df[order(df$Contribution), ]

  plot_ly(
    df,
    x = ~Contribution,
    y = ~reorder(Feature, Contribution),
    type = "bar",
    orientation = "h",
    marker = list(color = ifelse(df$Contribution > 0, "red", "blue"))
  )
})


# =========================================================
# ===== DECISION TEXT (OK)
# =========================================================
output$decision_text <- renderText({

  req(rv$pred)

  i <- max(1, min(input$obs_id, length(rv$pred)))

  p <- rv$pred
  val <- p[i]

  if(!is.finite(val)) return("Invalid prediction")

  mu <- mean(p, na.rm=TRUE)
  sdv <- sd(p, na.rm=TRUE)

  if(!is.finite(sdv) || sdv == 0) sdv <- 1e-6

  z <- (val - mu) / sdv
  perc <- round(100 * mean(p <= val, na.rm=TRUE), 1)

  decision <- if(z > 1){
    "VERY HIGH"
  } else if(z > 0.5){
    "HIGH"
  } else if(z < -1){
    "VERY LOW"
  } else if(z < -0.5){
    "LOW"
  } else {
    "AVERAGE"
  }

  paste0(
    "Prediction: ", round(val, 4),
    "\nZ-score: ", round(z, 3),
    "\nPercentile: ", perc, "%",
    "\nDecision: ", decision
  )
})


  
  # ===== SHAP DYNAMIC =====
output$shap_dynamic <- renderPlotly({

  req(rv$model, rv$X)

  X <- rv$X
  model <- rv$model

  if(is.null(X) || nrow(X) < 5) return(empty_plot())

  feature <- input$shap_feature

  # 🔥 fallback
  if(is.null(feature) || !(feature %in% colnames(X))){
    feature <- colnames(X)[1]
  }

  # ===== OBS =====
  i <- max(1, min(input$obs_id, nrow(X)))

  x0 <- X[i,,drop=FALSE]   # 🔥 KLUCZOWE (dataframe!)

  # ===== RANGE =====
  vals <- X[,feature]
  vals <- vals[is.finite(vals)]

  if(length(vals) < 5){
    showNotification("❌ Feature has no variability", type="error")
    return(empty_plot())
  }

  grid <- seq(min(vals), max(vals), length.out = 60)

  preds <- numeric(length(grid))

  # ===== SIMULATION =====
  for(k in seq_along(grid)){

    x_new <- x0

    # 🔥 zmiana tylko jednej zmiennej
    x_new[[feature]] <- grid[k]

    preds[k] <- tryCatch(
      predict(model, x_new),
      error=function(e) NA
    )
  }

  df <- data.frame(
    x = grid,
    y = preds
  )

  df <- df[is.finite(df$y), ]

  if(nrow(df) < 5){
    showNotification("❌ Model returned NA", type="error")
    return(empty_plot())
  }

  # ===== TREND =====
  trend <- tryCatch(
    stats::loess(y ~ x, data=df),
    error=function(e) NULL
  )

  trend_y <- if(!is.null(trend)){
    predict(trend, newdata=data.frame(x=df$x))
  } else {
    rep(NA, nrow(df))
  }

  p <- plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines+markers"
  )

  if(any(is.finite(trend_y))){
    p <- add_lines(
      p,
      x = df$x,
      y = trend_y,
      name = "trend",
      line = list(width=3)
    )
  }

  p %>% layout(
    title = paste("SHAP Dynamic:", feature),
    xaxis = list(title = feature),
    yaxis = list(title = "Prediction")
  )
})
  
  # ===== COUNTERFACTUAL =====
observeEvent(input$cf_run,{

  req(rv$model, rv$X, rv$pred)

  X <- rv$X
  model <- rv$model
  preds <- rv$pred

  if(is.null(X) || nrow(X) < 5){
    showNotification("❌ Not enough data", type="error")
    return()
  }

  # =========================================================
  # ===== INIT
  # =========================================================
  i <- max(1, min(input$obs_id, nrow(X)))
  x0 <- as.numeric(X[i,])

  sd_vec <- apply(X,2,sd,na.rm=TRUE)
  sd_vec[!is.finite(sd_vec) | sd_vec==0] <- 1

  ranges <- apply(X,2,safe_range)

  # =========================================================
  # ===== TARGET
  # =========================================================
  target <- switch(input$target_mode,
                   "Manual" = input$target_value,
                   "Auto (max prediction)" = max(preds, na.rm=TRUE),
                   "Range" = mean(input$target_range),
                   max(preds, na.rm=TRUE))

  # =========================================================
  # ===== VARIABLES (🔥 FIX)
  # =========================================================
  vars_idx <- match(input$cf_vars, colnames(X))
  vars_idx <- vars_idx[!is.na(vars_idx)]

  if(length(vars_idx)==0){
    vars_idx <- seq_along(x0)
  }

  # =========================================================
  # ===== LAGRANGE WEIGHTS
  # =========================================================
  lambda_cu <- input$lambda_cu
  lambda_ag <- input$lambda_ag

  # =========================================================
  # ===== POPULATION (Pareto) 🔥 FIX: DIVERSITY
  # =========================================================
  pop_size <- max(10, input$cf_pop)
  steps <- 60

  population <- lapply(1:pop_size, function(k){
    x0 + rnorm(length(x0), 0, sd_vec * runif(1,0.05,0.3))
  })

  evaluate <- function(x){

    pred <- tryCatch(predict(model, matrix(x,nrow=1)), error=function(e) NA)
    if(!is.finite(pred)) pred <- -Inf

    obj_pred <- -abs(pred - target)

    obj_cu <- 0
    obj_ag <- 0

    if("Cu" %in% colnames(X)){
      cu <- x[which(colnames(X)=="Cu")]
      if(!is.na(input$constraint_Cu)) obj_cu <- obj_cu - max(0, cu - input$constraint_Cu)
      if(!is.na(input$min_Cu))        obj_cu <- obj_cu - max(0, input$min_Cu - cu)
    }

    if("Ag" %in% colnames(X)){
      ag <- x[which(colnames(X)=="Ag")]
      if(!is.na(input$constraint_Ag)) obj_ag <- obj_ag - max(0, ag - input$constraint_Ag)
      if(!is.na(input$min_Ag))        obj_ag <- obj_ag - max(0, input$min_Ag - ag)
    }

    c(obj_pred, obj_cu, obj_ag)
  }

  # =========================================================
  # ===== PARETO DOMINANCE
  # =========================================================
  dominates <- function(a,b){
    all(a >= b) && any(a > b)
  }

  pareto_filter <- function(pop, scores){

    keep <- rep(TRUE, length(pop))

    for(i in seq_along(pop)){
      for(j in seq_along(pop)){
        if(i!=j && dominates(scores[[j]], scores[[i]])){
          keep[i] <- FALSE
          break
        }
      }
    }

    list(
      pop = pop[keep],
      scores = scores[keep]
    )
  }

  # =========================================================
  # ===== OPT LOOP
  # =========================================================
  for(step in 1:steps){

    new_pop <- list()

    for(ind in population){

      x <- ind

      grad <- numeric(length(x))

      pred0 <- tryCatch(predict(model, matrix(x,nrow=1)), error=function(e) NA)
      if(!is.finite(pred0)) next

      for(j in vars_idx){

        x_up <- x
        x_up[j] <- x_up[j] + sd_vec[j]*0.01

        p1 <- tryCatch(predict(model, matrix(x_up,nrow=1)), error=function(e) NA)

        if(is.finite(p1)){
          grad[j] <- (p1 - pred0)/(sd_vec[j]*0.01)
        }
      }

      grad[!is.finite(grad)] <- 0

      # ===== NORMALIZE
      gnorm <- sqrt(sum(grad^2))
      if(gnorm > 0) grad <- grad / gnorm

      # ===== LAGRANGE FORCE
      lagrange_grad <- rep(0,length(x))

      if("Cu" %in% colnames(X)){
        j <- which(colnames(X)=="Cu")
        if(!is.na(input$constraint_Cu) && x[j] > input$constraint_Cu){
          lagrange_grad[j] <- lagrange_grad[j] - lambda_cu
        }
      }

      if("Ag" %in% colnames(X)){
        j <- which(colnames(X)=="Ag")
        if(!is.na(input$constraint_Ag) && x[j] > input$constraint_Ag){
          lagrange_grad[j] <- lagrange_grad[j] - lambda_ag
        }
      }

      # ===== UPDATE (🔥 LEPSZA EKSPLORACJA)
      x_new <- x +
        0.1 * grad * sd_vec +
        0.05 * lagrange_grad +
        rnorm(length(x),0,sd_vec * runif(1,0.01,0.05))

      # ===== BOUNDS
      for(j in seq_along(x_new)){
        r <- ranges[,j]
        if(!all(is.finite(r)) || diff(r)==0) r <- c(-1,1)
        x_new[j] <- max(min(x_new[j], r[2]), r[1])
      }

      x_new[!is.finite(x_new)] <- 0

      new_pop[[length(new_pop)+1]] <- x_new
    }

    population <- c(population, new_pop)

    scores <- lapply(population, evaluate)

    # 🔥 zapisz BEFORE filtr (pełne Pareto)
    rv$cf_pareto <- list(
      pop = population,
      scores = scores
    )

    pf <- pareto_filter(population, scores)

    population <- pf$pop

    # 🔥 FIX: losowe przycinanie (nie bias)
    if(length(population) > pop_size){
      idx <- sample(seq_along(population), pop_size)
      population <- population[idx]
    }

    # 🔥 FIX: jeśli populacja się zapadnie → restart
    if(length(population) < 3){
      population <- lapply(1:pop_size, function(k){
        x0 + rnorm(length(x0), 0, sd_vec * runif(1,0.05,0.3))
      })
    }
  }

  # =========================================================
  # ===== FINAL RESULT
  # =========================================================
  scores <- lapply(population, evaluate)

  best_idx <- switch(input$cf_pick,
    "Best prediction" = which.max(sapply(scores, function(s) s[1])),
    "Min Cu" = which.max(sapply(scores, function(s) s[2])),
    "Min Ag" = which.max(sapply(scores, function(s) s[3])),
    "Balanced" = which.max(sapply(scores, function(s) sum(s))),
    which.max(sapply(scores, function(s) s[1]))
  )

  best_x <- population[[best_idx]]

  delta <- best_x - x0

  rv$cf <- data.frame(
    Feature = colnames(X),
    Original = x0,
    Counterfactual = best_x,
    Change = delta,
    AbsChange = abs(delta),
    stringsAsFactors = FALSE
  )

  rv$cf <- rv$cf[order(-rv$cf$AbsChange), ]

  showNotification(
    paste("🔥 Pareto CF (", length(population), " solutions)", sep=""),
    type="message"
  )
})




output$cf_table <- renderDT({
  req(rv$cf)
  
  df <- rv$cf
  
  if(is.null(df) || nrow(df)==0){
    return(datatable(data.frame(Message="No counterfactual found")))
  }
  
  datatable(df, options = list(pageLength = 20))
})

# =========================================================
# 🔥 TU WKLEJ WYKRESY PARETO
# =========================================================

output$cf_pareto_2d <- renderPlotly({

  req(rv$cf_pareto)

  scores <- rv$cf_pareto$scores

  df <- data.frame(
    pred = sapply(scores, function(s) s[1]),
    cu   = sapply(scores, function(s) s[2])
  )

  df$pred[!is.finite(df$pred)] <- 0
  df$cu[!is.finite(df$cu)] <- 0

  plot_ly(
    df,
    x = ~cu,
    y = ~pred,
    type = "scatter",
    mode = "markers",
    marker = list(size=7, opacity=0.7)
  ) %>%
    layout(
      xaxis = list(title="Cu objective"),
      yaxis = list(title="Prediction objective")
    )
})

output$cf_pareto_3d <- renderPlotly({

  req(rv$cf_pareto)

  scores <- rv$cf_pareto$scores

  df <- data.frame(
    pred = sapply(scores, function(s) s[1]),
    cu   = sapply(scores, function(s) s[2]),
    ag   = sapply(scores, function(s) s[3])
  )

  df[!is.finite(as.matrix(df))] <- 0

  plot_ly(
    df,
    x = ~cu,
    y = ~ag,
    z = ~pred,
    type = "scatter3d",
    mode = "markers",
    marker = list(size=4, opacity=0.8)
  ) %>%
    layout(
      scene = list(
        xaxis = list(title="Cu"),
        yaxis = list(title="Ag"),
        zaxis = list(title="Prediction")
      )
    )
})
  
  # ===== AUTO FEATURES =====
observeEvent(input$gen_features,{

  req(rv$data)

  df <- rv$data

  # =========================================================
  # ===== NUMERIC ONLY
  # =========================================================
  num <- df[, sapply(df, is.numeric), drop=FALSE]

  if(ncol(num) == 0){
    showNotification("❌ No numeric columns", type="error")
    return()
  }

  num <- as.data.frame(num)

  # =========================================================
  # ===== CLEAN
  # =========================================================
  num[!is.finite(as.matrix(num))] <- NA

  # 🔥 imputacja zamiast usuwania
  for(col in names(num)){
    v <- num[[col]]
    if(any(is.na(v))){
      med <- median(v, na.rm=TRUE)
      if(!is.finite(med)) med <- 0
      v[is.na(v)] <- med
      num[[col]] <- v
    }
  }

  # =========================================================
  # ===== REMOVE CONSTANTS (SAFE)
  # =========================================================
  var_ok <- apply(num,2,function(x){
    v <- var(x, na.rm=TRUE)
    is.finite(v) && v > 1e-12
  })

  if(sum(var_ok) < 2){
    showNotification("❌ Not enough variable features", type="error")
    return()
  }

  num <- num[, var_ok, drop=FALSE]

  # =========================================================
  # ===== FEATURE SELECTION
  # =========================================================
  if(!is.null(rv$shap)){

    cols <- intersect(
      setdiff(colnames(rv$shap),"BIAS"),
      colnames(num)
    )

    if(length(cols) >= 2){

      shap_vals <- colMeans(abs(rv$shap[,cols,drop=FALSE]), na.rm=TRUE)
      shap_vals[!is.finite(shap_vals)] <- 0

      top <- names(sort(shap_vals, decreasing=TRUE))[1:min(6,length(shap_vals))]

    } else {
      top <- colnames(num)[1:min(6,ncol(num))]
    }

  } else {
    top <- colnames(num)[1:min(6,ncol(num))]
  }

  # 🔥 HARD SAFETY
  top <- intersect(top, colnames(num))

  if(length(top) < 2){
    top <- colnames(num)[1:min(5,ncol(num))]
  }

  if(length(top) < 2){
    showNotification("❌ Not enough features after selection", type="error")
    return()
  }

  # =========================================================
  # ===== FEATURE ENGINEERING
  # =========================================================
  new_feats <- list()

  for(i in 1:(length(top)-1)){
    for(j in (i+1):length(top)){

      a <- num[[top[i]]]
      b <- num[[top[j]]]

      if(length(a) != length(b)) next

      ratio <- ifelse(abs(b) > 1e-9, a/b, 0)
      ratio[!is.finite(ratio)] <- 0

      prod <- a * b
      diff <- a - b
      logf <- log(abs(prod) + 1)
      sqrtf <- sqrt(abs(prod))

      prod[!is.finite(prod)] <- 0
      diff[!is.finite(diff)] <- 0
      logf[!is.finite(logf)] <- 0
      sqrtf[!is.finite(sqrtf)] <- 0

      new_feats[[paste0(top[i],"_BIO_",top[j])]]  <- prod
      new_feats[[paste0(top[i],"_DIFF_",top[j])]] <- diff
      new_feats[[paste0(top[i],"_RATIO_",top[j])]]<- ratio
      new_feats[[paste0(top[i],"_LOG_",top[j])]]  <- logf
      new_feats[[paste0(top[i],"_SQRT_",top[j])]] <- sqrtf
    }
  }

  if(length(new_feats) == 0){
    showNotification("❌ No features generated", type="error")
    return()
  }

  new_df <- as.data.frame(new_feats)

  # =========================================================
  # ===== FINAL CLEAN
  # =========================================================
  new_df[!is.finite(as.matrix(new_df))] <- 0

  keep <- apply(new_df,2,function(x){
    v <- var(x, na.rm=TRUE)
    is.finite(v) && v > 1e-10
  })

  new_df <- new_df[, keep, drop=FALSE]

  if(ncol(new_df) == 0){
    showNotification("❌ All generated features are constant", type="error")
    return()
  }

  # =========================================================
  # ===== CORRELATION PRUNING
  # =========================================================
  if(ncol(new_df) > 2){

    corr <- cor(new_df)
    corr[!is.finite(corr)] <- 0

    if(requireNamespace("caret", quietly=TRUE)){
      high_corr <- caret::findCorrelation(corr, cutoff=0.95)
      if(length(high_corr) > 0){
        new_df <- new_df[, -high_corr, drop=FALSE]
      }
    }
  }

  rv$engineered <- new_df

  showNotification(
    paste("✅ Generated", ncol(new_df), "features"),
    type="message"
  )
})


output$features_table <- renderDT({
  
  if(is.null(rv$engineered) || ncol(rv$engineered)==0){
    return(datatable(data.frame(Message="No features generated")))
  }
  
  df <- rv$engineered
  
  # 🔥 pokaz tylko próbkę jeśli duże
  if(ncol(df) > 50){
    df <- df[,1:50]
  }
  
  datatable(head(df,100))
})


output$dead_features_text <- renderText({
  
  if(is.null(rv$dead_features) || length(rv$dead_features)==0){
    return("No dead features")
  }
  
  paste0(
    "Dead features (", length(rv$dead_features), "):\n",
    paste(rv$dead_features, collapse=", ")
  )
})
  
  
# ===== GENETIC ALGORITHM (FIXED CLEAN) =====
observeEvent(input$run_ga,{

  req(rv$X, rv$y)

  X <- rv$X
  y <- rv$y

  if(is.null(X) || is.null(y) || ncol(X) < 2 || nrow(X) < 5){
    showNotification("❌ Not enough data for GA", type="error")
    return()
  }

  vars_all <- colnames(X)

  # ===== PARAMS (STABILNE) =====
  pop_size <- 40
  generations <- 25
  mutation_rate <- 0.25
  elite_size <- 6
  tournament_k <- 5

  # ===== SAFE SCORE FUNCTION =====
score_fn <- function(vars){

  if(length(vars) < 2) return(0)

  v1 <- vars[1]
  v2 <- vars[2]

  if(!(v1 %in% colnames(X)) || !(v2 %in% colnames(X))) return(0)

  df <- data.frame(
    y = y,
    X1 = X[, v1],
    X2 = X[, v2]
  )

  # ===== IMPUTACJA ZAMIENIAJĄCA complete.cases =====
  for(col in names(df)){
    v <- df[[col]]

    if(any(!is.finite(v))){
      med <- median(v[is.finite(v)], na.rm=TRUE)
      if(!is.finite(med)) med <- 0

      v[!is.finite(v)] <- med
      df[[col]] <- v
    }
  }

  # ===== MINIMALNA WALIDACJA =====
  if(nrow(df) < 5) return(0)

  # ===== MODEL =====
  s <- tryCatch({
    m <- lm(y ~ X1 + X2, data=df)
    summary(m)$r.squared
  }, error=function(e) NA)

  # ===== FALLBACK =====
  if(!is.finite(s) || is.na(s)){
    s <- tryCatch(
      abs(cor(df$y, df$X1)),
      error=function(e) 0
    )
  }

  if(!is.finite(s)) s <- 0

  return(as.numeric(s))
}

  # ===== INIT POPULATION =====
  pop <- replicate(pop_size, sample(vars_all, 2), simplify=FALSE)

  best_global <- NULL
  best_score <- -Inf
  history <- numeric(generations)

  # ===== EVOLUTION LOOP =====
  for(gen in seq_len(generations)){

    scores <- sapply(pop, score_fn)
    scores[!is.finite(scores)] <- 0

    # 🔥 ranking
    ord <- order(scores, decreasing=TRUE)
    pop <- pop[ord]
    scores <- scores[ord]

    # ===== GLOBAL BEST =====
    if(scores[1] > best_score){
      best_score <- scores[1]
      best_global <- pop[[1]]
    }

    history[gen] <- scores[1]

    # ===== ELITISM =====
    new_pop <- pop[1:elite_size]

    # ===== TOURNAMENT SELECTION =====
    select_parent <- function(){
      idx <- sample(seq_along(pop), tournament_k, replace=TRUE)
      best <- idx[which.max(scores[idx])]
      pop[[best]]
    }

    # ===== GENERATE NEW =====
    while(length(new_pop) < pop_size){

      p1 <- select_parent()
      p2 <- select_parent()

      child <- unique(c(p1, p2))

      if(length(child) > 2){
        child <- sample(child, 2)
      }

      # ===== MUTATION =====
      if(runif(1) < mutation_rate){
        j <- sample(1:2, 1)
        child[j] <- sample(vars_all, 1)
      }

      # ===== SAFETY =====
      child <- unique(child)

      if(length(child) < 2){
        child <- sample(vars_all, 2)
      }

      new_pop[[length(new_pop)+1]] <- child
    }

    # ===== DIVERSITY INJECTION =====
    if(gen %% 5 == 0){
      for(k in 1:5){
        new_pop[[sample(1:pop_size,1)]] <- sample(vars_all, 2)
      }
    }

    pop <- new_pop
  }

  # ===== FINAL SAFETY =====
  if(is.null(best_global) || !is.finite(best_score)){
    best_global <- sample(vars_all, 2)
    best_score <- 0
  }

  rv$ga_result <- list(
    vars = best_global,
    score = best_score,
    history = history
  )

  showNotification(
    paste0(
      "✅ Best: ",
      paste(best_global, collapse=" + "),
      " | R² = ", round(best_score, 4)
    ),
    type = "message"
  )
})



# ===== GA PLOT =====
output$ga_plot <- renderPlotly({
  req(rv$ga_result, rv$X, rv$y)

  v <- rv$ga_result$vars

  if(is.null(v) || length(v) < 1) return(empty_plot())

  x <- rv$X[, v[1]]
  y <- rv$y

  x <- as.numeric(x)
  y <- as.numeric(y)

  ok <- is.finite(x) & is.finite(y)

  if(sum(ok) < 5) return(empty_plot())

  df <- data.frame(x = x[ok], y = y[ok])

  # 🔥 trend line
  trend <- tryCatch(
    stats::lm(y ~ x, data=df),
    error=function(e) NULL
  )

  p <- plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "markers",
    marker = list(size=6, opacity=0.7)
  )

  if(!is.null(trend)){
    xs <- seq(min(df$x), max(df$x), length.out=50)
    ys <- predict(trend, newdata=data.frame(x=xs))

    p <- add_lines(p, x=xs, y=ys, name="trend")
  }

  p
})



# =========================================================
# ===== BAYESIAN OPTIMIZATION — SAFE ENTERPRISE VERSION
# =========================================================

# ===== NAME NORMALIZATION (KLUCZOWE) =====
normalize_df <- function(df, ref_names = NULL){

  df <- as.data.frame(df)

  # 🔥 ujednolicenie nazw
  names(df) <- make.names(names(df))

  # 🔥 jeśli mamy referencję → dopasuj 1:1
  if(!is.null(ref_names)){
    ref_names <- make.names(ref_names)

    # brakujące kolumny → dodaj
    missing <- setdiff(ref_names, names(df))
    for(m in missing){
      df[[m]] <- 0
    }

    # kolejność jak w design
    df <- df[, ref_names, drop=FALSE]
  }

  df
}

# ===== EI =====
expected_improvement <- function(mu, sigma, best){

  sigma[sigma < 1e-6] <- 1e-6

  improvement <- mu - best
  Z <- improvement / sigma

  ei <- improvement * pnorm(Z) + sigma * dnorm(Z)

  ei[!is.finite(ei)] <- 0
  ei[ei < 0] <- 0

  ei
}

# =========================================================
# ===== SINGLE BO (FIXED)
# =========================================================
observeEvent(input$run_bo,{

  req(rv$X, rv$model, rv$y)

  X <- rv$X
  y <- rv$y
  model <- rv$model

  if(nrow(X) < 10 || ncol(X) < 2){
    showNotification("❌ Not enough data for BO", type="error")
    return()
  }

  # ===== SCALE =====
  Xs <- scale(X)
  Xs[!is.finite(Xs)] <- 0

  X_train <- normalize_df(Xs)
  y_train <- as.numeric(y)

  ok <- is.finite(rowSums(X_train)) & is.finite(y_train)
  X_train <- X_train[ok,,drop=FALSE]
  y_train <- y_train[ok]

  if(nrow(X_train) < 10){
    showNotification("❌ Too few valid points", type="error")
    return()
  }

  bounds <- apply(X_train, 2, safe_range)

  iters <- input$bo_iter
  history <- numeric(iters)
  path <- list()

  for(i in seq_len(iters)){

    gp <- tryCatch(
      km(design = X_train, response = y_train, nugget.estim = TRUE),
      error=function(e) NULL
    )

    if(is.null(gp)) next

    # ===== GENERATE CANDIDATES =====
    n_cand <- 500
    candidates <- matrix(0, nrow=n_cand, ncol=ncol(X_train))

    for(j in 1:ncol(X_train)){
      r <- bounds[,j]
      candidates[,j] <- runif(n_cand, r[1], r[2])
    }

    colnames(candidates) <- colnames(X_train)

    newdata <- normalize_df(candidates, colnames(X_train))

    pred <- tryCatch(
      predict(gp, newdata=newdata, type="UK"),
      error=function(e) NULL
    )

    if(is.null(pred)) next

    mu <- pred$mean
    sigma <- pred$sd

    mu[!is.finite(mu)] <- 0
    sigma[!is.finite(sigma)] <- 1e-6

    best <- max(y_train, na.rm=TRUE)

    ei <- expected_improvement(mu, sigma, best)

    if(all(ei <= 0)) next

    idx <- which.max(ei)
    x_new <- newdata[idx,]

    # ===== BACK TO ORIGINAL SCALE =====
    x_orig <- as.numeric(
      x_new * attr(Xs,"scaled:scale") +
      attr(Xs,"scaled:center")
    )

    y_new <- tryCatch(
      predict(model, matrix(x_orig, nrow=1)),
      error=function(e) NA
    )

    if(!is.finite(y_new)) next

    # ===== UPDATE =====
    X_train <- rbind(X_train, x_new)
    y_train <- c(y_train, y_new)

    history[i] <- y_new
    path[[i]] <- x_new
  }

  history <- history[is.finite(history)]

  if(length(history) < 3){
    showNotification("❌ BO failed", type="error")
    return()
  }

  rv$bo_history <- history
  rv$bo_path <- do.call(rbind, path)
  rv$bo_gp_final <- gp
  rv$bo_X_final <- X_train

  showNotification("✅ Bayesian Optimization OK", type="message")
})

# =========================================================
# ===== MULTI BO (FIXED)
# =========================================================
observeEvent(input$run_bo_multi,{

  req(rv$X, rv$model, rv$pred)

  X_train <- normalize_df(rv$X)
  model <- rv$model

  Y <- data.frame(
    pred = rv$pred,
    Cu = if("Cu" %in% colnames(X_train)) X_train[,"Cu"] else 0,
    Ag = if("Ag" %in% colnames(X_train)) X_train[,"Ag"] else 0
  )

  bounds <- apply(X_train, 2, safe_range)

  history <- numeric(30)
  path <- list()

  for(i in 1:30){

    models <- lapply(Y, function(ycol){
      tryCatch(
        km(design=X_train, response=ycol, nugget.estim=TRUE),
        error=function(e) NULL
      )
    })

    candidates <- matrix(0, nrow=200, ncol=ncol(X_train))

    for(j in 1:ncol(X_train)){
      r <- bounds[,j]
      candidates[,j] <- runif(200, r[1], r[2])
    }

    colnames(candidates) <- colnames(X_train)

    scores <- apply(candidates,1,function(x){

      nd <- normalize_df(as.data.frame(t(x)), colnames(X_train))

      preds <- sapply(models,function(m){
        if(is.null(m)) return(0)

        out <- tryCatch(
          predict(m, newdata=nd),
          error=function(e) NULL
        )

        if(is.null(out)) return(0)
        out$mean
      })

      preds[!is.finite(preds)] <- 0

      preds["pred"] - 0.5*abs(preds["Cu"]) - 0.5*abs(preds["Ag"])
    })

    idx <- which.max(scores)
    x_new <- candidates[idx,]

    y_new <- predict(model, matrix(x_new, nrow=1))

    X_train <- rbind(X_train, x_new)
    Y <- rbind(Y, data.frame(
      pred=y_new,
      Cu=x_new["Cu"],
      Ag=x_new["Ag"]
    ))

    history[i] <- y_new
    path[[i]] <- x_new
  }

  rv$bo_multi_history <- history
  rv$bo_multi_path <- do.call(rbind, path)

  showNotification("✅ Multi BO finished", type="message")
})

# =========================================================
# ===== SAFE SURFACE + EI (FIXED)
# =========================================================
output$bo_surface <- renderPlotly({

  req(rv$bo_gp_final, rv$bo_X_final, input$bo_x1, input$bo_x2)

  gp <- rv$bo_gp_final
  X <- rv$bo_X_final

  vars <- colnames(X)

  x1 <- input$bo_x1
  x2 <- input$bo_x2

  grid1 <- seq(min(X[,x1]), max(X[,x1]), length.out=30)
  grid2 <- seq(min(X[,x2]), max(X[,x2]), length.out=30)

  grid <- expand.grid(grid1, grid2)
  colnames(grid)[1:2] <- c(x1,x2)

  for(v in vars[!vars %in% c(x1,x2)]){
    grid[[v]] <- mean(X[[v]])
  }

  grid <- normalize_df(grid, vars)

  pred <- predict(gp, newdata=grid)

  z <- matrix(pred$mean, nrow=length(grid1))

  plot_ly(x=grid1, y=grid2, z=z, type="surface")
})

output$bo_ei <- renderPlotly({

  req(rv$bo_gp_final, rv$bo_X_final, rv$bo_history,
      input$bo_x1, input$bo_x2)

  gp <- rv$bo_gp_final
  X <- rv$bo_X_final

  vars <- colnames(X)

  x1 <- input$bo_x1
  x2 <- input$bo_x2

  grid1 <- seq(min(X[,x1]), max(X[,x1]), length.out=40)
  grid2 <- seq(min(X[,x2]), max(X[,x2]), length.out=40)

  grid <- expand.grid(grid1, grid2)
  colnames(grid)[1:2] <- c(x1,x2)

  for(v in vars[!vars %in% c(x1,x2)]){
    grid[[v]] <- mean(X[[v]])
  }

  grid <- normalize_df(grid, vars)

  pred <- predict(gp, newdata=grid)

  mu <- pred$mean
  sigma <- pred$sd

  best <- max(rv$bo_history, na.rm=TRUE)

  ei <- expected_improvement(mu, sigma, best)

  z <- matrix(ei, nrow=length(grid1))

  plot_ly(x=grid1, y=grid2, z=z, type="surface")
})

# =========================================================
# ===== BO PLOT (PRZYWRÓCENIE)
# =========================================================
output$bo_plot <- renderPlotly({

  req(rv$bo_history)

  h <- rv$bo_history

  df <- data.frame(
    iter = seq_along(h),
    val = h,
    best = cummax(h)
  )

  plot_ly(
    df,
    x = ~iter,
    y = ~val,
    type = "scatter",
    mode = "lines"
  ) %>%
    add_lines(y = ~best, name = "Best")
})





# =========================================================
# ===== SURFACE (UI AXES)
# =========================================================
output$bo_surface <- renderPlotly({

  req(rv$bo_gp_final, rv$bo_X_final, input$bo_x1, input$bo_x2)

  gp <- rv$bo_gp_final
  X <- rv$bo_X_final

  x1 <- input$bo_x1
  x2 <- input$bo_x2

  vars <- colnames(X)

  grid1 <- seq(min(X[,x1]), max(X[,x1]), length.out=30)
  grid2 <- seq(min(X[,x2]), max(X[,x2]), length.out=30)

  grid <- expand.grid(grid1, grid2)
  colnames(grid)[1:2] <- c(x1,x2)

  for(v in vars[!vars %in% c(x1,x2)]){
    grid[[v]] <- mean(X[[v]])
  }

  grid <- grid[,vars]

  pred <- predict(gp, newdata=grid, type="UK")

  z <- matrix(pred$mean, nrow=length(grid1))

  plot_ly(x=grid1, y=grid2, z=z, type="surface")
})

# =========================================================
# ===== EI (UI AXES)
# =========================================================
output$bo_ei <- renderPlotly({

  req(rv$bo_gp_final, rv$bo_X_final, rv$bo_history,
      input$bo_x1, input$bo_x2)

  gp <- rv$bo_gp_final
  X <- rv$bo_X_final

  x1_name <- input$bo_x1
  x2_name <- input$bo_x2

  vars <- colnames(X)

  x1 <- seq(min(X[,x1_name]), max(X[,x1_name]), length.out=40)
  x2 <- seq(min(X[,x2_name]), max(X[,x2_name]), length.out=40)

  grid <- expand.grid(x1, x2)
  colnames(grid)[1:2] <- c(x1_name, x2_name)

  for(v in vars[!vars %in% c(x1_name, x2_name)]){
    grid[[v]] <- mean(X[[v]], na.rm=TRUE)
  }

  grid <- grid[, vars]

  pred <- predict(gp, newdata=grid, type="UK")

  mu <- pred$mean
  sigma <- pred$sd
  best <- max(rv$bo_history, na.rm=TRUE)

  ei <- expected_improvement(mu, sigma, best)

  z <- matrix(ei, nrow=length(x1))

  plot_ly(x=x1, y=x2, z=z, type="surface")
})

# =========================================================
# ===== PATH
# =========================================================
output$bo_path <- renderPlotly({

  req(rv$bo_path)

  df <- as.data.frame(rv$bo_path)

  if(nrow(df) < 2) return(empty_plot())

  plot_ly(df,
          x=~df[,1],
          y=~df[,2],
          z=seq_len(nrow(df)),
          type="scatter3d",
          mode="lines+markers")
})







  
  
  
  
observeEvent(input$run_dyn,{

  req(rv$model, rv$X)

  X <- rv$X
  model <- rv$model

  if(is.null(X) || nrow(X) < 2){
    showNotification("❌ DYN: not enough data", type="error")
    return()
  }

  steps <- max(5, input$sim_time)

  # ===== INIT STATE =====
  i <- max(1, min(input$obs_id, nrow(X)))
  x <- as.numeric(X[i,])

  if(any(!is.finite(x))){
    x[!is.finite(x)] <- 0
  }

  p_dim <- length(x)

  history <- matrix(NA, nrow=steps, ncol=p_dim)
  preds <- numeric(steps)

  # ===== STATS =====
  sd_vec <- apply(X, 2, sd, na.rm=TRUE)
  sd_vec[!is.finite(sd_vec) | sd_vec == 0] <- 1e-6

  ranges <- apply(X, 2, safe_range)

  # ===== STABILITY PARAMS =====
  base_noise <- 0.12
  drift_strength <- 0.02
  momentum <- rep(0, p_dim)

  best_pred <- -Inf
  stagnation <- 0

  for(t in seq_len(steps)){

    # ===== ADAPTIVE ANNEALING =====
    noise_scale <- base_noise * exp(-t / steps)

    noise <- rnorm(p_dim, 0, sd_vec * noise_scale)
    noise[!is.finite(noise)] <- 0

    # ===== MOMENTUM UPDATE =====
    momentum <- 0.7 * momentum + noise

    # ===== CONTROLLED DRIFT (pull to center) =====
    center <- colMeans(X, na.rm=TRUE)
    center[!is.finite(center)] <- 0

    drift <- drift_strength * (center - x)

    # ===== STATE UPDATE =====
    x <- x + momentum + drift

    # ===== HARD BOUNDARIES =====
    for(j in seq_len(p_dim)){
      r <- ranges[,j]

      if(!all(is.finite(r)) || diff(r) == 0){
        r <- c(-1,1)
      }

      x[j] <- max(min(x[j], r[2]), r[1])
    }

    x[!is.finite(x)] <- 0

    # ===== SAFE PREDICTION =====
    p <- tryCatch(
      predict(model, matrix(x, nrow=1)),
      error=function(e) NA
    )

    if(!is.finite(p)){
      p <- NA
    }

    preds[t] <- p
    history[t,] <- x

    # ===== EARLY STOP (stagnation) =====
    if(is.finite(p)){
      if(p > best_pred){
        best_pred <- p
        stagnation <- 0
      } else {
        stagnation <- stagnation + 1
      }
    }

    if(stagnation > 10){
      break
    }
  }

  preds[!is.finite(preds)] <- NA

  rv$dyn_history <- history
  rv$dyn_pred <- preds

  # ===== SAFE PLOT =====
  output$dyn_plot <- renderPlotly({

    p <- rv$dyn_pred

    if(is.null(p) || sum(is.finite(p)) < 2){
      return(empty_plot())
    }

    df <- data.frame(
      step = seq_along(p),
      pred = p
    )

    df <- df[is.finite(df$pred), , drop=FALSE]

    if(nrow(df) < 2) return(empty_plot())

    plot_ly(
      df,
      x = ~step,
      y = ~pred,
      type = "scatter",
      mode = "lines",
      line = list(width = 3)
    )
  })
})


  
observeEvent(input$run_ode,{

  req(rv$X)

  if(!require(deSolve)){
    showNotification("❌ Package deSolve not available", type="error")
    return()
  }

  X <- rv$X

  if(is.null(X) || nrow(X) < 3){
    showNotification("❌ ODE: not enough data", type="error")
    return()
  }

  # ===== SAFE PARAMS =====
  params <- list(
    growth = 0.25,
    stress = 0.08,
    decay = 0.04,
    capacity = 100,
    damping = 0.02
  )

  # ===== SAFE MODEL =====
  model <- function(t, state, parameters){

    with(as.list(c(state, parameters)),{

      # ===== HARD SAFETY =====
      Embryo <- ifelse(is.finite(Embryo), max(Embryo, 0), 0)
      Cu     <- ifelse(is.finite(Cu), max(Cu, 0), 0)

      # ===== LOGISTIC GROWTH =====
      growth_term <- growth * Embryo * (1 - Embryo / (capacity + 1e-6))

      # ===== STRESS RESPONSE (bounded) =====
      stress_term <- stress * Cu * Embryo / (1 + Embryo + Cu)

      # ===== DAMPING (prevents explosion) =====
      damping_term <- damping * Embryo^1.2

      dEmbryo <- growth_term - stress_term - damping_term

      # ===== Cu DECAY + FLOOR =====
      dCu <- -decay * Cu

      # ===== FINAL SAFETY =====
      dEmbryo <- ifelse(is.finite(dEmbryo), dEmbryo, 0)
      dCu     <- ifelse(is.finite(dCu), dCu, 0)

      list(c(dEmbryo, dCu))
    })
  }

  # ===== INIT STATE =====
  Cu_init <- if("Cu" %in% colnames(X)){
    mean(X[,"Cu"], na.rm=TRUE)
  } else {
    0
  }

  if(!is.finite(Cu_init)) Cu_init <- 0

  Embryo_init <- 100

  state <- c(
    Embryo = Embryo_init,
    Cu = Cu_init
  )

  state[!is.finite(state)] <- 0

  # ===== TIME GRID =====
  times <- seq(0, 50, by = 1)

  # ===== SOLVER (PRIMARY) =====
  out <- tryCatch(
    ode(
      y = state,
      times = times,
      func = model,
      parms = params,
      method = "lsoda"
    ),
    error = function(e) NULL
  )

  # ===== FALLBACK SOLVER =====
  if(is.null(out)){
    out <- tryCatch(
      ode(
        y = state,
        times = times,
        func = model,
        parms = params,
        method = "rk4"
      ),
      error = function(e) NULL
    )
  }

  if(is.null(out)){
    showNotification("❌ ODE failed (all solvers)", type="error")
    return()
  }

  df <- as.data.frame(out)

  # ===== CLEAN =====
  df <- df[apply(df,1,function(r) all(is.finite(r))), , drop=FALSE]

  if(nrow(df) < 5){
    showNotification("⚠️ ODE unstable → too few valid points", type="warning")
    return()
  }

  # ===== NORMALIZATION (optional stability) =====
  if(max(df$Embryo, na.rm=TRUE) > 1e6){
    df$Embryo <- df$Embryo / max(df$Embryo, na.rm=TRUE)
  }

  # ===== SAVE =====
  rv$ode <- df

  # ===== PLOT =====
  output$ode_plot <- renderPlotly({

    d <- rv$ode

    if(is.null(d) || nrow(d) < 5){
      return(empty_plot())
    }

    plot_ly(
      d,
      x = ~time,
      y = ~Embryo,
      type = "scatter",
      mode = "lines",
      line = list(width = 3)
    )
  })
})

observeEvent(input$run_rl,{

  req(rv$model, rv$X, rv$pred)

  if(!require(torch)){
    showNotification("❌ torch not available", type="error")
    return()
  }

  X <- rv$X
  model <- rv$model

  if(is.null(X) || nrow(X) < 2){
    showNotification("❌ RL: not enough data", type="error")
    return()
  }

  # ===== INIT STATE =====
  i <- max(1, min(input$obs_id, nrow(X)))
  state <- torch_tensor(as.numeric(X[i,]), dtype=torch_float())

  dim_state <- length(state)

  # ===== POLICY NETWORK =====
  policy <- nn_module(
    initialize = function(){
      self$l1 <- nn_linear(dim_state, 32)
      self$l2 <- nn_linear(32, 32)
      self$out <- nn_linear(32, dim_state)
    },
    forward = function(x){
      x %>%
        self$l1() %>% nnf_relu() %>%
        self$l2() %>% nnf_relu() %>%
        self$out()
    }
  )()

  opt <- optim_adam(policy$parameters, lr=0.003)

  history <- numeric(50)

  # ===== STATS =====
  pred_mean <- mean(rv$pred, na.rm=TRUE)
  pred_sd   <- sd(rv$pred, na.rm=TRUE)

  if(!is.finite(pred_sd) || pred_sd == 0) pred_sd <- 1

  # ===== RANGE CONSTRAINTS =====
  ranges <- apply(X, 2, safe_range)

  # ===== RL LOOP =====
  for(t in 1:50){

    # ===== ACTION =====
    action <- policy(state)

    # 🔥 CLIP (stabilność)
    action <- torch_clamp(action, -0.5, 0.5)

    # ===== EXPLORATION =====
    noise <- torch_tensor(rnorm(dim_state, 0, 0.05), dtype=torch_float())
    action <- action + noise

    # ===== NEW STATE =====
    new_state <- state + action

    # ===== HARD BOUNDS =====
    ns <- as.numeric(new_state)

    for(j in seq_len(dim_state)){
      r <- ranges[,j]

      if(!all(is.finite(r)) || diff(r) == 0){
        r <- c(-1,1)
      }

      ns[j] <- max(min(ns[j], r[2]), r[1])
    }

    ns[!is.finite(ns)] <- 0
    new_state <- torch_tensor(ns, dtype=torch_float())

    # ===== REWARD =====
    reward_val <- tryCatch(
      predict(model, matrix(ns, nrow=1)),
      error=function(e) NA
    )

    if(!is.finite(reward_val)) reward_val <- pred_mean

    # 🔥 NORMALIZACJA (kluczowa)
    reward_val <- reward_val - pred_mean
    reward_val <- max(min(reward_val, 10), -10)

    reward <- torch_tensor(reward_val, dtype=torch_float())

    # ===== LOSS =====
    logits <- policy(state)

    entropy_penalty <- torch_mean(logits^2)

    loss <- -torch_mean(logits) * reward + 0.01 * entropy_penalty

    # ===== BACKPROP =====
    opt$zero_grad()
    loss$backward()

    nn_utils_clip_grad_norm_(policy$parameters, max_norm = 1)

    opt$step()

    # ===== UPDATE STATE =====
    state <- new_state$detach()

    history[t] <- as.numeric(reward)
  }

  history[!is.finite(history)] <- NA

  rv$rl_history <- history

  showNotification("🤖 RL training finished", type="message")
})

output$rl_plot <- renderPlotly({

  h <- rv$rl_history

  if(is.null(h) || sum(is.finite(h)) < 2){
    return(empty_plot())
  }

  df <- data.frame(
    step = seq_along(h),
    reward = h
  )

  df <- df[is.finite(df$reward), , drop=FALSE]

  if(nrow(df) < 2) return(empty_plot())

  plot_ly(
    df,
    x = ~step,
    y = ~reward,
    type="scatter",
    mode="lines",
    line = list(width = 3)
  )
})
  
  
  
  
}


shinyApp(ui, server)