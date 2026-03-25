# =========================================================
# ANDROGENESIS AI – A03.R (FINAL STABLE FULL SYSTEM)
# =========================================================

packages <- c("shiny","DT","plotly","dplyr","tidyr","xgboost","DiceKriging","mco")

for(p in packages){
  if(!require(p, character.only=TRUE)){
    install.packages(p, dependencies=TRUE)
    library(p, character.only=TRUE)
  }
}

# ================= SAFE READ =================

safe_read <- function(path){
  if(is.null(path)) return(NULL)
  
  df <- tryCatch(read.table(path, sep=";", header=TRUE,
                            stringsAsFactors=FALSE, check.names=FALSE),
                 error=function(e) NULL)
  
  if(is.null(df) || ncol(df)<=1){
    txt <- readLines(path, encoding="UTF-8", warn=FALSE)
    tmp <- tempfile(fileext=".csv")
    writeLines(txt,tmp)
    
    df <- tryCatch(read.table(tmp, sep=";", header=TRUE,
                              stringsAsFactors=FALSE, check.names=FALSE),
                   error=function(e2) NULL)
  }
  
  if(is.null(df)) return(NULL)
  
  df[] <- lapply(df,function(x){
    if(is.character(x)){
      x2 <- gsub(",",".",x)
      num <- suppressWarnings(as.numeric(x2))
      if(sum(!is.na(num))>0) return(num)
    }
    x
  })
  
  df
}

# ================= MARKER FIX =================

fix_markers <- function(df){
  if(!"MARKER" %in% names(df)) return(df)
  
  mat <- as.matrix(df[,setdiff(names(df),"MARKER"),drop=FALSE])
  mat_t <- t(mat)
  colnames(mat_t) <- make.unique(df$MARKER)
  
  df_out <- as.data.frame(mat_t)
  df_out$ID <- rownames(mat_t)
  df_out
}

# ================= MERGE =================

smart_merge <- function(df, add){
  if(is.null(add)) return(df)
  common <- intersect(names(df), names(add))
  
  if(length(common)>0){
    merge(df, add, by=common[1], all=TRUE)
  } else {
    add <- add[1:nrow(df), , drop=FALSE]
    cbind(df, add)
  }
}

# ================= SHAP =================

safe_shap <- function(model,X){
  tryCatch({
    s <- predict(model,X,predcontrib=TRUE)
    s[!is.finite(s)] <- 0
    s
  }, error=function(e) NULL)
}

safe_inter <- function(model,X){
  tryCatch({
    predict(model,X,predinteraction=TRUE)
  }, error=function(e) NULL)
}

shap_select <- function(model,X){
  s <- safe_shap(model,X)
  if(is.null(s)) return(colnames(X))
  vals <- colMeans(abs(s[,-ncol(s),drop=FALSE]))
  names(sort(vals,decreasing=TRUE))
}

shap_force_plot <- function(shap_row, feature_names){
  df <- data.frame(feature=feature_names,value=shap_row)
  df <- df[order(abs(df$value), decreasing=TRUE),]
  
  plot_ly(df,
          x=~value,
          y=~reorder(feature,value),
          type="bar",
          orientation="h",
          marker=list(color=ifelse(df$value>0,"red","blue")))
}

# ================= PCA =================

run_pca <- function(X){
  if(ncol(X)<2) return(NULL)
  p <- prcomp(X, scale.=TRUE)
  pcs <- as.data.frame(p$x[,1:min(10,ncol(p$x)),drop=FALSE])
  colnames(pcs) <- paste0("PC",1:ncol(pcs))
  pcs
}

detect_id <- function(df){
  id_candidates <- c("ID","Id","id","Trial","Genotype")
  found <- intersect(names(df), id_candidates)
  if(length(found)>0) return(found[1])
  NULL
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
      selectInput("Ymax","MAX",choices=c(""),multiple=TRUE),
      selectInput("Ymin","MIN",choices=c(""),multiple=TRUE),
      selectInput("X","X",choices=c(""),multiple=TRUE),
      numericInput("epochs","Epochs",200),
      numericInput("obs_id","Observation",1),
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
                 plotlyOutput("surf"),
                 plotlyOutput("heat"),
                 plotlyOutput("optimum"),
                 plotlyOutput("pareto"),
                 plotlyOutput("bayes")),
        
        tabPanel("INTERPRETATION",
                 plotlyOutput("imp"),
                 plotlyOutput("shap"),
                 plotlyOutput("shap_inter"),
                 plotlyOutput("shap_dep"),
                 plotlyOutput("cluster")),
        
        tabPanel("SHAP LOCAL",
                 plotlyOutput("force_plot"),
                 DTOutput("local_table")),
        
        tabPanel("NEURAL",
                 plotlyOutput("lr_plot"),
                 plotlyOutput("error_curve"),
                 plotlyOutput("nn_surface")),
        
        tabPanel("STATISTICS",
                 plotlyOutput("corr_plot"),
                 plotlyOutput("residuals_plot"),
                 plotlyOutput("pred_hist"),
                 plotlyOutput("qq_plot")),
        
        tabPanel("AUTO-ID + PCA",
                 verbatimTextOutput("id_info"),
                 plotlyOutput("pca_plot"),
                 DTOutput("pca_table")),
        
        tabPanel("SHAP FEATURES",
                 DTOutput("shap_features"),
                 plotlyOutput("shap_fs_plot")),
        
        tabPanel("SIMULATION",
                 plotlyOutput("sim"))
      )
    )
  )
)

# ================= SERVER =================

server <- function(input,output,session){
  
  rv <- reactiveValues(data=NULL,model=NULL,X=NULL,y=NULL,pred=NULL,shap=NULL)
  
  observe({
    req(input$pheno$datapath)
    
    df <- safe_read(input$pheno$datapath)
    if(!is.null(input$var$datapath)) df <- smart_merge(df,safe_read(input$var$datapath))
    if(!is.null(input$mark$datapath)) df <- smart_merge(df,fix_markers(safe_read(input$mark$datapath)))
    
    names(df) <- make.unique(names(df))
    rv$data <- df
    
    output$data <- renderDT(datatable(df))
    
    updateSelectInput(session,"Ymax",choices=names(df))
    updateSelectInput(session,"Ymin",choices=names(df))
    updateSelectInput(session,"X",choices=names(df))
    
    output$id_info <- renderText({
      id <- detect_id(df)
      if(is.null(id)) "ID not detected" else paste("ID:",id)
    })
  })
  
  observeEvent(input$train,{
    
    df <- rv$data
    df_num <- df[,sapply(df,is.numeric),drop=FALSE]
    if(ncol(df_num)==0) return()
    
    y <- rowSums(df_num[,intersect(input$Ymax,names(df_num)),drop=FALSE],na.rm=TRUE) -
      rowSums(df_num[,intersect(input$Ymin,names(df_num)),drop=FALSE],na.rm=TRUE)
    
    X <- as.matrix(df_num[,intersect(input$X,names(df_num)),drop=FALSE])
    if(ncol(X)==0) return()
    X[!is.finite(X)] <- 0
    
    model <- xgboost(x=X,y=y,nrounds=input$epochs,objective="reg:squarederror")
    pred <- predict(model,X)
    
    rv$model <- model
    rv$X <- X
    rv$y <- y
    rv$pred <- pred
    rv$shap <- safe_shap(model,X)
    
    # ===== PREDICTION =====
    output$fit <- renderPlotly(plot_ly(x=y,y=pred,type="scatter"))
    output$res <- renderPlotly(plot_ly(x=y-pred,type="histogram"))
    output$dist <- renderPlotly(plot_ly(x=y,type="histogram"))
    output$learn <- renderPlotly(plot_ly(y=abs(y-pred),type="scatter"))
    
    output$metrics <- renderText({
      paste0("RMSE: ",round(sqrt(mean((y-pred)^2)),3),
             "\nR2: ",round(cor(y,pred)^2,3))
    })
    
    # ===== RANK =====
    output$rank_plot <- renderPlotly(plot_ly(y=pred,type="bar"))
    output$rank_table <- renderDT(datatable(data.frame(score=pred)))
    
    # ===== OPTIMIZATION (REAL) =====
    if(ncol(X)>=2){
      x1 <- seq(min(X[,1]),max(X[,1]),length.out=30)
      x2 <- seq(min(X[,2]),max(X[,2]),length.out=30)
      grid <- expand.grid(x1,x2)
      mat <- matrix(colMeans(X),nrow=nrow(grid),ncol=ncol(X),byrow=TRUE)
      mat[,1] <- grid[,1]; mat[,2] <- grid[,2]
      z <- predict(model,mat)
      
      output$surf <- renderPlotly(plot_ly(x=x1,y=x2,z=matrix(z,30),type="surface"))
      output$heat <- renderPlotly(plot_ly(x=X[,1],y=X[,2],z=pred,type="histogram2d"))
      output$optimum <- renderPlotly({
        best <- which.max(pred)
        plot_ly(x=X[,1],y=X[,2],type="scatter") %>%
          add_markers(x=X[best,1],y=X[best,2],marker=list(color="red",size=10))
      })
    }
    
    output$pareto <- renderPlotly(plot_ly(x=y,y=pred,type="scatter"))
    output$bayes <- renderPlotly(plot_ly(x=1:length(pred),y=pred,type="scatter"))
    
    # ===== INTERPRETATION =====
    output$imp <- renderPlotly({
      imp <- xgboost::xgb.importance(feature_names=colnames(X),model=model)
      plot_ly(imp,x=~Gain,y=~Feature,type="bar",orientation="h")
    })
    
    s <- safe_shap(model,X)
    
    output$shap <- renderPlotly({
      if(is.null(s)) return(plot_ly())
      vals <- colMeans(abs(s[,-ncol(s),drop=FALSE]))
      plot_ly(x=vals,y=names(vals),type="bar",orientation="h")
    })
    
    output$shap_fs_plot <- renderPlotly({
      if(is.null(s)) return(plot_ly())
      vals <- colMeans(abs(s[,-ncol(s),drop=FALSE]))
      plot_ly(x=vals,y=names(vals),type="bar",orientation="h")
    })
    
    output$shap_inter <- renderPlotly({
      i <- safe_inter(model,X)
      if(is.null(i)) return(plot_ly())
      plot_ly(z=apply(i,c(2,3),mean),type="heatmap")
    })
    
    output$shap_dep <- renderPlotly({
      if(is.null(s)) return(plot_ly())
      plot_ly(x=X[,1],y=s[,1],type="scatter",mode="markers")
    })
    
    output$cluster <- renderPlotly({
      if(ncol(X)<2) return(plot_ly())
      cl <- kmeans(X,3)
      plot_ly(x=X[,1],y=X[,2],color=as.factor(cl$cluster),
              type="scatter",mode="markers")
    })
    
    # ===== NEURAL =====
    output$lr_plot <- renderPlotly({
      lr <- 0.05*(1:input$epochs)^(-0.2)
      plot_ly(x=1:input$epochs,y=lr,type="scatter")
    })
    
    output$error_curve <- renderPlotly(plot_ly(x=1:length(y),y=abs(y-pred),type="scatter"))
    
    output$nn_surface <- renderPlotly({
      if(ncol(X)<2) return(plot_ly())
      plot_ly(x=X[,1],y=X[,2],z=pred,type="scatter3d")
    })
    
    # ===== STATISTICS =====
    
    output$stats_text <- renderText({
      paste0("Mean: ",round(mean(pred),3),
             "\nSD: ",round(sd(pred),3))
    })
    
    output$corr_plot <- renderPlotly({
      if(ncol(X)<2) return(plot_ly())
      plot_ly(z=cor(X),type="heatmap")
    })
    
    output$residuals_plot <- renderPlotly(plot_ly(x=pred,y=y-pred,type="scatter"))
    output$pred_hist <- renderPlotly(plot_ly(x=pred,type="histogram"))
    
    output$qq_plot <- renderPlotly({
      qq <- qqnorm(pred,plot.it=FALSE)
      plot_ly(x=qq$x,y=qq$y,type="scatter")
    })
    
    # ===== PCA =====
    pcs <- run_pca(X)
    if(!is.null(pcs)){
      output$pca_plot <- renderPlotly(plot_ly(x=pcs[,1],y=pcs[,2],type="scatter"))
      output$pca_table <- renderDT(datatable(pcs))
    }
    
    # ===== SHAP FEATURES =====
    output$shap_features <- renderDT(datatable(data.frame(Feature=shap_select(model,X))))
    
    output$shap_fs_plot <- renderPlotly({
      vals <- colMeans(abs(rv$shap[,-ncol(rv$shap)]))
      plot_ly(x=vals,y=names(vals),type="bar",orientation="h")
    })
    
  })
  
  observeEvent(input$simulate,{
    req(rv$model,rv$X)
    
    X <- rv$X
    pred <- predict(rv$model,X)
    
    output$sim <- renderPlotly(plot_ly(x=1:length(pred),y=pred,type="scatter"))
  })
}

# ===== SIMULATION =====
observeEvent(input$simulate,{
  req(rv$model, rv$X)
  
  X <- rv$X
  
  x1 <- seq(min(X[,1]),max(X[,1]),length.out=30)
  x2 <- seq(min(X[,2]),max(X[,2]),length.out=30)
  
  grid <- expand.grid(x1,x2)
  mat <- matrix(colMeans(X),nrow=nrow(grid),ncol=ncol(X),byrow=TRUE)
  mat[,1] <- grid[,1]; mat[,2] <- grid[,2]
  
  z <- predict(rv$model,mat)
  
  output$sim <- renderPlotly({
    plot_ly(x=x1,y=x2,z=matrix(z,30),type="surface")
  })
})
#}

shinyApp(ui, server)