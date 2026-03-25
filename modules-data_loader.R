# =========================================================
# 🔥 AUTO PACKAGE LOADER (PRODUCTION SAFE)
# =========================================================

safe_library <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(c(
  "shiny","shinycssloaders","plotly","DT",
  "xgboost","future","promises","digest",
  "tidyr","dplyr"
), safe_library))

library(shiny)
library(shinycssloaders)
library(plotly)
library(DT)

BASE_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
get_path <- function(...) file.path(BASE_DIR, ...)

source(get_path("utils-helpers.R"), local = TRUE)

# =========================================================
# 🔥 UI
# =========================================================
ui <- fluidPage(

  titlePanel("ANDROGENESIS AI – PARETO OPTIMIZATION"),

  sidebarLayout(
    sidebarPanel(

      fileInput("phenotypes","Phenotypes CSV"),
      actionButton("load_pheno","Load"),

      fileInput("variability","Variability CSV"),
      actionButton("load_var","Load"),

      fileInput("markers","Markers CSV"),
      actionButton("load_mark","Load"),

      hr(),

      checkboxGroupInput("target_vars","CECHY (Y)",choices=NULL),
      checkboxGroupInput("condition_vars","WARUNKI (X)",choices=NULL),

      uiOutput("direction_ui"),

      numericInput("weight_default","Waga",1),

      hr(),

      numericInput("epochs","Epochs",100,min=10),

      actionButton("train_model","Train"),
      actionButton("run_optimization","Optimize"),

      hr(),

      downloadButton("download_report","Report")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("RAW",
                 DTOutput("raw_pheno"),
                 DTOutput("raw_variability"),
                 DTOutput("raw_markers")),
        tabPanel("Data", DTOutput("data_table")),
        tabPanel("Optimization", DTOutput("opt_table")),
        tabPanel("Ranking", DTOutput("ranking_table")),
        tabPanel("Prediction", DTOutput("prediction_table"))
      )
    )
  )
)

# =========================================================
# 🔥 SERVER
# =========================================================
server <- function(input, output, session){

  rv <- reactiveValues()

  # ---------------- LOAD ----------------
  observeEvent(input$load_pheno,{
    df <- as.data.frame(safe_read_csv(input$phenotypes))
    rv$phenotypes_raw <- df
    output$raw_pheno <- renderDT(head(df,4))
  })

  observeEvent(input$load_var,{
    df <- as.data.frame(safe_read_csv(input$variability))
    rv$variability_raw <- df
    output$raw_variability <- renderDT(head(df,4))
  })

  observeEvent(input$load_mark,{
    df <- as.data.frame(safe_read_csv(input$markers))
    rv$markers_raw <- df
    output$raw_markers <- renderDT(head(df,4))
  })

  # ---------------- MERGE ----------------
  observe({

    req(rv$phenotypes_raw)

    df <- rv$phenotypes_raw

    if (!is.null(rv$variability_raw) &&
        "NAZWA_REGENERANTA" %in% colnames(df) &&
        "NAZWA_REGENERANTA" %in% colnames(rv$variability_raw)) {

      df <- merge(df, rv$variability_raw,
                  by="NAZWA_REGENERANTA", all.x=TRUE)
    }

    if (!is.null(rv$markers_raw) &&
        "MARKER" %in% colnames(rv$markers_raw)) {

      markers <- rv$markers_raw

      markers <- tidyr::pivot_longer(markers,
                                     cols=-MARKER,
                                     names_to="NAZWA_REGENERANTA",
                                     values_to="val")

      markers$NAZWA_REGENERANTA <- gsub("A$","",markers$NAZWA_REGENERANTA)

      markers <- tidyr::pivot_wider(markers,
                                   names_from=MARKER,
                                   values_from=val)

      df <- merge(df, markers,
                  by="NAZWA_REGENERANTA", all.x=TRUE)
    }

    df <- df[, unlist(lapply(df, function(x)!is.list(x))), drop=FALSE]

    rv$phenotypes <- df

    updateCheckboxGroupInput(session,"target_vars",choices=colnames(df))
    updateCheckboxGroupInput(session,"condition_vars",choices=colnames(df))

    output$data_table <- renderDT(head(df,10))
  })

  # ---------------- DIRECTION UI ----------------
  output$direction_ui <- renderUI({

    req(input$target_vars)

    tagList(
      lapply(input$target_vars, function(v){

        fluidRow(
          column(6, strong(v)),
          column(6,
                 selectInput(
                   paste0("dir_", v),
                   NULL,
                   c("max","min"),
                   selected="max"
                 )
          )
        )
      })
    )
  })

  # ---------------- CONFIG (FIXED) ----------------
  observe({

    req(input$target_vars)

    vars <- input$target_vars

    directions <- sapply(vars, function(v){
      val <- input[[paste0("dir_", v)]]
      if (is.null(val)) return("max")
      val
    })

    # 🔥 usuń konflikty X vs Y
    conditions <- setdiff(input$condition_vars, vars)

    updateCheckboxGroupInput(session,
                             "condition_vars",
                             choices = setdiff(colnames(rv$phenotypes), vars),
                             selected = conditions)

    config <- data.frame(
      variable = vars,
      direction = directions,
      weight = input$weight_default,
      stringsAsFactors = FALSE
    )

    rv$optimization_config <- config
  })

  # ---------------- MODULES ----------------
  source(get_path("modules-preprocessing.R"), local=TRUE)
  source(get_path("modules-neural_network.R"), local=TRUE)
  source(get_path("modules-optimization.R"), local=TRUE)
  source(get_path("modules-prediction.R"), local=TRUE)
  source(get_path("modules-ranking.R"), local=TRUE)

}

shinyApp(ui,server)