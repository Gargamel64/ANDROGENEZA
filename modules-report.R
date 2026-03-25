# =========================================================
# 🔥 MODULE: REPORT (ZIP + PDF + PARETO + INTERPRETATION)
# =========================================================

library(zip)

# ---------------------------------------------------------
# 🔧 UTILS
# ---------------------------------------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

safe_write_csv <- function(df, path) {
  tryCatch({
    write.csv(df, path, row.names = FALSE)
  }, error = function(e) {
    message(paste("Błąd zapisu:", path, e$message))
  })
}

# ---------------------------------------------------------
# 🔥 PDF GENERATOR (FIXED)
# ---------------------------------------------------------
generate_pdf_report <- function(temp_dir, rv) {

  report_path <- file.path(temp_dir, "report.Rmd")

  rmd_lines <- c(
    "---",
    "title: \"ANDROGENESIS AI REPORT\"",
    "output: pdf_document",
    "---",
    "",
    "# 1. Dane",
    "",
    paste("Liczba obserwacji:", nrow(rv$phenotypes)),
    "",
    "# 2. Pareto",
    "",
    paste("Liczba rozwiązań Pareto:",
          ifelse(is.null(rv$optimization), 0, nrow(rv$optimization))),
    "",
    "```{r}",
    "if (!is.null(rv$optimization)) {",
    "  print(head(rv$optimization))",
    "}",
    "```",
    "",
    "# 3. Rekomendacje",
    "",
    if (!is.null(rv$recommendations_text)) rv$recommendations_text else "Brak rekomendacji",
    "",
    "# 4. Interpretacja",
    "",
    "```{r}",
    "if (!is.null(rv$results)) {",
    "  print(head(rv$results))",
    "}",
    "```"
  )

  writeLines(rmd_lines, report_path)

  tryCatch({
    rmarkdown::render(
      report_path,
      output_file = "report.pdf",
      output_dir = temp_dir,
      quiet = TRUE
    )
  }, error = function(e) {
    message("Błąd PDF:", e$message)
  })
}

# ---------------------------------------------------------
# 🔥 ZIP REPORT
# ---------------------------------------------------------
output$download_report <- downloadHandler(

  filename = function() {
    paste0("ANDROGENESIS_AI_report_", Sys.Date(), ".zip")
  },

  content = function(file) {

    if (is.null(rv$phenotypes)) stop("Brak danych")
    if (is.null(rv$model)) stop("Brak modelu")
    if (is.null(rv$ranking)) stop("Brak rankingu")

    temp_dir <- file.path(tempdir(), paste0("report_", as.numeric(Sys.time())))
    dir.create(temp_dir, recursive = TRUE)

    # -----------------------------------------------------
    # 📊 DATA
    # -----------------------------------------------------
    safe_write_csv(rv$phenotypes, file.path(temp_dir, "phenotypes.csv"))
    safe_write_csv(rv$ranking, file.path(temp_dir, "ranking.csv"))

    if (!is.null(rv$results)) {
      safe_write_csv(rv$results, file.path(temp_dir, "predictions.csv"))
    }

    # -----------------------------------------------------
    # 🔝 OPTIMIZATION
    # -----------------------------------------------------
    if (!is.null(rv$optimization)) {
      safe_write_csv(rv$optimization, file.path(temp_dir, "optimization.csv"))
    }

    # -----------------------------------------------------
    # 🔧 CONFIG
    # -----------------------------------------------------
    if (!is.null(rv$optimization_config)) {
      safe_write_csv(rv$optimization_config,
                     file.path(temp_dir, "optimization_config.csv"))
    }

    # -----------------------------------------------------
    # 🔬 PREPROCESS
    # -----------------------------------------------------
    if (!is.null(rv$preprocess)) {

      safe_write_csv(
        data.frame(variable = rv$preprocess$condition_cols),
        file.path(temp_dir, "condition_vars.csv")
      )

      safe_write_csv(
        data.frame(variable = rv$preprocess$target_cols),
        file.path(temp_dir, "target_vars.csv")
      )
    }

    # -----------------------------------------------------
    # 🔥 REKOMENDACJE
    # -----------------------------------------------------
    if (!is.null(rv$recommendations)) {
      safe_write_csv(rv$recommendations,
                     file.path(temp_dir, "recommendations.csv"))
    }

    if (!is.null(rv$recommendations_text)) {
      writeLines(rv$recommendations_text,
                 file.path(temp_dir, "recommendations.txt"))
    }

    # -----------------------------------------------------
    # 🔥 PDF
    # -----------------------------------------------------
    generate_pdf_report(temp_dir, rv)

    # -----------------------------------------------------
    # 📦 ZIP
    # -----------------------------------------------------
    files <- list.files(temp_dir, full.names = TRUE)

    zip::zip(zipfile = file, files = files)

  }

)

# ---------------------------------------------------------
# 🔥 DIRECT PDF DOWNLOAD
# ---------------------------------------------------------
output$download_pdf <- downloadHandler(

  filename = function() {
    paste0("ANDROGENESIS_AI_report_", Sys.Date(), ".pdf")
  },

  content = function(file) {

    temp_dir <- tempdir()

    generate_pdf_report(temp_dir, rv)

    pdf_file <- file.path(temp_dir, "report.pdf")

    file.copy(pdf_file, file, overwrite = TRUE)
  }

)