# =========================================================
# 🔥 SETUP (SELF-HEALING + PARETO READY)
# =========================================================

# ---------------------------------------------------------
# 📦 REQUIRED PACKAGES
# ---------------------------------------------------------
required_packages <- c(

  # core
  "shiny","data.table","dplyr","tidyr","readr","stringr","purrr","tibble",

  # ML
  "xgboost","Matrix","digest",

  # viz
  "plotly","ggplot2","DT","htmlwidgets",

  # preprocessing
  "fastDummies",

  # async
  "future","promises",

  # report
  "zip",

  # optional
  "umap","Rtsne","corrplot","pheatmap"
)

# ---------------------------------------------------------
# 🔧 SAFE INSTALL
# ---------------------------------------------------------
safe_install_and_load <- function(pkg){

  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {

    message(paste("📦 Instalacja:", pkg))

    tryCatch({

      install.packages(pkg, dependencies = TRUE)

      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        warning(paste("⚠️ Nie można załadować:", pkg))
      } else {
        message(paste("✔ OK:", pkg))
      }

    }, error = function(e) {

      warning(paste("⚠️ Install fail:", pkg, "|", e$message))
    })
  }
}

# ---------------------------------------------------------
# 📦 INSTALL ALL
# ---------------------------------------------------------
for (pkg in required_packages) {
  safe_install_and_load(pkg)
}

# ---------------------------------------------------------
# ⚡ ASYNC CONFIG (FIXED)
# ---------------------------------------------------------
init_async <- function() {

  tryCatch({

    if (requireNamespace("future", quietly = TRUE)) {

      library(future)
      library(promises)

      # 🔥 tylko JEDEN plan
      future::plan(future::multisession)

      message("✔ Async: multisession")

    } else {

      warning("⚠️ Brak 'future' – async OFF")
    }

  }, error = function(e) {

    warning(paste("⚠️ Async error:", e$message))
  })
}

init_async()

# ---------------------------------------------------------
# 📁 PROJECT STRUCTURE
# ---------------------------------------------------------
ensure_dir <- function(path){

  if (!dir.exists(path)) {

    success <- tryCatch({
      dir.create(path, recursive = TRUE)
    }, error = function(e) FALSE)

    if (!success) {
      warning(paste("⚠️ Nie można utworzyć:", path))
      return(FALSE)
    }
  }

  return(TRUE)
}

dirs <- c(
  "ANDROGENESIS_AI",
  "ANDROGENESIS_AI/data",
  "ANDROGENESIS_AI/models",
  "ANDROGENESIS_AI/cache",
  "ANDROGENESIS_AI/reports",
  "ANDROGENESIS_AI/debug"
)

for (d in dirs) {
  ensure_dir(d)
}

# ---------------------------------------------------------
# 💾 CACHE INIT
# ---------------------------------------------------------
init_cache <- function() {

  cache_dir <- "ANDROGENESIS_AI/cache"

  if (ensure_dir(cache_dir)) {
    message("✔ Cache:", normalizePath(cache_dir))
  }
}

init_cache()

# ---------------------------------------------------------
# 🔍 SYSTEM DIAGNOSTICS
# ---------------------------------------------------------
print_system_info <- function() {

  message("===================================")
  message("✔ ANDROGENESIS AI – START")
  message("-----------------------------------")

  message("R: ", R.version.string)

  cores <- tryCatch(parallel::detectCores(), error = function(e) NA)
  message("Cores: ", cores)

  message("WD: ", getwd())

  message("-----------------------------------")

  # 🔥 kluczowe dla pipeline B
  critical <- c("xgboost","shiny","plotly","future")

  for (pkg in critical) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      message("✔ ", pkg, " OK")
    } else {
      warning("⚠️ ", pkg, " BRAK")
    }
  }

  message("-----------------------------------")
  message("✔ TRYB:")
  message("→ Pareto optimization")
  message("→ multi-model (multi-output)")
  message("→ self-healing packages")
  message("→ async (jeśli dostępne)")
  message("===================================")
}

print_system_info()

# ---------------------------------------------------------
# 🚨 XGBOOST CHECK
# ---------------------------------------------------------
check_xgboost <- function() {

  if (!requireNamespace("xgboost", quietly = TRUE)) {

    warning("⚠️ xgboost BRAK")

    make_path <- Sys.which("make")

    if (make_path == "") {
      warning("⚠️ Brak Rtools")
    } else {
      message("✔ Rtools:", make_path)
    }

  } else {
    message("✔ xgboost OK")
  }
}

check_xgboost()