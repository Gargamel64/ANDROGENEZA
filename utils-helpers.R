# =========================================================
# 🔥 UTILS HELPERS (PARETO + MULTI-MODEL SAFE)
# =========================================================

library(digest)

# ---------------------------------------------------------
# 🔧 PATH UTILS
# ---------------------------------------------------------
get_base_path <- function() {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

get_path <- function(...) {
  file.path(get_base_path(), ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# ---------------------------------------------------------
# 📥 SAFE READ
# ---------------------------------------------------------
safe_read_csv <- function(file) {

  tryCatch({

    if (is.null(file)) return(NULL)

    if (!file.exists(file$datapath)) {
      warning("Plik CSV nie istnieje")
      return(NULL)
    }

    df <- readr::read_csv(
      file$datapath,
      show_col_types = FALSE,
      locale = readr::locale(decimal_mark = ",")
    )

    df <- as.data.frame(df)

    return(df)

  }, error = function(e) {

    message("Błąd wczytywania CSV: ", e$message)
    return(NULL)
  })
}

safe_read_rds <- function(path) {

  tryCatch({

    if (is.null(path) || !file.exists(path)) {
      warning(paste("Plik nie istnieje:", path))
      return(NULL)
    }

    readRDS(path)

  }, error = function(e) {

    message("Błąd wczytywania RDS: ", e$message)
    return(NULL)
  })
}

# ---------------------------------------------------------
# 💾 SAFE WRITE
# ---------------------------------------------------------
safe_save_rds <- function(object, path) {

  tryCatch({

    ensure_dir(dirname(path))
    saveRDS(object, path)

  }, error = function(e) {

    message("Błąd zapisu RDS:", e$message)
  })
}

# ---------------------------------------------------------
# 🔥 HASH
# ---------------------------------------------------------
create_hash <- function(...) {
  digest(list(...))
}

# ---------------------------------------------------------
# 🔥 MODEL CACHE (MULTI-MODEL SAFE)
# ---------------------------------------------------------
get_model_cache_path <- function(hash) {
  get_path("ANDROGENESIS_AI", "models", paste0("model_", hash, ".rds"))
}

save_model_cache <- function(model, hash) {

  path <- get_model_cache_path(hash)

  # 🔥 walidacja: model musi być listą
  if (!is.list(model)) {
    warning("Model nie jest listą (multi-output wymagany)")
  }

  safe_save_rds(model, path)

  return(path)
}

load_model_cache <- function(hash) {

  path <- get_model_cache_path(hash)

  if (!file.exists(path)) return(NULL)

  model <- safe_read_rds(path)

  # 🔥 walidacja kompatybilności
  if (!is.list(model)) {
    warning("Cache modelu niekompatybilny (brak multi-model)")
    return(NULL)
  }

  return(model)
}

# ---------------------------------------------------------
# 🔥 PREPROCESS CACHE
# ---------------------------------------------------------
get_preprocess_cache_path <- function(hash) {
  get_path("ANDROGENESIS_AI", "cache", paste0("preprocess_", hash, ".rds"))
}

save_preprocess_cache <- function(preprocess, hash) {

  path <- get_preprocess_cache_path(hash)

  safe_save_rds(preprocess, path)

  return(path)
}

load_preprocess_cache <- function(hash) {

  path <- get_preprocess_cache_path(hash)

  if (!file.exists(path)) return(NULL)

  safe_read_rds(path)
}

# ---------------------------------------------------------
# 🔧 LEGACY COMPAT
# ---------------------------------------------------------
save_model <- function(model, name) {

  path <- get_path("ANDROGENESIS_AI", "models", paste0(name, ".rds"))

  safe_save_rds(model, path)
}

load_model <- function(name) {

  path <- get_path("ANDROGENESIS_AI", "models", paste0(name, ".rds"))

  if (!file.exists(path)) {
    stop(paste("Model nie istnieje:", path))
  }

  readRDS(path)
}