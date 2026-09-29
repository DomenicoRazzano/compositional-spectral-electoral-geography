# 00_setup.R

required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "tibble",
  "ggplot2",
  "fs",
  "compositions"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

path_data_raw <- here::here("05_data", "raw")

if (!dir.exists(path_data_raw)) {
  warning("Raw data directory does not exist: ", path_data_raw)
}

path_data_interim <- here::here("05_data", "interim")
path_data_processed <- here::here("05_data", "processed")
path_data_public <- here::here("05_data", "public")
path_data_lookup <- here::here("05_data", "lookup")
path_data_compositional <- here::here("05_data", "processed", "compositional")

path_outputs_figures <- here::here("07_outputs", "figures")
path_outputs_tables <- here::here("07_outputs", "tables")
path_outputs_tables_compositional <- here::here(
  "07_outputs", "tables", "compositional"
)
path_outputs_figures_compositional <- file.path(
  path_outputs_figures,
  "compositional"
)

fs::dir_create(c(
  path_data_interim,
  path_data_processed,
  path_data_public,
  path_data_lookup,
  path_data_compositional,
  path_outputs_figures,
  path_outputs_tables,
  path_outputs_tables_compositional,
  path_outputs_figures_compositional
))

functions_dir <- here::here("06_code", "functions")
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)

for (f in function_files) {
  tryCatch(
    source(f),
    error = function(e) {
      stop("Error sourcing function file: ", f, "\n", e$message)
    }
  )
}