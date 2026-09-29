# project_io.R
# Small I/O helpers used by the data-construction scripts:
#   load_tabular_file()
#   write_project_csv()
#   write_project_rds()
#   write_named_csv_list()



load_tabular_file <- function(
    file_path,
    delimiter = ";",
    locale = readr::locale(encoding = "UTF-8")
) {
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext == "rds") {
    
    obj <- readRDS(file_path)
    
  } else if (ext %in% c("rdata", "rda")) {
    
    tmp_env <- new.env(parent = emptyenv())
    loaded_names <- load(file_path, envir = tmp_env)
    
    if (length(loaded_names) == 0) {
      stop("No objects found in file: ", basename(file_path))
    }
    
    if (length(loaded_names) > 1) {
      stop(
        "Multiple objects found in ",
        basename(file_path),
        ": ",
        paste(loaded_names, collapse = ", ")
      )
    }
    
    obj <- tmp_env[[loaded_names[1]]]
    
  } else if (ext == "csv") {
    
    obj <- readr::read_csv(
      file_path,
      show_col_types = FALSE,
      trim_ws = TRUE,
      guess_max = 10000,
      locale = locale
    )
    
    # Some administrative CSV files are actually semicolon-separated
    if (
      ncol(obj) == 1 &&
      any(stringr::str_detect(obj[[1]], ";"), na.rm = TRUE)
    ) {
      obj <- readr::read_delim(
        file = file_path,
        delim = ";",
        show_col_types = FALSE,
        trim_ws = TRUE,
        guess_max = 10000,
        locale = locale
      )
    }
    
  } else if (ext == "tsv") {
    
    obj <- readr::read_tsv(
      file_path,
      show_col_types = FALSE,
      trim_ws = TRUE,
      guess_max = 10000,
      locale = locale
    )
    
  } else if (ext == "txt") {
    
    obj <- readr::read_delim(
      file = file_path,
      delim = delimiter,
      show_col_types = FALSE,
      trim_ws = TRUE,
      guess_max = 10000,
      locale = locale
    )
    
  } else {
    
    stop("Unsupported file extension for file: ", basename(file_path))
  }
  
  if (!inherits(obj, "data.frame")) {
    stop("Loaded object is not tabular: ", basename(file_path))
  }
  
  obj
}



write_project_csv <- function(x, path) {
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  readr::write_csv(x, path)
  
  #message("Wrote CSV: ", path)
  
  invisible(path)
}



write_project_rds <- function(x, path) {
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(x, path)
  
   # message("Wrote RDS: ", path)
  
  invisible(path)
}



# Write a named list of data frames as CSV files

write_named_csv_list <- function(
    x,
    output_dir,
    file_map = NULL,
    skip_empty = FALSE
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(file_map)) {
    if (is.null(names(x)) || any(names(x) == "")) {
      stop("x must be a named list if file_map is not supplied.")
    }
    
    file_map <- stats::setNames(
      paste0(names(x), ".csv"),
      names(x)
    )
  }
  
  purrr::iwalk(file_map, function(file_name, object_name) {
    obj <- x[[object_name]]
    
    if (is.null(obj)) {
      return(invisible(NULL))
    }
    
    if (!inherits(obj, "data.frame")) {
      stop("Object is not tabular: ", object_name)
    }
    
    if (skip_empty && nrow(obj) == 0) {
      return(invisible(NULL))
    }
    
    write_project_csv(
      obj,
      file.path(output_dir, file_name)
    )
  })
  
  invisible(output_dir)
}