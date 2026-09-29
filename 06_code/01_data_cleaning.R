# 01_data_cleaning.R
# Builds the pre-territorial processed and analytical Camera datasets.

source(here::here("06_code", "00_setup.R"))

message("Building pre-territorial Camera datasets")


# Raw file catalog

all_files <- list.files(
  path = path_data_raw,
  full.names = TRUE
)

if (length(all_files) == 0) {
  stop("No files found in raw directory: ", path_data_raw)
}


file_index <- tibble::tibble(
  file_path = all_files,
  file_name = basename(all_files),
  file_stub = tools::file_path_sans_ext(basename(all_files))
) |>
  dplyr::mutate(
    file_stub_key = stringr::str_to_lower(.data$file_stub)
  )


catalog <- get_camera_file_catalog() |>
  dplyr::mutate(
    file_stub_key = stringr::str_to_lower(.data$file_stub)
  )

file_index <- dplyr::inner_join(
  file_index,
  catalog,
  by = "file_stub_key"
) |>
  dplyr::transmute(
    file_path = .data$file_path,
    file_name = .data$file_name,
    file_stub = .data$file_stub.x,
    file_stub_key = .data$file_stub_key,
    schema_id = .data$schema_id,
    election_date = .data$election_date
  ) |>
  dplyr::arrange(
    .data$election_date,
    .data$file_stub
  )

missing_expected_files <- catalog |>
  dplyr::filter(!.data$file_stub_key %in% file_index$file_stub_key) |>
  dplyr::pull(.data$file_stub)

if (length(missing_expected_files) > 0) {
  stop(
    "Missing expected files: ",
    paste(missing_expected_files, collapse = ", ")
  )
}

duplicate_catalog_entries <- file_index |>
  dplyr::count(.data$file_stub_key, name = "n_files") |>
  dplyr::filter(.data$n_files > 1)

if (nrow(duplicate_catalog_entries) > 0) {
  stop(
    "Multiple files found for the same expected file stub: ",
    paste(duplicate_catalog_entries$file_stub, collapse = ", ")
  )
}

message("Matched raw files: ", nrow(file_index))


# Ordinary raw files

camera_processed <- purrr::pmap_dfr(
  .l = list(
    fp = file_index$file_path,
    fn = file_index$file_name,
    schema_id = file_index$schema_id
  ),
  .f = function(fp, fn, schema_id) {
    
    raw_obj <- load_tabular_file(fp)
    
    process_camera_dataset(
      df = raw_obj,
      dataset_name = fn,
      schema_id = schema_id
    )
  }
)


# Special electoral supplements:
# Valle d'Aosta is absent from ordinary Mattarellum proportional files.
# Candidate-level uninominal returns are recoded as list-equivalent components.

special_supplements <- build_special_supplements(
  raw_dir = path_data_raw
)

if (nrow(special_supplements) > 0) {
  
  message("Adding supplement rows: ", nrow(special_supplements))
  
  missing_from_supplements <- setdiff(
    names(camera_processed),
    names(special_supplements)
  )
  
  for (col_name in missing_from_supplements) {
    special_supplements[[col_name]] <- NA
  }
  
  missing_from_processed <- setdiff(
    names(special_supplements),
    names(camera_processed)
  )
  
  for (col_name in missing_from_processed) {
    camera_processed[[col_name]] <- NA
  }
  
  camera_processed <- dplyr::bind_rows(
    camera_processed,
    special_supplements |>
      dplyr::select(dplyr::all_of(names(camera_processed)))
  )
}


write_project_rds(
  camera_processed,
  here::here("05_data", "processed", "camera_all_processed.rds")
)

write_project_csv(
  camera_processed,
  here::here("05_data", "processed", "camera_all_processed.csv")
)


# Processed validation

validation_dir <- here::here(
  "05_data",
  "processed",
  "validation_camera_processed"
)

validation_results <- validate_dataset(
  df = camera_processed,
  expected_dates = sort(unique(catalog$election_date)),
  stage = "processed",
  post_territorial = FALSE
)

message("Processed validation summary:")
save_and_report_validation(
  validation_results = validation_results,
  validation_dir = validation_dir,
  stop_message = "Processed-data validation failed.",
  warning_message = "Processed-data validation found issues to be handled by the reporting-unit audit.",
  success_message = "Processed-data validation completed with no blocking errors."
)


# Reporting-unit audit

exclusion_audit_dir <- here::here(
  "05_data",
  "processed",
  "exclusion_audit_camera"
)

audit_results <- audit_reporting_units(camera_processed)

# Recompute residual null ballots after excluded reporting units are removed.
camera_analytical <- derive_null_ballots(audit_results$analytical_data)
audit_results$analytical_data <- camera_analytical


write_project_rds(
  camera_analytical,
  here::here("05_data", "processed", "camera_all_analytical.rds")
)

write_project_csv(
  camera_analytical,
  here::here("05_data", "processed", "camera_all_analytical.csv")
)


write_audit_outputs(
  audit_results,
  exclusion_audit_dir
)

plot_audit_results(
  audit_results,
  exclusion_audit_dir
)


# Analytical validation

validation_dir <- here::here(
  "05_data",
  "processed",
  "validation_camera_analytical"
)

validation_results_analytical <- validate_dataset(
  df = camera_analytical,
  expected_dates = sort(unique(catalog$election_date)),
  stage = "analytical",
  post_territorial = FALSE
)

message("Analytical validation summary:")
save_and_report_validation(
  validation_results = validation_results_analytical,
  validation_dir = validation_dir,
  stop_message = "Final validation failed on the analytical dataset.",
  warning_message = "Final validation completed with warning-level issues.",
  success_message = "Final validation completed successfully with no critical errors."
)


message("Processed rows: ", nrow(camera_processed))
message("Analytical rows: ", nrow(camera_analytical))
message(
  "Excluded reporting units: ",
  sum(audit_results$audit_summary_year$excluded_reporting_units, na.rm = TRUE)
)
message("Done: pre-territorial Camera datasets")