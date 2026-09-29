# save_and_report_validation.R
# Save validation outputs and enforce validation severity logic


save_and_report_validation <- function(
    validation_results,
    validation_dir,
    stop_message,
    warning_message,
    success_message
) {
  
  # Remove stale validation outputs from previous runs; otherwise old issue files may survive a clean validation run.
  if (dir.exists(validation_dir)) {
    fs::dir_delete(validation_dir)
  }
  
  fs::dir_create(validation_dir)
  
  print(validation_results$summary, n = Inf, width = Inf)
  
  main_tables <- list(
    validation_summary = validation_results$summary,
    rows_per_election = validation_results$rows_per_election,
    column_na_summary = validation_results$column_na_summary
  )
  
  main_file_map <- c(
    validation_summary = "validation_summary.csv",
    rows_per_election = "rows_per_election.csv",
    column_na_summary = "column_na_summary.csv"
  )
  
  write_named_csv_list(
    x = main_tables,
    output_dir = validation_dir,
    file_map = main_file_map,
    skip_empty = FALSE
  )
  
  if (!is.null(validation_results$issue_tables)) {
    
    issue_file_map <- stats::setNames(
      paste0(names(validation_results$issue_tables), ".csv"),
      names(validation_results$issue_tables)
    )
    
    write_named_csv_list(
      x = validation_results$issue_tables,
      output_dir = validation_dir,
      file_map = issue_file_map,
      skip_empty = TRUE
    )
  }
  
  critical_checks <- validation_results$summary |>
    dplyr::filter(
      .data$stop_pipeline,
      .data$n_problem_rows > 0
    )
  
  warning_checks <- validation_results$summary |>
    dplyr::filter(
      .data$severity == "warning",
      .data$n_problem_rows > 0
    )
  
  if (nrow(critical_checks) > 0) {
    stop(
      stop_message,
      " See validation files in: ", validation_dir,
      "\nFailed blocking checks: ",
      paste(critical_checks$check_name, collapse = ", ")
    )
  }
  
  if (nrow(warning_checks) > 0) {
    warning(
      warning_message,
      " See validation files in: ", validation_dir,
      "\nWarning checks: ",
      paste(warning_checks$check_name, collapse = ", ")
    )
  } else {
    message(success_message)
  }
  
  invisible(
    list(
      critical_checks = critical_checks,
      warning_checks = warning_checks
    )
  )
}