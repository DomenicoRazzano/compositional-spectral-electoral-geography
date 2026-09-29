# write_audit_outputs.R
# Writes the audit tables produced by the reporting-unit exclusion step.


write_audit_outputs <- function(audit_results, output_dir) {
  
  # Remove stale outputs from previous runs.
  # Otherwise files removed from output_map would remain on disk and create confusion.
  if (dir.exists(output_dir)) {
    fs::dir_delete(output_dir)
  }
  
  fs::dir_create(output_dir)
  
  output_map <- c(
    duplicate_exact_rows = "duplicate_exact_rows.csv",
    duplicate_reporting_list_keys = "duplicate_reporting_list_keys.csv",
    list_vote_anomalies = "list_vote_anomalies.csv",
    excluded_rows_all = "excluded_rows_all.csv",
    excluded_municipal_units = "excluded_municipal_units.csv",
    audit_summary_year = "audit_summary_year.csv",
    audit_summary_reason = "audit_summary_reason.csv",
    audit_summary_size = "audit_summary_size.csv",
    audit_summary_province_raw = "audit_summary_province_raw.csv"
  )
  
  missing_objects <- setdiff(names(output_map), names(audit_results))
  
  if (length(missing_objects) > 0) {
    stop(
      "audit_results is missing required objects: ",
      paste(missing_objects, collapse = ", ")
    )
  }
  
  write_named_csv_list(
    x = audit_results,
    output_dir = output_dir,
    file_map = output_map,
    skip_empty = FALSE
  )
  
  invisible(output_dir)
}