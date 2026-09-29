# territorial_apply_lookup.R
# Apply harmonized province lookup to the analytical dataset.


load_province_lookup <- function(lookup_path) {
  if (!file.exists(lookup_path)) {
    stop("Missing province lookup: ", lookup_path)
  }
  
  readr::read_csv(
    lookup_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
}


normalize_territorial_join_keys <- function(df, join_key) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(join_key),
        ~ dplyr::na_if(stringr::str_squish(as.character(.x)), "")
      ),
      election_date = as.character(.data$election_date),
      municipality = stringr::str_to_upper(.data$municipality),
      province_raw = stringr::str_to_upper(.data$province_raw),
      district_raw = stringr::str_to_upper(.data$district_raw),
      plurinominal_college_raw =
        stringr::str_to_upper(.data$plurinominal_college_raw),
      uninominal_college_raw =
        stringr::str_to_upper(.data$uninominal_college_raw)
    )
}


check_lookup_ready_for_application <- function(lookup, join_key) {
  
  required_lookup <- c(
    join_key,
    "province_actual",
    "province_harmonized",
    "territorial_match_status"
  )
  
  missing_lookup <- setdiff(required_lookup, names(lookup))
  
  if (length(missing_lookup) > 0) {
    stop(
      "Province lookup is missing required columns: ",
      paste(missing_lookup, collapse = ", ")
    )
  }
  
  duplicated_lookup_keys <- lookup |>
    dplyr::count(
      dplyr::across(dplyr::all_of(join_key)),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicated_lookup_keys) > 0) {
    print(duplicated_lookup_keys, n = 100)
    stop(
      "province_lookup_actual_harmonized.csv contains duplicate join keys."
    )
  }
  
  unresolved_lookup <- lookup |>
    dplyr::filter(
      is.na(.data$territorial_match_status) |
        .data$territorial_match_status != "harmonized_resolved"
    )
  
  if (nrow(unresolved_lookup) > 0) {
    print(
      unresolved_lookup |>
        dplyr::select(
          dplyr::all_of(join_key),
          province_actual,
          province_harmonized,
          territorial_match_status
        ),
      n = 100
    )
    stop(
      "Cannot apply harmonized provinces: unresolved lookup rows remain."
    )
  }
  
  missing_lookup_provinces <- lookup |>
    dplyr::filter(
      is.na(.data$province_actual) |
        is.na(.data$province_harmonized)
    )
  
  if (nrow(missing_lookup_provinces) > 0) {
    print(
      missing_lookup_provinces |>
        dplyr::select(
          dplyr::all_of(join_key),
          province_actual,
          province_harmonized,
          territorial_match_status
        ),
      n = 100
    )
    stop(
      "Lookup contains resolved rows with missing actual or harmonized province."
    )
  }
  
  invisible(TRUE)
}


check_required_analytical_columns_for_lookup <- function(analytical, join_key) {
  
  required_analytical <- c(
    join_key,
    "year",
    "source_dataset",
    "province_raw_source",
    "registered_voters",
    "voters",
    "abstentions",
    "blank_ballots",
    "null_ballots",
    "list_name",
    "list_votes"
  )
  
  missing_analytical <- setdiff(required_analytical, names(analytical))
  
  if (length(missing_analytical) > 0) {
    stop(
      "Analytical dataset is missing required columns: ",
      paste(missing_analytical, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}


check_accounting_after_territorial_join <- function(harmonized) {
  
  reporting_unit_key <- c(
    "election_date",
    "source_dataset",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_raw",
    "municipality"
  )
  
  accounting_check <- harmonized |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(reporting_unit_key))
    ) |>
    dplyr::summarise(
      registered_voters = dplyr::first(.data$registered_voters),
      voters = dplyr::first(.data$voters),
      abstentions = dplyr::first(.data$abstentions),
      blank_ballots = dplyr::first(.data$blank_ballots),
      null_ballots = dplyr::first(.data$null_ballots),
      total_list_votes = sum(.data$list_votes, na.rm = TRUE),
      turnout_gap = .data$registered_voters -
        .data$voters -
        .data$abstentions,
      ballot_gap = .data$voters -
        .data$blank_ballots -
        .data$null_ballots -
        .data$total_list_votes,
      .groups = "drop"
    )
  
  bad_accounting <- accounting_check |>
    dplyr::filter(
      abs(.data$turnout_gap) > 1e-6 |
        abs(.data$ballot_gap) > 1e-6 |
        .data$registered_voters < 0 |
        .data$voters < 0 |
        .data$abstentions < 0 |
        .data$blank_ballots < 0 |
        .data$null_ballots < 0 |
        .data$total_list_votes < 0
    )
  
  if (nrow(bad_accounting) > 0) {
    print(bad_accounting, n = 100)
    stop(
      "Accounting integrity failed after harmonized province assignment."
    )
  }
  
  invisible(TRUE)
}


apply_harmonized_province_lookup <- function(
    analytical,
    lookup,
    unmatched_path = NULL
) {
  
  join_key <- territorial_lookup_key()
  
  check_required_analytical_columns_for_lookup(analytical, join_key)
  
  analytical <- normalize_territorial_join_keys(analytical, join_key)
  lookup <- normalize_territorial_join_keys(lookup, join_key)
  
  check_lookup_ready_for_application(lookup, join_key)
  
  lookup_append_candidates <- c(
    join_key,
    "province_actual",
    "province_actual_code",
    "province_actual_abbrev",
    "region_actual",
    "province_actual_source",
    "actual_match_status",
    "municipality_situas",
    "municipality_code_alphanumeric",
    "municipality_code_numeric",
    "cadastral_code",
    "municipality_target",
    "municipality_code_target",
    "province_harmonized",
    "province_harmonized_code",
    "province_harmonized_abbrev",
    "region_harmonized",
    "province_harmonized_frame_date",
    "province_harmonized_source",
    "territorial_match_status",
    "territorial_match_note"
  )
  
  lookup_append_cols <- intersect(
    lookup_append_candidates,
    names(lookup)
  )
  
  lookup_for_join <- lookup |>
    dplyr::select(dplyr::all_of(lookup_append_cols))
  
  stale_columns <- intersect(
    setdiff(lookup_append_cols, join_key),
    names(analytical)
  )
  
  if (length(stale_columns) > 0) {
    message("Removing stale territorial columns")
    
    analytical <- analytical |>
      dplyr::select(-dplyr::all_of(stale_columns))
  }
  
  if ("province" %in% names(analytical)) {
    message("Removing stale province alias column")
    
    analytical <- analytical |>
      dplyr::select(-dplyr::all_of("province"))
  }
  
  n_rows_before <- nrow(analytical)
  
  harmonized <- analytical |>
    dplyr::left_join(
      lookup_for_join,
      by = join_key,
      relationship = "many-to-one",
      na_matches = "na"
    )
  
  n_rows_after <- nrow(harmonized)
  
  if (n_rows_after != n_rows_before) {
    stop(
      "Row count changed after applying harmonized province lookup. ",
      "Rows before: ", n_rows_before,
      "; rows after: ", n_rows_after,
      "."
    )
  }
  
  unmatched_rows <- harmonized |>
    dplyr::filter(
      is.na(.data$province_actual) |
        is.na(.data$province_harmonized) |
        is.na(.data$territorial_match_status)
    )
  
  if (nrow(unmatched_rows) > 0) {
    
    if (!is.null(unmatched_path)) {
      write_project_csv(
        unmatched_rows |>
          dplyr::select(
            dplyr::all_of(join_key),
            source_dataset,
            province_raw_source,
            list_name,
            list_votes
          ) |>
          dplyr::distinct(),
        unmatched_path
      )
    }
    
    stop("Some analytical rows did not match the harmonized province lookup.")
  }
  
  bad_status_rows <- harmonized |>
    dplyr::filter(.data$territorial_match_status != "harmonized_resolved")
  
  if (nrow(bad_status_rows) > 0) {
    print(
      bad_status_rows |>
        dplyr::select(
          dplyr::all_of(join_key),
          province_actual,
          province_harmonized,
          territorial_match_status
        ) |>
        dplyr::distinct(),
      n = 100
    )
    
    stop(
      "Some joined rows have territorial_match_status != harmonized_resolved."
    )
  }
  
  if (any(is.na(harmonized$province_harmonized))) {
    stop("Final harmonized province contains NA values.")
  }
  
  check_accounting_after_territorial_join(harmonized)
  
  attr(harmonized, "territorial_application") <- list(
    input_rows = n_rows_before,
    output_rows = n_rows_after,
    n_unmatched_rows = nrow(unmatched_rows)
  )
  
  harmonized
}


build_harmonized_application_summaries <- function(
    harmonized,
    lookup,
    output_rds_path,
    output_csv_path
) {
  
  application <- attr(harmonized, "territorial_application")
  
  application_summary <- tibble::tibble(
    step = "03_apply_harmonized_provinces",
    input_rows = application$input_rows,
    output_rows = application$output_rows,
    n_lookup_rows = nrow(lookup),
    n_unmatched_rows = application$n_unmatched_rows,
    n_distinct_elections = dplyr::n_distinct(harmonized$election_date),
    n_distinct_harmonized_provinces_overall =
      dplyr::n_distinct(harmonized$province_harmonized),
    min_year = min(harmonized$year, na.rm = TRUE),
    max_year = max(harmonized$year, na.rm = TRUE),
    output_rds = output_rds_path,
    output_csv = output_csv_path
  )
  
  node_counts <- harmonized |>
    dplyr::distinct(
      .data$election_date,
      .data$year,
      .data$province_harmonized
    ) |>
    dplyr::count(
      .data$election_date,
      .data$year,
      name = "n_harmonized_provinces"
    ) |>
    dplyr::arrange(.data$year)
  
  coverage_flags <- node_counts |>
    dplyr::mutate(
      coverage_note = dplyr::case_when(
        .data$election_date == "19480418" &
          .data$n_harmonized_provinces == 105L ~
          "1948 has 105 harmonized provinces: Gorizia is missing from the source; Trieste is institutionally absent.",
        .data$election_date == "19530607" &
          .data$n_harmonized_provinces == 106L ~
          "1953 has 106 harmonized provinces: Trieste is institutionally absent.",
        .data$n_harmonized_provinces == 107L ~
          "Full 107-province harmonized target coverage.",
        TRUE ~
          "Unexpected province coverage count; inspect source data and lookup."
      )
    )
  
  source_summary <- harmonized |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(territorial_lookup_key())),
      province_actual,
      province_harmonized,
      territorial_match_status,
      dplyr::across(dplyr::any_of(c(
        "province_actual_source",
        "province_harmonized_source"
      )))
    ) |>
    dplyr::count(
      .data$territorial_match_status,
      dplyr::across(dplyr::any_of(c(
        "province_actual_source",
        "province_harmonized_source"
      ))),
      name = "n_reporting_units",
      sort = TRUE
    )
  
  list(
    application_summary = application_summary,
    coverage_flags = coverage_flags,
    source_summary = source_summary
  )
}


validate_harmonized_analytical_dataset <- function(harmonized, validation_dir) {
  
  validation_results <- validate_dataset(
    df = harmonized,
    expected_dates = sort(unique(get_camera_file_catalog()$election_date)),
    stage = "analytical",
    post_territorial = TRUE
  )
  
  save_and_report_validation(
    validation_results = validation_results,
    validation_dir = validation_dir,
    stop_message = "Final post-territorial validation failed.",
    warning_message = "Final post-territorial validation completed with warning-level issues.",
    success_message = "Final post-territorial validation completed successfully with no critical errors."
  )
  
  invisible(validation_results)
}


write_harmonized_dataset_outputs <- function(
    harmonized,
    summaries,
    output_rds_path,
    output_csv_path,
    application_summary_path,
    node_counts_path,
    source_summary_path
) {
  
  write_project_rds(
    harmonized,
    output_rds_path
  )
  
  write_project_csv(
    harmonized,
    output_csv_path
  )
  
  write_project_csv(
    summaries$application_summary,
    application_summary_path
  )
  
  write_project_csv(
    summaries$coverage_flags,
    node_counts_path
  )
  
  write_project_csv(
    summaries$source_summary,
    source_summary_path
  )
  
  invisible(output_rds_path)
}