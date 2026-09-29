# validation.R
# Validation contract for pre- and post-territorial Camera datasets.


validate_dataset <- function(
    df,
    expected_dates,
    stage = c("processed", "analytical"),
    post_territorial = FALSE
) {
  
  stage <- match.arg(stage)
  
  first_non_missing_number <- function(x) {
    x_non_missing <- x[!is.na(x)]
    
    if (length(x_non_missing) == 0) {
      return(NA_real_)
    }
    
    as.numeric(x_non_missing[[1]])
  }
  

  validation_contract <- list(
    processed = c(
      missing_expected_election_dates = "error",
      unexpected_election_dates = "error",
      missing_or_empty_key_text_fields = "error",
      missing_raw_province = "info",
      invalid_raw_province_source_values = "error",
      missing_analytical_province = if (post_territorial) "error" else "info",
      missing_registered_voters = "warning",
      missing_voters = "warning",
      missing_blank_ballots = "warning",
      missing_null_ballots = "warning",
      missing_list_votes = "error",
      negative_count_values = "warning",
      voters_greater_than_registered_voters = "warning",
      blank_ballots_greater_than_voters = "warning",
      list_votes_greater_than_voters = "warning",
      duplicate_reporting_unit_list_rows = "warning",
      inconsistent_reporting_unit_totals_across_lists = "warning",
      reporting_unit_accounting_gap = "warning"
    ),
    analytical = c(
      missing_expected_election_dates = "error",
      unexpected_election_dates = "error",
      missing_or_empty_key_text_fields = "error",
      missing_raw_province = "info",
      invalid_raw_province_source_values = "error",
      missing_analytical_province = if (post_territorial) "error" else "info",
      missing_registered_voters = "error",
      missing_voters = "error",
      missing_blank_ballots = "error",
      missing_null_ballots = "error",
      missing_list_votes = "error",
      negative_count_values = "error",
      voters_greater_than_registered_voters = "error",
      blank_ballots_greater_than_voters = "error",
      list_votes_greater_than_voters = "error",
      duplicate_reporting_unit_list_rows = "error",
      inconsistent_reporting_unit_totals_across_lists = "error",
      reporting_unit_accounting_gap = "error"
    )
  )
  
  severity_for <- function(check_name) {
    severity <- unname(validation_contract[[stage]][[check_name]])
    
    if (is.null(severity) || length(severity) == 0 || is.na(severity)) {
      stop("No validation severity defined for check: ", check_name)
    }
    
    severity
  }
  
  pre_territorial_required_vars <- c(
    "election_date",
    "year",
    "source_dataset",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_raw",
    "province_raw_source",
    "municipality",
    "registered_voters",
    "voters",
    "abstentions",
    "blank_ballots",
    "null_ballots",
    "list_name",
    "list_votes"
  )
  
  post_territorial_required_vars <- c(
    pre_territorial_required_vars,
    "province_actual",
    "province_harmonized",
    "territorial_match_status"
  )
  
  required_vars <- if (post_territorial) {
    post_territorial_required_vars
  } else {
    pre_territorial_required_vars
  }
  
  missing_required_vars <- setdiff(required_vars, names(df))
  
  if (length(missing_required_vars) > 0) {
    stop(
      "Dataset is missing required columns: ",
      paste(missing_required_vars, collapse = ", ")
    )
  }
  
  reporting_unit_key <- c(
    "election_date",
    "source_dataset",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_raw",
    "municipality"
  )
  
  validation_summary <- tibble::tibble(
    check_name = character(),
    severity = character(),
    n_problem_rows = integer(),
    stop_pipeline = logical()
  )
  
  issue_tables <- list()
  
  add_issue <- function(name, severity, tbl) {
    issue_tables[[name]] <<- tbl
    
    validation_summary <<- dplyr::bind_rows(
      validation_summary,
      tibble::tibble(
        check_name = name,
        severity = severity,
        n_problem_rows = nrow(tbl),
        stop_pipeline = severity == "error" & nrow(tbl) > 0
      )
    )
  }
  
  rows_per_election <- df |>
    dplyr::count(
      .data$election_date,
      .data$source_dataset,
      name = "n_rows"
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset
    )
  
  column_na_summary <- df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ sum(is.na(.x))
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "n_missing"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$n_missing),
      .data$variable
    )
  
  missing_dates <- tibble::tibble(
    election_date = setdiff(expected_dates, unique(df$election_date))
  )
  
  add_issue(
    name = "missing_expected_election_dates",
    severity = severity_for("missing_expected_election_dates"),
    tbl = missing_dates
  )
  
  unexpected_dates <- tibble::tibble(
    election_date = setdiff(unique(df$election_date), expected_dates)
  )
  
  add_issue(
    name = "unexpected_election_dates",
    severity = severity_for("unexpected_election_dates"),
    tbl = unexpected_dates
  )
  
  missing_text_fields <- df |>
    dplyr::filter(
      is.na(.data$municipality) | .data$municipality == "" |
        is.na(.data$list_name) | .data$list_name == ""
    ) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name
    )
  
  add_issue(
    name = "missing_or_empty_key_text_fields",
    severity = severity_for("missing_or_empty_key_text_fields"),
    tbl = missing_text_fields
  )
  
  missing_raw_province <- df |>
    dplyr::filter(
      is.na(.data$province_raw) | .data$province_raw == ""
    ) |>
    dplyr::select(
      election_date,
      source_dataset,
      municipality,
      province_raw,
      province_raw_source
    )
  
  add_issue(
    name = "missing_raw_province",
    severity = severity_for("missing_raw_province"),
    tbl = missing_raw_province
  )
  
  valid_raw_province_sources <- c( 
    "raw_source_field",
    "missing_raw_source_field",
    "not_available_in_raw_source",
    "vda_fixed_raw_proxy"
  )
  
  invalid_raw_province_source <- df |>
    dplyr::filter(
      is.na(.data$province_raw_source) |
        !.data$province_raw_source %in% valid_raw_province_sources
    ) |>
    dplyr::select(
      election_date,
      source_dataset,
      municipality,
      province_raw,
      province_raw_source
    )
  
  add_issue(
    name = "invalid_raw_province_source_values",
    severity = severity_for("invalid_raw_province_source_values"),
    tbl = invalid_raw_province_source
  )
  
  if (post_territorial) {
    
    missing_analytical_province <- df |>
      dplyr::filter(
        is.na(.data$province_harmonized) | .data$province_harmonized == "" |
          is.na(.data$province_actual) | .data$province_actual == ""
      ) |>
      dplyr::select(
        election_date,
        source_dataset,
        municipality,
        province_raw,
        province_actual,
        province_harmonized,
        territorial_match_status
      )
    
  } else {
    
    missing_analytical_province <- tibble::tibble()
  }
  
  add_issue(
    name = "missing_analytical_province",
    severity = severity_for("missing_analytical_province"),
    tbl = missing_analytical_province
  )
  
  missing_registered_voters <- df |>
    dplyr::filter(is.na(.data$registered_voters)) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      registered_voters
    )
  
  add_issue(
    name = "missing_registered_voters",
    severity = severity_for("missing_registered_voters"),
    tbl = missing_registered_voters
  )
  
  missing_voters <- df |>
    dplyr::filter(is.na(.data$voters)) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      voters
    )
  
  add_issue(
    name = "missing_voters",
    severity = severity_for("missing_voters"),
    tbl = missing_voters
  )
  
  missing_blank_ballots <- df |>
    dplyr::filter(is.na(.data$blank_ballots)) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      blank_ballots
    )
  
  add_issue(
    name = "missing_blank_ballots",
    severity = severity_for("missing_blank_ballots"),
    tbl = missing_blank_ballots
  )
  
  missing_null_ballots <- df |>
    dplyr::filter(is.na(.data$null_ballots)) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      null_ballots
    )
  
  add_issue(
    name = "missing_null_ballots",
    severity = severity_for("missing_null_ballots"),
    tbl = missing_null_ballots
  )
  
  missing_list_votes <- df |>
    dplyr::filter(is.na(.data$list_votes)) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      list_votes
    )
  
  add_issue(
    name = "missing_list_votes",
    severity = severity_for("missing_list_votes"),
    tbl = missing_list_votes
  )
  
  negative_counts <- df |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      registered_voters,
      voters,
      abstentions,
      blank_ballots,
      null_ballots,
      list_votes
    ) |>
    tidyr::pivot_longer(
      cols = c(
        "registered_voters",
        "voters",
        "abstentions",
        "blank_ballots",
        "null_ballots",
        "list_votes"
      ),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::filter(
      !is.na(.data$value) & .data$value < 0
    )
  
  add_issue(
    name = "negative_count_values",
    severity = severity_for("negative_count_values"),
    tbl = negative_counts
  )
  
  voters_gt_registered <- df |>
    dplyr::filter(
      !is.na(.data$voters) &
        !is.na(.data$registered_voters) &
        .data$voters > .data$registered_voters
    ) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      registered_voters,
      voters
    )
  
  add_issue(
    name = "voters_greater_than_registered_voters",
    severity = severity_for("voters_greater_than_registered_voters"),
    tbl = voters_gt_registered
  )
  
  blank_gt_voters <- df |>
    dplyr::filter(
      !is.na(.data$blank_ballots) &
        !is.na(.data$voters) &
        .data$blank_ballots > .data$voters
    ) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      blank_ballots,
      voters
    )
  
  add_issue(
    name = "blank_ballots_greater_than_voters",
    severity = severity_for("blank_ballots_greater_than_voters"),
    tbl = blank_gt_voters
  )
  
  list_votes_gt_voters <- df |>
    dplyr::filter(
      !is.na(.data$list_votes) &
        !is.na(.data$voters) &
        .data$list_votes > .data$voters
    ) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      list_name,
      list_votes,
      voters
    )
  
  add_issue(
    name = "list_votes_greater_than_voters",
    severity = severity_for("list_votes_greater_than_voters"),
    tbl = list_votes_gt_voters
  )
  
  duplicate_reporting_unit_list_rows <- df |>
    dplyr::count(
      dplyr::across(dplyr::all_of(c(reporting_unit_key, "list_name"))),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$municipality,
      .data$list_name
    )
  
  add_issue(
    name = "duplicate_reporting_unit_list_rows",
    severity = severity_for("duplicate_reporting_unit_list_rows"),
    tbl = duplicate_reporting_unit_list_rows
  )
  
  inconsistent_reporting_unit_totals <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(reporting_unit_key))
    ) |>
    dplyr::summarise(
      n_registered_values = dplyr::n_distinct(.data$registered_voters[!is.na(.data$registered_voters)]),
      n_voters_values = dplyr::n_distinct(.data$voters[!is.na(.data$voters)]),
      n_abstentions_values = dplyr::n_distinct(.data$abstentions[!is.na(.data$abstentions)]),
      n_blank_values = dplyr::n_distinct(.data$blank_ballots[!is.na(.data$blank_ballots)]),
      n_null_values = dplyr::n_distinct(.data$null_ballots[!is.na(.data$null_ballots)]),
      min_registered_voters = if (all(is.na(.data$registered_voters))) NA_real_ else min(.data$registered_voters, na.rm = TRUE),
      max_registered_voters = if (all(is.na(.data$registered_voters))) NA_real_ else max(.data$registered_voters, na.rm = TRUE),
      min_voters = if (all(is.na(.data$voters))) NA_real_ else min(.data$voters, na.rm = TRUE),
      max_voters = if (all(is.na(.data$voters))) NA_real_ else max(.data$voters, na.rm = TRUE),
      min_abstentions = if (all(is.na(.data$abstentions))) NA_real_ else min(.data$abstentions, na.rm = TRUE),
      max_abstentions = if (all(is.na(.data$abstentions))) NA_real_ else max(.data$abstentions, na.rm = TRUE),
      min_blank_ballots = if (all(is.na(.data$blank_ballots))) NA_real_ else min(.data$blank_ballots, na.rm = TRUE),
      max_blank_ballots = if (all(is.na(.data$blank_ballots))) NA_real_ else max(.data$blank_ballots, na.rm = TRUE),
      min_null_ballots = if (all(is.na(.data$null_ballots))) NA_real_ else min(.data$null_ballots, na.rm = TRUE),
      max_null_ballots = if (all(is.na(.data$null_ballots))) NA_real_ else max(.data$null_ballots, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      registered_inconsistent = .data$n_registered_values > 1,
      voters_inconsistent = .data$n_voters_values > 1,
      abstentions_inconsistent = .data$n_abstentions_values > 1,
      blank_inconsistent = .data$n_blank_values > 1,
      null_inconsistent = .data$n_null_values > 1
    ) |>
    dplyr::filter(
      .data$registered_inconsistent |
        .data$voters_inconsistent |
        .data$abstentions_inconsistent |
        .data$blank_inconsistent |
        .data$null_inconsistent
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$municipality
    )
  
  add_issue(
    name = "inconsistent_reporting_unit_totals_across_lists",
    severity = severity_for("inconsistent_reporting_unit_totals_across_lists"),
    tbl = inconsistent_reporting_unit_totals
  )
  
  reporting_unit_accounting_gap <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("year", reporting_unit_key)))
    ) |>
    dplyr::summarise(
      registered_voters = first_non_missing_number(.data$registered_voters),
      voters = first_non_missing_number(.data$voters),
      abstentions = first_non_missing_number(.data$abstentions),
      blank_ballots = first_non_missing_number(.data$blank_ballots),
      null_ballots = first_non_missing_number(.data$null_ballots),
      total_list_votes = sum(.data$list_votes, na.rm = TRUE),
      reconstructed_voters =
        .data$total_list_votes +
        .data$blank_ballots +
        .data$null_ballots,
      voters_gap =
        .data$voters -
        .data$reconstructed_voters,
      electorate_gap =
        .data$registered_voters -
        .data$voters -
        .data$abstentions,
      abs_voters_gap = abs(.data$voters_gap),
      abs_electorate_gap = abs(.data$electorate_gap),
      voters_gap_share = dplyr::if_else(
        !is.na(.data$voters) & .data$voters > 0,
        .data$voters_gap / .data$voters,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      (!is.na(.data$voters_gap) & .data$voters_gap != 0) |
        (!is.na(.data$electorate_gap) & .data$electorate_gap != 0)
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$abs_voters_gap),
      dplyr::desc(.data$abs_electorate_gap),
      .data$election_date,
      .data$municipality
    )
  
  add_issue(
    name = "reporting_unit_accounting_gap",
    severity = severity_for("reporting_unit_accounting_gap"),
    tbl = reporting_unit_accounting_gap
  )
  
  
  list(
    summary = validation_summary,
    issue_tables = issue_tables,
    rows_per_election = rows_per_election,
    column_na_summary = column_na_summary,
    stage = stage
  )
}
