# audit_exclusions.R
# Reporting-unit audit for the pre-territorial Camera dataset.

# input: processed dataset
# outputs:
# - a deduplicated version of the data;
# - reporting-unit-level anomalies;
# - excluded rows;
# - the analytical dataset (internally consistent and ready for analysis);
# - several audit tables.




audit_reporting_units <- function(df) {
  
  if (!"province_raw" %in% names(df)) {
    df$province_raw <- NA_character_
  }
  
  if (!"province_raw_source" %in% names(df)) {
    df$province_raw_source <- NA_character_
  }
  
  # Internal accounting key: reporting unit at which consistency is checked. 
  audit_key <- c(
    "election_date",
    "source_dataset",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_raw",
    "municipality"
  )
  
  audit_reason_cols <- c( # cols reason_* are boolean flags attached to excluded rows
    "exclusion_level",
    "exclusion_reason", # ex. "negative_null + list_votes_gt_voters"
    "reason_missing_registered",
    "reason_missing_voters",
    "reason_missing_blank",
    "reason_voters_gt_registered",
    "reason_blank_gt_voters",
    "reason_negative_null",
    "reason_negative_list_votes",
    "reason_list_votes_gt_voters"
  )
  
  missing_key_cols <- setdiff(audit_key, names(df))
  
  if (length(missing_key_cols) > 0) {
    stop(
      "audit_reporting_units() is missing key columns: ",
      paste(missing_key_cols, collapse = ", ")
    )
  }
  
  required_cols <- c(
    audit_key,
    "province_raw_source",
    "registered_voters",
    "voters",
    "blank_ballots",
    "null_ballots",
    "list_name",
    "list_votes"
  )
  
  missing_required_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_required_cols) > 0) {
    stop(
      "audit_reporting_units() is missing required columns: ",
      paste(missing_required_cols, collapse = ", ")
    )
  }
  
  first_non_missing <- function(x) {
    x_non_missing <- x[!is.na(x)]
    
    if (length(x_non_missing) == 0) {
      return(NA)
    }
    
    x_non_missing[[1]]
  }
  
  first_non_missing_character <- function(x) {
    value <- first_non_missing(x)
    
    if (is.na(value)) {
      return(NA_character_)
    }
    
    as.character(value)
  }
  
  
  
  duplicate_exact_rows <- df |>
    dplyr::add_count(
      dplyr::across(dplyr::everything()),
      name = "n_duplicates"
    ) |>
    dplyr::filter(.data$n_duplicates > 1) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$province_raw,
      .data$municipality,
      .data$list_name
    )
  
  
  processed_unique <- df |>
    dplyr::distinct() |>
    derive_null_ballots() # null ballots are recomputed after exact de-duplication
  
  
  duplicate_reporting_list_keys <- processed_unique |>
    dplyr::count(
      dplyr::across(dplyr::all_of(c(audit_key, "list_name"))),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$municipality,
      .data$list_name
    )
  
  if (nrow(duplicate_reporting_list_keys) > 0) {
    warning(
      "Duplicate reporting-unit/list keys detected after exact de-duplication."
    )
  }
  
  
  municipality_anomalies <- processed_unique |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(audit_key))
    ) |>
    dplyr::summarise(
      province_raw_ref = first_non_missing_character(.data$province_raw),
      province_raw_source_ref = first_non_missing_character(.data$province_raw_source),
      
      registered_voters_ref = first_non_missing(.data$registered_voters),
      voters_ref = first_non_missing(.data$voters),
      blank_ballots_ref = first_non_missing(.data$blank_ballots),
      null_ballots_ref = first_non_missing(.data$null_ballots),
      
      any_missing_registered = any(is.na(.data$registered_voters)),
      any_missing_voters = any(is.na(.data$voters)),
      any_missing_blank = any(is.na(.data$blank_ballots)),
      
      any_voters_gt_registered = any(
        !is.na(.data$voters) &
          !is.na(.data$registered_voters) &
          .data$voters > .data$registered_voters
      ),
      
      any_blank_gt_voters = any(
        !is.na(.data$blank_ballots) &
          !is.na(.data$voters) &
          .data$blank_ballots > .data$voters
      ),
      
      any_negative_list_votes = any(
        !is.na(.data$list_votes) &
          .data$list_votes < 0
      ),
      
      any_negative_null = any(
        !is.na(.data$null_ballots) &
          .data$null_ballots < 0
      ),
      
      any_list_votes_gt_voters = any(
        !is.na(.data$list_votes) &
          !is.na(.data$voters) &
          .data$list_votes > .data$voters
      ),
      
      n_rows_in_group = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$any_missing_registered |
        .data$any_missing_voters |
        .data$any_missing_blank |
        .data$any_voters_gt_registered |
        .data$any_blank_gt_voters |
        .data$any_negative_null |
        .data$any_negative_list_votes |
        .data$any_list_votes_gt_voters
    ) |>
    dplyr::mutate(
      exclusion_level = "reporting_unit_election",
      reason_missing_registered = .data$any_missing_registered,
      reason_missing_voters = .data$any_missing_voters,
      reason_missing_blank = .data$any_missing_blank,
      reason_voters_gt_registered = .data$any_voters_gt_registered,
      reason_blank_gt_voters = .data$any_blank_gt_voters,
      reason_negative_null = .data$any_negative_null,
      reason_negative_list_votes = .data$any_negative_list_votes,
      reason_list_votes_gt_voters = .data$any_list_votes_gt_voters
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      exclusion_reason = paste(
        c(
          if (reason_missing_registered) "missing_registered" else NULL,
          if (reason_missing_voters) "missing_voters" else NULL,
          if (reason_missing_blank) "missing_blank" else NULL,
          if (reason_voters_gt_registered) "voters_gt_registered" else NULL,
          if (reason_blank_gt_voters) "blank_gt_voters" else NULL,
          if (reason_negative_null) "negative_null" else NULL,
          if (reason_negative_list_votes) "negative_list_votes" else NULL,
          if (reason_list_votes_gt_voters) "list_votes_gt_voters" else NULL
        ),
        collapse = " + "
      )
    ) |>
    dplyr::ungroup()
  
  
  list_vote_anomalies <- processed_unique |>
    dplyr::filter(
      !is.na(.data$list_votes) &
        !is.na(.data$voters) &
        .data$list_votes > .data$voters
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$province_raw,
      .data$municipality,
      .data$list_name
    )
  
  
  excluded_rows_all <- processed_unique |>
    dplyr::semi_join( 
      municipality_anomalies |>
        dplyr::select(dplyr::all_of(audit_key)),
      by = audit_key
    ) |>
    dplyr::left_join(
      municipality_anomalies |>
        dplyr::select(dplyr::all_of(c(audit_key, audit_reason_cols))),
      by = audit_key
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$source_dataset,
      .data$province_raw,
      .data$municipality,
      .data$list_name
    )
  
  analytical_data <- processed_unique |>
    dplyr::anti_join( 
      municipality_anomalies |>
        dplyr::select(dplyr::all_of(audit_key)),
      by = audit_key
    )

  
  excluded_municipal_units <- municipality_anomalies |>
    dplyr::mutate(
      registered_voters_ref = as.numeric(.data$registered_voters_ref),
      municipality_size_class = dplyr::case_when(
        is.na(.data$registered_voters_ref) ~ "missing_registered",
        .data$registered_voters_ref < 1000 ~ "<1,000",
        .data$registered_voters_ref < 5000 ~ "1,000-4,999",
        .data$registered_voters_ref < 10000 ~ "5,000-9,999",
        .data$registered_voters_ref < 50000 ~ "10,000-49,999",
        TRUE ~ "50,000+"
      )
    )
  
  processed_reporting_units <- processed_unique |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(audit_key))
    ) |>
    dplyr::summarise(
      province_raw_ref = first_non_missing_character(.data$province_raw),
      province_raw_source_ref = first_non_missing_character(.data$province_raw_source),
      registered_voters_ref = first_non_missing(.data$registered_voters),
      .groups = "drop"
    )
  
  
  audit_summary_year <- processed_reporting_units |>
    dplyr::group_by(
      .data$election_date,
      year = as.integer(substr(.data$election_date, 1, 4))
    ) |>
    dplyr::summarise(
      total_reporting_units = dplyr::n(),
      total_registered_voters = sum(.data$registered_voters_ref, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      excluded_municipal_units |>
        dplyr::group_by(.data$election_date) |>
        dplyr::summarise(
          excluded_reporting_units = dplyr::n(),
          excluded_registered_voters = sum(.data$registered_voters_ref, na.rm = TRUE),
          
          excluded_due_missing_registered = sum(.data$reason_missing_registered, na.rm = TRUE),
          excluded_due_missing_voters = sum(.data$reason_missing_voters, na.rm = TRUE),
          excluded_due_missing_blank = sum(.data$reason_missing_blank, na.rm = TRUE),
          excluded_due_voters_gt_registered = sum(.data$reason_voters_gt_registered, na.rm = TRUE),
          excluded_due_blank_gt_voters = sum(.data$reason_blank_gt_voters, na.rm = TRUE),
          excluded_due_negative_null = sum(.data$reason_negative_null, na.rm = TRUE),
          excluded_due_negative_list_votes = sum(.data$reason_negative_list_votes, na.rm = TRUE),
          excluded_due_list_votes_gt_voters = sum(.data$reason_list_votes_gt_voters, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "election_date"
    ) |>
    dplyr::left_join(
      list_vote_anomalies |>
        dplyr::group_by(.data$election_date) |>
        dplyr::summarise(
          flagged_list_rows_gt_voters = dplyr::n(),
          .groups = "drop"
        ),
      by = "election_date"
    ) |>
    dplyr::mutate(
      excluded_reporting_units = dplyr::coalesce(.data$excluded_reporting_units, 0L),
      excluded_registered_voters = dplyr::coalesce(.data$excluded_registered_voters, 0),
      
      excluded_due_missing_registered = dplyr::coalesce(.data$excluded_due_missing_registered, 0L),
      excluded_due_missing_voters = dplyr::coalesce(.data$excluded_due_missing_voters, 0L),
      excluded_due_missing_blank = dplyr::coalesce(.data$excluded_due_missing_blank, 0L),
      excluded_due_voters_gt_registered = dplyr::coalesce(.data$excluded_due_voters_gt_registered, 0L),
      excluded_due_blank_gt_voters = dplyr::coalesce(.data$excluded_due_blank_gt_voters, 0L),
      excluded_due_negative_null = dplyr::coalesce(.data$excluded_due_negative_null, 0L),
      excluded_due_negative_list_votes = dplyr::coalesce(.data$excluded_due_negative_list_votes, 0L),
      excluded_due_list_votes_gt_voters = dplyr::coalesce(.data$excluded_due_list_votes_gt_voters, 0L),
      flagged_list_rows_gt_voters = dplyr::coalesce(.data$flagged_list_rows_gt_voters, 0L),
      
      excluded_reporting_unit_share =
        .data$excluded_reporting_units / .data$total_reporting_units,
      
      excluded_electorate_share = dplyr::if_else(
        .data$total_registered_voters > 0,
        .data$excluded_registered_voters / .data$total_registered_voters,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$election_date)
  
  
  audit_summary_reason <- excluded_municipal_units |>
    dplyr::count(
      exclusion_reason,
      name = "n_excluded_reporting_units"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$n_excluded_reporting_units),
      .data$exclusion_reason
    )
  
  audit_summary_size <- excluded_municipal_units |>
    dplyr::count(
      municipality_size_class,
      name = "n_excluded_reporting_units"
    ) |>
    dplyr::arrange(.data$municipality_size_class)
  
  audit_summary_province_raw <- excluded_municipal_units |>
    dplyr::count(
      province_raw_ref,
      province_raw_source_ref,
      name = "n_excluded_reporting_units"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$n_excluded_reporting_units),
      .data$province_raw_ref
    )
  
  audit_year_size <- excluded_municipal_units |>
    dplyr::count(
      election_date,
      municipality_size_class,
      name = "n_excluded_reporting_units"
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality_size_class
    )

  
  audit_excluded_vs_retained_size <- processed_reporting_units |>
    dplyr::left_join(
      excluded_municipal_units |>
        dplyr::select(dplyr::all_of(audit_key)) |>
        dplyr::mutate(excluded = TRUE),
      by = audit_key
    ) |>
    dplyr::mutate(
      status = dplyr::if_else(
        dplyr::coalesce(.data$excluded, FALSE),
        "excluded",
        "retained"
      ),
      municipality_size_class = dplyr::case_when(
        is.na(.data$registered_voters_ref) ~ "missing_registered",
        .data$registered_voters_ref < 1000 ~ "<1,000",
        .data$registered_voters_ref < 5000 ~ "1,000-4,999",
        .data$registered_voters_ref < 10000 ~ "5,000-9,999",
        .data$registered_voters_ref < 50000 ~ "10,000-49,999",
        TRUE ~ "50,000+"
      )
    )
  
  audit_size_distribution <- audit_excluded_vs_retained_size |>
    dplyr::count(
      status,
      municipality_size_class,
      name = "n_reporting_units"
    ) |>
    dplyr::arrange(
      .data$status,
      .data$municipality_size_class
    )
  
  
  audit_excluded_electorate_comment <- audit_summary_year |>
    dplyr::transmute(
      election_date = .data$election_date,
      excluded_reporting_unit_share_pct =
        round(100 * .data$excluded_reporting_unit_share, 2),
      excluded_electorate_share_pct =
        round(100 * .data$excluded_electorate_share, 2)
    )
  
  audit_comment_year <- audit_summary_year |>
    dplyr::transmute(
      election_date,
      excluded_reporting_units,
      flagged_list_rows_gt_voters,
      reporting_unit_exclusion_pct =
        round(100 * .data$excluded_reporting_unit_share, 2),
      electorate_exclusion_pct =
        round(100 * .data$excluded_electorate_share, 2)
    )
  
  
  list(
    processed_unique = processed_unique,
    analytical_data = analytical_data,
    duplicate_exact_rows = duplicate_exact_rows,
    duplicate_reporting_list_keys = duplicate_reporting_list_keys,
    list_vote_anomalies = list_vote_anomalies,
    excluded_rows_all = excluded_rows_all,
    excluded_municipal_units = excluded_municipal_units,
    audit_summary_year = audit_summary_year,
    audit_summary_reason = audit_summary_reason,
    audit_summary_size = audit_summary_size,
    audit_summary_province_raw = audit_summary_province_raw
  )
}
