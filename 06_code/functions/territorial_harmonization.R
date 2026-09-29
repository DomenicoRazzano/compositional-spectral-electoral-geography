# territorial_harmonization.R
# Harmonized province assignment in the fixed target territorial frame.


build_target_reference <- function(target_frame_date, situas_dir) {
  
  target_file <- file.path(
    situas_dir,
    paste0("situas_comuni_", target_frame_date, ".csv")
  )
  
  target_ref <- standardize_situas_file(
    file_path = target_file,
    election_date = target_frame_date
  )
  
  target_by_cadastral <- target_ref |>
    dplyr::filter(
      !is.na(.data$cadastral_code),
      .data$cadastral_code != ""
    ) |>
    dplyr::distinct(cadastral_code, .keep_all = TRUE) |>
    dplyr::transmute(
      cadastral_code = .data$cadastral_code,
      municipality_target = .data$municipality_situas,
      municipality_target_match = .data$municipality_match,
      municipality_code_target = .data$municipality_code_alphanumeric,
      province_harmonized = .data$province_actual,
      province_harmonized_code = .data$province_actual_code,
      province_harmonized_abbrev = .data$province_actual_abbrev,
      region_harmonized = .data$region_actual,
      region_harmonized_code = .data$region_actual_code
    )
  
  target_alias <- expand_situas_aliases(target_ref)
  
  target_alias_uniqueness <- target_alias |>
    dplyr::group_by(.data$municipality_alias_match) |>
    dplyr::summarise(
      n_target_provinces = dplyr::n_distinct(.data$province_actual),
      candidate_target_provinces = paste(
        sort(unique(.data$province_actual)),
        collapse = " | "
      ),
      .groups = "drop"
    )
  
  target_by_name <- target_alias |>
    dplyr::left_join(
      target_alias_uniqueness,
      by = "municipality_alias_match"
    ) |>
    dplyr::filter(.data$n_target_provinces == 1L) |>
    dplyr::distinct(municipality_alias_match, .keep_all = TRUE) |>
    dplyr::transmute(
      municipality_match_target = .data$municipality_alias_match,
      municipality_target = .data$municipality_situas,
      municipality_target_match = .data$municipality_alias_match,
      municipality_code_target = .data$municipality_code_alphanumeric,
      province_harmonized = .data$province_actual,
      province_harmonized_code = .data$province_actual_code,
      province_harmonized_abbrev = .data$province_actual_abbrev,
      region_harmonized = .data$region_actual,
      region_harmonized_code = .data$region_actual_code
    )
  
  list(
    target_ref = target_ref,
    target_by_cadastral = target_by_cadastral,
    target_by_name = target_by_name
  )
}


assign_direct_harmonized_province <- function(
    lookup_actual,
    target_reference,
    target_frame_date
) {
  
  historical_homonym_names <- lookup_actual |>
    dplyr::filter(
      .data$actual_match_status == "actual_resolved",
      !is.na(.data$election_date),
      !is.na(.data$municipality_match),
      !is.na(.data$cadastral_code),
      !is.na(.data$municipality_code_alphanumeric),
      !is.na(.data$province_actual)
    ) |>
    dplyr::distinct(
      .data$election_date,
      .data$municipality_match,
      .data$cadastral_code,
      .data$municipality_code_alphanumeric,
      .data$province_actual
    ) |>
    dplyr::group_by(
      .data$election_date,
      .data$municipality_match
    ) |>
    dplyr::summarise(
      n_cadastral = dplyr::n_distinct(.data$cadastral_code, na.rm = TRUE),
      n_codes = dplyr::n_distinct(.data$municipality_code_alphanumeric, na.rm = TRUE),
      n_provinces = dplyr::n_distinct(.data$province_actual, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$n_cadastral > 1,
      .data$n_codes > 1,
      .data$n_provinces > 1
    ) |>
    dplyr::distinct(.data$municipality_match) |>
    dplyr::pull(.data$municipality_match)
  
  lookup_actual |>
    dplyr::left_join(
      target_reference$target_by_cadastral,
      by = "cadastral_code"
    ) |>
    dplyr::rename(
      municipality_target_cadastral = municipality_target,
      municipality_target_match_cadastral = municipality_target_match,
      municipality_code_target_cadastral = municipality_code_target,
      province_harmonized_cadastral = province_harmonized,
      province_harmonized_code_cadastral = province_harmonized_code,
      province_harmonized_abbrev_cadastral = province_harmonized_abbrev,
      region_harmonized_cadastral = region_harmonized,
      region_harmonized_code_cadastral = region_harmonized_code
    ) |>
    dplyr::left_join(
      target_reference$target_by_name,
      by = c("municipality_match" = "municipality_match_target")
    ) |>
    dplyr::rename(
      municipality_target_name = municipality_target,
      municipality_target_match_name = municipality_target_match,
      municipality_code_target_name = municipality_code_target,
      province_harmonized_name = province_harmonized,
      province_harmonized_code_name = province_harmonized_code,
      province_harmonized_abbrev_name = province_harmonized_abbrev,
      region_harmonized_name = region_harmonized,
      region_harmonized_code_name = region_harmonized_code
    ) |>
    dplyr::mutate(
      unsafe_homonym_name_match = dplyr::case_when(
        .data$actual_match_status == "actual_resolved" &
          is.na(.data$province_harmonized_cadastral) &
          !is.na(.data$province_harmonized_name) &
          .data$municipality_match %in% historical_homonym_names &
          !is.na(.data$municipality_match) &
          !is.na(.data$municipality_target_match_name) &
          .data$municipality_match == .data$municipality_target_match_name &
          !is.na(.data$municipality_code_alphanumeric) &
          !is.na(.data$municipality_code_target_name) &
          .data$municipality_code_alphanumeric != .data$municipality_code_target_name ~ TRUE,
        TRUE ~ FALSE
      ),
      
      harmonized_direct_method = dplyr::case_when(
        .data$actual_match_status != "actual_resolved" ~ NA_character_,
        !is.na(.data$province_harmonized_cadastral) ~
          "target_cadastral_code_match",
        is.na(.data$province_harmonized_cadastral) &
          !is.na(.data$province_harmonized_name) &
          !.data$unsafe_homonym_name_match ~
          "target_name_match",
        .data$unsafe_homonym_name_match ~
          "target_name_match_blocked_historical_homonym",
        TRUE ~ NA_character_
      ),
      
      municipality_target_raw = dplyr::coalesce(
        .data$municipality_target_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$municipality_target_name,
          NA_character_
        )
      ),
      
      municipality_target_match_raw = dplyr::coalesce(
        .data$municipality_target_match_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$municipality_target_match_name,
          NA_character_
        )
      ),
      
      municipality_code_target_raw = dplyr::coalesce(
        .data$municipality_code_target_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$municipality_code_target_name,
          NA_character_
        )
      ),
      
      province_harmonized_raw = dplyr::coalesce(
        .data$province_harmonized_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$province_harmonized_name,
          NA_character_
        )
      ),
      
      province_harmonized_code_raw = dplyr::coalesce(
        .data$province_harmonized_code_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$province_harmonized_code_name,
          NA_character_
        )
      ),
      
      province_harmonized_abbrev_raw = dplyr::coalesce(
        .data$province_harmonized_abbrev_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$province_harmonized_abbrev_name,
          NA_character_
        )
      ),
      
      region_harmonized_raw = dplyr::coalesce(
        .data$region_harmonized_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$region_harmonized_name,
          NA_character_
        )
      ),
      
      region_harmonized_code_raw = dplyr::coalesce(
        .data$region_harmonized_code_cadastral,
        dplyr::if_else(
          !.data$unsafe_homonym_name_match,
          .data$region_harmonized_code_name,
          NA_character_
        )
      ),
      municipality_target = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$municipality_target_raw,
        NA_character_
      ),
      municipality_target_match = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$municipality_target_match_raw,
        NA_character_
      ),
      municipality_code_target = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$municipality_code_target_raw,
        NA_character_
      ),
      province_harmonized = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$province_harmonized_raw,
        NA_character_
      ),
      province_harmonized_code = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$province_harmonized_code_raw,
        NA_character_
      ),
      province_harmonized_abbrev = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$province_harmonized_abbrev_raw,
        NA_character_
      ),
      region_harmonized = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$region_harmonized_raw,
        NA_character_
      ),
      region_harmonized_code = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$region_harmonized_code_raw,
        NA_character_
      ),
      province_harmonized_source = dplyr::if_else(
        .data$actual_match_status == "actual_resolved",
        .data$harmonized_direct_method,
        NA_character_
      ),
      province_harmonized_frame_date = target_frame_date
    ) |>
    dplyr::select(
      election_date,
      year,
      municipality,
      province_raw,
      province_raw_source,
      province_raw_match,
      district_raw,
      plurinominal_college_raw,
      uninominal_college_raw,
      municipality_match,
      municipality_parent,
      municipality_dash_parent,
      municipality_situas,
      municipality_code_alphanumeric,
      municipality_code_numeric,
      cadastral_code,
      province_actual,
      province_actual_code,
      province_actual_abbrev,
      province_actual_match,
      region_actual,
      region_actual_code,
      province_actual_source,
      actual_match_status,
      actual_match_key_used,
      actual_match_priority,
      actual_manual_reason,
      candidate_actual_provinces,
      municipality_target,
      municipality_target_match,
      municipality_code_target,
      province_harmonized,
      province_harmonized_code,
      province_harmonized_abbrev,
      region_harmonized,
      region_harmonized_code,
      province_harmonized_source,
      province_harmonized_frame_date
    )
}


load_historical_to_target_crosswalk <- function(lookup_dir, election_dates) {
  
  path <- file.path(
    lookup_dir,
    "historical_to_target_municipality_crosswalk.csv"
  )
  
  if (!file.exists(path)) {
    return(tibble::tibble(
      election_date = character(),
      municipality_match = character(),
      province_raw_match_manual = character(),
      municipality_target = character(),
      municipality_target_match_manual = character(),
      harmonization_reason = character(),
      manual_scope_priority = integer()
    ))
  }
  
  raw <- load_tabular_file(path)
  
  required_cols <- c(
    "election_date",
    "municipality_match",
    "province_raw",
    "municipality_target",
    "harmonization_reason"
  )
  
  missing_cols <- setdiff(required_cols, names(raw))
  
  if (length(missing_cols) > 0) {
    stop(
      "historical_to_target_municipality_crosswalk.csv is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  out <- raw |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      municipality_match = normalize_name_for_lookup(.data$municipality_match),
      province_raw_match_manual =
        normalize_province_for_lookup(.data$province_raw),
      municipality_target = as.character(.data$municipality_target),
      municipality_target_match_manual =
        normalize_name_for_lookup(.data$municipality_target),
      harmonization_reason = as.character(.data$harmonization_reason)
    ) |>
    dplyr::select(
      election_date,
      municipality_match,
      province_raw_match_manual,
      municipality_target,
      municipality_target_match_manual,
      harmonization_reason
    ) |>
    expand_all_election_dates(election_dates)
  
  check_unique_manual_key(
    out,
    c(
      "election_date",
      "municipality_match",
      "province_raw_match_manual"
    ),
    "historical_to_target"
  )
  
  out
}


apply_historical_to_target_crosswalk <- function(
    lookup_harmonized_direct,
    target_reference,
    historical_to_target
) {
  
  lookup_key <- territorial_lookup_key()
  
  target_by_name_crosswalk <- target_reference$target_by_name |>
    dplyr::transmute(
      municipality_target_match_manual = .data$municipality_match_target,
      municipality_target_crosswalk = .data$municipality_target,
      municipality_target_match_crosswalk = .data$municipality_target_match,
      municipality_code_target_crosswalk = .data$municipality_code_target,
      province_harmonized_crosswalk = .data$province_harmonized,
      province_harmonized_code_crosswalk = .data$province_harmonized_code,
      province_harmonized_abbrev_crosswalk = .data$province_harmonized_abbrev,
      region_harmonized_crosswalk = .data$region_harmonized,
      region_harmonized_code_crosswalk = .data$region_harmonized_code
    )
  
  lookup_harmonized_direct |>
    dplyr::filter(
      .data$actual_match_status == "actual_resolved",
      is.na(.data$province_harmonized)
    ) |>
    dplyr::left_join(
      historical_to_target,
      by = c("election_date", "municipality_match"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(.data$municipality_target_match_manual),
      is.na(.data$province_raw_match_manual) |
        is.na(.data$province_raw_match) |
        .data$province_raw_match_manual == .data$province_raw_match
    ) |>
    dplyr::left_join(
      target_by_name_crosswalk,
      by = "municipality_target_match_manual"
    ) |>
    dplyr::filter(!is.na(.data$province_harmonized_crosswalk)) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw,
      .data$district_raw,
      .data$plurinominal_college_raw,
      .data$uninominal_college_raw,
      .data$manual_scope_priority
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(lookup_key)),
      .keep_all = TRUE
    ) |>
    dplyr::transmute(
      election_date = .data$election_date,
      year = .data$year,
      municipality = .data$municipality,
      province_raw = .data$province_raw,
      province_raw_source = .data$province_raw_source,
      province_raw_match = .data$province_raw_match,
      district_raw = .data$district_raw,
      plurinominal_college_raw = .data$plurinominal_college_raw,
      uninominal_college_raw = .data$uninominal_college_raw,
      municipality_match = .data$municipality_match,
      municipality_parent = .data$municipality_parent,
      municipality_dash_parent = .data$municipality_dash_parent,
      municipality_situas = .data$municipality_situas,
      municipality_code_alphanumeric = .data$municipality_code_alphanumeric,
      municipality_code_numeric = .data$municipality_code_numeric,
      cadastral_code = .data$cadastral_code,
      province_actual = .data$province_actual,
      province_actual_code = .data$province_actual_code,
      province_actual_abbrev = .data$province_actual_abbrev,
      province_actual_match = .data$province_actual_match,
      region_actual = .data$region_actual,
      region_actual_code = .data$region_actual_code,
      province_actual_source = .data$province_actual_source,
      actual_match_status = .data$actual_match_status,
      actual_match_key_used = .data$actual_match_key_used,
      actual_match_priority = .data$actual_match_priority,
      actual_manual_reason = .data$actual_manual_reason,
      candidate_actual_provinces = .data$candidate_actual_provinces,
      municipality_target = .data$municipality_target_crosswalk,
      municipality_target_match = .data$municipality_target_match_crosswalk,
      municipality_code_target = .data$municipality_code_target_crosswalk,
      province_harmonized = .data$province_harmonized_crosswalk,
      province_harmonized_code = .data$province_harmonized_code_crosswalk,
      province_harmonized_abbrev = .data$province_harmonized_abbrev_crosswalk,
      region_harmonized = .data$region_harmonized_crosswalk,
      region_harmonized_code = .data$region_harmonized_code_crosswalk,
      province_harmonized_source = "historical_to_target_crosswalk",
      province_harmonized_frame_date = .data$province_harmonized_frame_date,
      territorial_match_note = .data$harmonization_reason
    )
}


assign_harmonized_province <- function(
    lookup_actual,
    target_frame_date,
    situas_dir,
    lookup_dir,
    election_dates
) {
  
  lookup_key <- territorial_lookup_key()
  
  target_reference <- build_target_reference(
    target_frame_date = target_frame_date,
    situas_dir = situas_dir
  )
  
  lookup_harmonized_direct <- assign_direct_harmonized_province(
    lookup_actual = lookup_actual,
    target_reference = target_reference,
    target_frame_date = target_frame_date
  )
  
  historical_to_target <- load_historical_to_target_crosswalk(
    lookup_dir = lookup_dir,
    election_dates = election_dates
  )
  
  lookup_crosswalk <- apply_historical_to_target_crosswalk(
    lookup_harmonized_direct = lookup_harmonized_direct,
    target_reference = target_reference,
    historical_to_target = historical_to_target
  )
  
  resolved_crosswalk_keys <- lookup_crosswalk |>
    dplyr::select(dplyr::all_of(lookup_key))
  
  lookup_final <- lookup_harmonized_direct |>
    dplyr::anti_join(
      resolved_crosswalk_keys,
      by = lookup_key
    ) |>
    dplyr::mutate(
      territorial_match_note = NA_character_
    ) |>
    dplyr::bind_rows(lookup_crosswalk) |>
    dplyr::mutate(
      territorial_match_status = dplyr::case_when(
        .data$actual_match_status != "actual_resolved" ~
          .data$actual_match_status,
        !is.na(.data$province_harmonized) ~
          "harmonized_resolved",
        TRUE ~
          "harmonized_unresolved"
      )
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw,
      .data$district_raw,
      .data$plurinominal_college_raw,
      .data$uninominal_college_raw
    )
  
  check_unique_lookup_key(
    lookup_final,
    lookup_key,
    "lookup_final"
  )
  
  lookup_final
}