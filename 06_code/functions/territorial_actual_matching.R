# territorial_actual_matching.R
# Historical province assignment for electoral reporting units.


build_electoral_units <- function(analytical) {
  lookup_key <- territorial_lookup_key()
  
  electoral_units <- analytical |>
    dplyr::distinct(
      election_date = as.character(.data$election_date),
      year = as.integer(.data$year),
      municipality = as.character(.data$municipality),
      province_raw = as.character(.data$province_raw),
      province_raw_source = as.character(.data$province_raw_source),
      district_raw = as.character(.data$district_raw),
      plurinominal_college_raw = as.character(.data$plurinominal_college_raw),
      uninominal_college_raw = as.character(.data$uninominal_college_raw)
    ) |>
    dplyr::mutate(
      municipality_match = normalize_name_for_lookup(.data$municipality),
      municipality_parent = extract_parent_municipality(.data$municipality),
      municipality_dash_parent = extract_parent_from_dash_label(.data$municipality),
      province_raw_match = normalize_province_for_lookup(.data$province_raw)
    )
  
  check_unique_lookup_key(
    electoral_units,
    lookup_key,
    "electoral_units"
  )
  
  electoral_units
}


build_actual_situas_reference <- function(election_dates, situas_dir) {
  situas_actual_alias <- build_situas_alias_reference(
    election_dates = election_dates,
    input_dir = situas_dir
  ) |>
    dplyr::mutate(
      province_actual_match = normalize_province_for_lookup(.data$province_actual)
    )
  
  actual_alias_uniqueness <- situas_actual_alias |>
    dplyr::group_by(.data$election_date, .data$municipality_alias_match) |>
    dplyr::summarise(
      n_actual_provinces = dplyr::n_distinct(.data$province_actual),
      candidate_actual_provinces = paste(
        sort(unique(.data$province_actual)),
        collapse = " | "
      ),
      .groups = "drop"
    )
  
  situas_actual_unique <- situas_actual_alias |>
    dplyr::left_join(
      actual_alias_uniqueness,
      by = c("election_date", "municipality_alias_match")
    ) |>
    dplyr::filter(.data$n_actual_provinces == 1L) |>
    dplyr::distinct(
      election_date,
      municipality_alias_match,
      .keep_all = TRUE
    )
  
  actual_alias_province_uniqueness <- situas_actual_alias |>
    dplyr::group_by(
      .data$election_date,
      .data$municipality_alias_match,
      .data$province_actual_match
    ) |>
    dplyr::summarise(
      n_actual_municipalities = dplyr::n_distinct(
        .data$municipality_code_alphanumeric
      ),
      candidate_actual_municipalities = paste(
        sort(unique(.data$municipality_situas)),
        collapse = " | "
      ),
      .groups = "drop"
    )
  
  situas_actual_name_province_unique <- situas_actual_alias |>
    dplyr::left_join(
      actual_alias_province_uniqueness,
      by = c(
        "election_date",
        "municipality_alias_match",
        "province_actual_match"
      )
    ) |>
    dplyr::filter(.data$n_actual_municipalities == 1L) |>
    dplyr::distinct(
      election_date,
      municipality_alias_match,
      province_actual_match,
      .keep_all = TRUE
    )
  
  list(
    situas_actual_alias = situas_actual_alias,
    actual_alias_uniqueness = actual_alias_uniqueness,
    situas_actual_unique = situas_actual_unique,
    situas_actual_name_province_unique = situas_actual_name_province_unique
  )
}


make_actual_match_unique_name <- function(
    electoral_units,
    situas_actual_unique,
    actual_alias_uniqueness,
    key_col,
    match_source_label
) {
  join_by <- stats::setNames(
    c("election_date", "municipality_alias_match"),
    c("election_date", key_col)
  )
  
  electoral_units |>
    dplyr::filter(!is.na(.data[[key_col]])) |>
    dplyr::left_join(
      situas_actual_unique |>
        dplyr::select(
          election_date,
          municipality_alias_match,
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          province_actual_match,
          region_actual,
          region_actual_code
        ),
      by = join_by
    ) |>
    dplyr::left_join(
      actual_alias_uniqueness,
      by = join_by
    ) |>
    dplyr::mutate(
      province_actual_source = dplyr::case_when(
        !is.na(.data$province_actual) ~ match_source_label,
        is.na(.data$province_actual) & .data$n_actual_provinces > 1 ~
          "actual_ambiguous_name",
        TRUE ~ NA_character_
      ),
      actual_match_status = dplyr::case_when(
        !is.na(.data$province_actual) ~ "actual_resolved",
        .data$n_actual_provinces > 1 ~ "actual_ambiguous",
        TRUE ~ "actual_unresolved"
      ),
      actual_match_key_used = key_col,
      actual_manual_reason = NA_character_
    )
}


make_actual_match_raw_province <- function(
    electoral_units,
    situas_actual_name_province_unique,
    key_col,
    match_source_label
) {
  join_by <- stats::setNames(
    c("election_date", "municipality_alias_match", "province_actual_match"),
    c("election_date", key_col, "province_raw_match")
  )
  
  electoral_units |>
    dplyr::filter(
      !is.na(.data[[key_col]]),
      !is.na(.data$province_raw_match)
    ) |>
    dplyr::left_join(
      situas_actual_name_province_unique |>
        dplyr::select(
          election_date,
          municipality_alias_match,
          province_actual_match,
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code
        ),
      by = join_by
    ) |>
    dplyr::mutate(
      n_actual_provinces = NA_integer_,
      candidate_actual_provinces = NA_character_,
      province_actual_source = dplyr::case_when(
        !is.na(.data$province_actual) ~ match_source_label,
        TRUE ~ NA_character_
      ),
      actual_match_status = dplyr::case_when(
        !is.na(.data$province_actual) ~ "actual_resolved",
        TRUE ~ "actual_unresolved"
      ),
      actual_match_key_used = key_col,
      actual_manual_reason = NA_character_
    )
}


load_actual_name_aliases <- function(lookup_dir, election_dates) {
  path <- file.path(lookup_dir, "actual_name_aliases.csv")
  
  if (!file.exists(path)) {
    return(tibble::tibble(
      election_date = character(),
      municipality_match = character(),
      province_raw_match_manual = character(),
      municipality_situas_manual = character(),
      municipality_situas_manual_match = character(),
      actual_resolution_reason = character(),
      manual_scope_priority = integer()
    ))
  }
  
  raw <- load_tabular_file(path)
  
  required_cols <- c(
    "election_date",
    "municipality_match",
    "province_raw",
    "municipality_situas_manual",
    "actual_resolution_reason"
  )
  
  missing_cols <- setdiff(required_cols, names(raw))
  
  if (length(missing_cols) > 0) {
    stop(
      "actual_name_aliases.csv is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  out <- raw |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      municipality_match = normalize_name_for_lookup(.data$municipality_match),
      province_raw_match_manual =
        normalize_province_for_lookup(.data$province_raw),
      municipality_situas_manual =
        as.character(.data$municipality_situas_manual),
      municipality_situas_manual_match =
        normalize_name_for_lookup(.data$municipality_situas_manual),
      actual_resolution_reason =
        as.character(.data$actual_resolution_reason)
    ) |>
    dplyr::select(
      election_date,
      municipality_match,
      province_raw_match_manual,
      municipality_situas_manual,
      municipality_situas_manual_match,
      actual_resolution_reason
    ) |>
    expand_all_election_dates(election_dates)
  
  check_unique_manual_key(
    out,
    c(
      "election_date",
      "municipality_match",
      "province_raw_match_manual"
    ),
    "actual_name_aliases"
  )
  
  out
}


load_actual_province_overrides <- function(lookup_dir, election_dates) {
  path <- file.path(lookup_dir, "actual_province_manual_resolutions.csv")
  
  if (!file.exists(path)) {
    return(tibble::tibble(
      election_date = character(),
      municipality_match = character(),
      province_raw_match_manual = character(),
      district_raw_manual = character(),
      plurinominal_college_raw_manual = character(),
      uninominal_college_raw_manual = character(),
      province_actual_manual = character(),
      province_actual_manual_match = character(),
      actual_resolution_reason = character(),
      manual_scope_priority = integer()
    ))
  }
  
  raw <- load_tabular_file(path)
  
  required_cols <- c(
    "election_date",
    "municipality",
    "province_raw",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_actual_manual",
    "actual_resolution_reason"
  )
  
  missing_cols <- setdiff(required_cols, names(raw))
  
  if (length(missing_cols) > 0) {
    stop(
      "actual_province_manual_resolutions.csv is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  out <- raw |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      municipality_match = normalize_name_for_lookup(.data$municipality),
      province_raw_match_manual =
        normalize_province_for_lookup(.data$province_raw),
      district_raw_manual = as.character(.data$district_raw),
      plurinominal_college_raw_manual =
        as.character(.data$plurinominal_college_raw),
      uninominal_college_raw_manual =
        as.character(.data$uninominal_college_raw),
      province_actual_manual =
        as.character(.data$province_actual_manual),
      province_actual_manual_match =
        normalize_province_for_lookup(.data$province_actual_manual),
      actual_resolution_reason =
        as.character(.data$actual_resolution_reason)
    ) |>
    dplyr::select(
      election_date,
      municipality_match,
      province_raw_match_manual,
      district_raw_manual,
      plurinominal_college_raw_manual,
      uninominal_college_raw_manual,
      province_actual_manual,
      province_actual_manual_match,
      actual_resolution_reason
    ) |>
    expand_all_election_dates(election_dates)
  
  check_unique_manual_key(
    out,
    c(
      "election_date",
      "municipality_match",
      "province_raw_match_manual",
      "district_raw_manual",
      "plurinominal_college_raw_manual",
      "uninominal_college_raw_manual"
    ),
    "actual_overrides"
  )
  
  out
}


assign_actual_province <- function(
    electoral_units,
    actual_reference,
    lookup_dir,
    election_dates
) {
  lookup_key <- territorial_lookup_key()
  
  actual_direct_raw <- make_actual_match_raw_province(
    electoral_units,
    actual_reference$situas_actual_name_province_unique,
    "municipality_match",
    "situas_date_specific_name_province_raw_match"
  )
  
  actual_direct_unique <- make_actual_match_unique_name(
    electoral_units,
    actual_reference$situas_actual_unique,
    actual_reference$actual_alias_uniqueness,
    "municipality_match",
    "situas_date_specific_name_match"
  )
  
  actual_parent_raw <- make_actual_match_raw_province(
    electoral_units,
    actual_reference$situas_actual_name_province_unique,
    "municipality_parent",
    "situas_parent_municipality_province_raw_match"
  )
  
  actual_parent_unique <- make_actual_match_unique_name(
    electoral_units,
    actual_reference$situas_actual_unique,
    actual_reference$actual_alias_uniqueness,
    "municipality_parent",
    "situas_parent_municipality_match"
  )
  
  actual_dash_raw <- make_actual_match_raw_province(
    electoral_units,
    actual_reference$situas_actual_name_province_unique,
    "municipality_dash_parent",
    "situas_dash_parent_municipality_province_raw_match"
  )
  
  actual_dash_unique <- make_actual_match_unique_name(
    electoral_units,
    actual_reference$situas_actual_unique,
    actual_reference$actual_alias_uniqueness,
    "municipality_dash_parent",
    "situas_dash_parent_municipality_match"
  )
  
  actual_name_aliases <- load_actual_name_aliases(
    lookup_dir = lookup_dir,
    election_dates = election_dates
  )
  
  actual_alias_base <- electoral_units |>
    dplyr::left_join(
      actual_name_aliases,
      by = c("election_date", "municipality_match"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(.data$municipality_situas_manual_match),
      is.na(.data$province_raw_match_manual) |
        is.na(.data$province_raw_match) |
        .data$province_raw_match_manual == .data$province_raw_match
    )
  
  actual_alias_manual_raw <- actual_alias_base |>
    dplyr::filter(!is.na(.data$province_raw_match_manual)) |>
    dplyr::left_join(
      actual_reference$situas_actual_name_province_unique |>
        dplyr::select(
          election_date,
          municipality_alias_match,
          province_actual_match,
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code
        ),
      by = c(
        "election_date",
        "municipality_situas_manual_match" = "municipality_alias_match",
        "province_raw_match_manual" = "province_actual_match"
      )
    )
  
  actual_alias_manual_unique <- actual_alias_base |>
    dplyr::filter(is.na(.data$province_raw_match_manual)) |>
    dplyr::left_join(
      actual_reference$situas_actual_unique |>
        dplyr::select(
          election_date,
          municipality_alias_match,
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          province_actual_match,
          region_actual,
          region_actual_code
        ),
      by = c(
        "election_date",
        "municipality_situas_manual_match" = "municipality_alias_match"
      )
    )
  
  actual_alias_manual <- dplyr::bind_rows(
    actual_alias_manual_raw,
    actual_alias_manual_unique
  ) |>
    dplyr::mutate(
      n_actual_provinces = NA_integer_,
      candidate_actual_provinces = NA_character_,
      province_actual_source = dplyr::case_when(
        !is.na(.data$province_actual) ~ "manual_actual_name_alias",
        TRUE ~ NA_character_
      ),
      actual_match_status = dplyr::case_when(
        !is.na(.data$province_actual) ~ "actual_resolved",
        TRUE ~ "actual_unresolved"
      ),
      actual_match_key_used = "manual_actual_name_alias",
      actual_manual_reason = .data$actual_resolution_reason
    )
  
  actual_overrides <- load_actual_province_overrides(
    lookup_dir = lookup_dir,
    election_dates = election_dates
  )
  
  actual_override_base <- electoral_units |>
    dplyr::left_join(
      actual_overrides,
      by = c("election_date", "municipality_match"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(.data$province_actual_manual_match),
      is.na(.data$province_raw_match_manual) |
        is.na(.data$province_raw_match) |
        .data$province_raw_match_manual == .data$province_raw_match,
      is.na(.data$district_raw_manual) |
        is.na(.data$district_raw) |
        .data$district_raw_manual == .data$district_raw,
      is.na(.data$plurinominal_college_raw_manual) |
        is.na(.data$plurinominal_college_raw) |
        .data$plurinominal_college_raw_manual == .data$plurinominal_college_raw,
      is.na(.data$uninominal_college_raw_manual) |
        is.na(.data$uninominal_college_raw) |
        .data$uninominal_college_raw_manual == .data$uninominal_college_raw
    )
  
  actual_override_manual <- actual_override_base |>
    dplyr::left_join(
      actual_reference$situas_actual_name_province_unique |>
        dplyr::select(
          election_date,
          municipality_alias_match,
          province_actual_match,
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code
        ),
      by = c(
        "election_date",
        "municipality_match" = "municipality_alias_match",
        "province_actual_manual_match" = "province_actual_match"
      )
    ) |>
    dplyr::mutate(
      n_actual_provinces = NA_integer_,
      candidate_actual_provinces = NA_character_,
      province_actual_source = dplyr::case_when(
        !is.na(.data$province_actual) ~ "manual_actual_province_override",
        TRUE ~ NA_character_
      ),
      actual_match_status = dplyr::case_when(
        !is.na(.data$province_actual) ~ "actual_resolved",
        TRUE ~ "actual_unresolved"
      ),
      actual_match_key_used = "manual_actual_province_override",
      actual_manual_reason = .data$actual_resolution_reason
    )
  
  lookup_actual <- dplyr::bind_rows(
    actual_direct_raw,
    actual_direct_unique,
    actual_parent_raw,
    actual_parent_unique,
    actual_dash_raw,
    actual_dash_unique,
    actual_alias_manual,
    actual_override_manual
  ) |>
    dplyr::mutate(
      actual_match_priority = dplyr::case_when(
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_date_specific_name_province_raw_match" ~ 1L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_date_specific_name_match" ~ 2L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_parent_municipality_province_raw_match" ~ 3L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_parent_municipality_match" ~ 4L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_dash_parent_municipality_province_raw_match" ~ 5L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "situas_dash_parent_municipality_match" ~ 6L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "manual_actual_name_alias" ~ 7L,
        .data$actual_match_status == "actual_resolved" &
          .data$province_actual_source ==
          "manual_actual_province_override" ~ 8L,
        .data$actual_match_status == "actual_ambiguous" ~ 90L,
        TRUE ~ 99L
      )
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw,
      .data$district_raw,
      .data$plurinominal_college_raw,
      .data$uninominal_college_raw,
      .data$actual_match_priority
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(lookup_key)),
      .keep_all = TRUE
    )
  
  check_unique_lookup_key(
    lookup_actual,
    lookup_key,
    "lookup_actual"
  )
  
  lookup_actual
}