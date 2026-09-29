# territorial_outputs.R
# Diagnostics and output writers for territorial lookup construction.


build_territorial_lookup_diagnostics <- function(lookup_final) {
  
  lookup_summary <- lookup_final |>
    dplyr::count(
      territorial_match_status,
      province_actual_source,
      province_harmonized_source,
      name = "n_rows",
      sort = TRUE
    )
  
  unresolved <- lookup_final |>
    dplyr::filter(.data$territorial_match_status != "harmonized_resolved")
  
  harmonized_unresolved_template <- lookup_final |>
    dplyr::filter(
      .data$territorial_match_status == "harmonized_unresolved",
      .data$actual_match_status == "actual_resolved"
    ) |>
    dplyr::distinct(
      election_date,
      year,
      municipality,
      province_raw,
      province_raw_source,
      municipality_match,
      municipality_situas,
      cadastral_code,
      province_actual,
      province_actual_code,
      province_actual_abbrev,
      region_actual,
      district_raw,
      plurinominal_college_raw,
      uninominal_college_raw
    ) |>
    dplyr::mutate(
      municipality_target = NA_character_,
      harmonization_reason = NA_character_
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw
    )
  
  actual_unresolved_template <- lookup_final |>
    dplyr::filter(.data$territorial_match_status == "actual_unresolved") |>
    dplyr::distinct(
      election_date,
      year,
      municipality,
      province_raw,
      province_raw_source,
      municipality_match,
      municipality_parent,
      municipality_dash_parent,
      district_raw,
      plurinominal_college_raw,
      uninominal_college_raw
    ) |>
    dplyr::mutate(
      municipality_situas_manual = NA_character_,
      province_actual_manual = NA_character_,
      actual_resolution_reason = NA_character_
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw
    )
  
  actual_ambiguous_template <- lookup_final |>
    dplyr::filter(.data$territorial_match_status == "actual_ambiguous") |>
    dplyr::distinct(
      election_date,
      year,
      municipality,
      province_raw,
      province_raw_source,
      municipality_match,
      candidate_actual_provinces,
      district_raw,
      plurinominal_college_raw,
      uninominal_college_raw
    ) |>
    dplyr::mutate(
      province_actual_manual = NA_character_,
      actual_resolution_reason = NA_character_
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality,
      .data$province_raw
    )
  
  actual_unresolved_summary <- actual_unresolved_template |>
    dplyr::count(
      municipality,
      municipality_match,
      municipality_parent,
      municipality_dash_parent,
      province_raw,
      province_raw_source,
      name = "n_rows",
      sort = TRUE
    )
  
  actual_ambiguous_summary <- actual_ambiguous_template |>
    dplyr::count(
      municipality,
      municipality_match,
      province_raw,
      province_raw_source,
      candidate_actual_provinces,
      name = "n_rows",
      sort = TRUE
    )
  
  harmonized_unresolved_summary <- harmonized_unresolved_template |>
    dplyr::count(
      municipality,
      municipality_match,
      province_raw,
      municipality_situas,
      cadastral_code,
      province_actual,
      name = "n_rows",
      sort = TRUE
    )
  
  node_counts <- lookup_final |>
    dplyr::filter(.data$territorial_match_status == "harmonized_resolved") |>
    dplyr::group_by(.data$election_date, .data$year) |>
    dplyr::summarise(
      n_municipality_units = dplyr::n(),
      n_actual_provinces = dplyr::n_distinct(.data$province_actual),
      n_harmonized_provinces = dplyr::n_distinct(.data$province_harmonized),
      .groups = "drop"
    )
  
  actual_name_aliases_template <- actual_unresolved_summary |>
    dplyr::transmute(
      election_date = "ALL",
      municipality_match = .data$municipality_match,
      province_raw = .data$province_raw,
      municipality_situas_manual = NA_character_,
      actual_resolution_reason = NA_character_,
      n_rows = .data$n_rows
    )
  
  actual_province_manual_resolutions_template <- actual_ambiguous_template |>
    dplyr::transmute(
      election_date = .data$election_date,
      municipality = .data$municipality,
      province_raw = .data$province_raw,
      district_raw = .data$district_raw,
      plurinominal_college_raw = .data$plurinominal_college_raw,
      uninominal_college_raw = .data$uninominal_college_raw,
      province_actual_manual = NA_character_,
      actual_resolution_reason = NA_character_
    )
  
  historical_to_target_crosswalk_all_template <- harmonized_unresolved_summary |>
    dplyr::transmute(
      election_date = "ALL",
      municipality_match = .data$municipality_match,
      province_raw = .data$province_raw,
      municipality_target = NA_character_,
      harmonization_reason = NA_character_,
      n_rows = .data$n_rows,
      municipality_situas = .data$municipality_situas,
      cadastral_code = .data$cadastral_code,
      province_actual = .data$province_actual
    )
  
  list(
    lookup_summary = lookup_summary,
    unresolved = unresolved,
    harmonized_unresolved_template = harmonized_unresolved_template,
    harmonized_unresolved_summary = harmonized_unresolved_summary,
    actual_unresolved_template = actual_unresolved_template,
    actual_unresolved_summary = actual_unresolved_summary,
    actual_ambiguous_template = actual_ambiguous_template,
    actual_ambiguous_summary = actual_ambiguous_summary,
    node_counts = node_counts,
    actual_name_aliases_template = actual_name_aliases_template,
    actual_province_manual_resolutions_template =
      actual_province_manual_resolutions_template,
    historical_to_target_crosswalk_all_template =
      historical_to_target_crosswalk_all_template
  )
}


write_territorial_lookup_outputs <- function(lookup_final, diagnostics, lookup_dir) {
  
  write_project_csv(
    lookup_final,
    file.path(lookup_dir, "province_lookup_actual_harmonized.csv")
  )
  
  write_project_csv(
    diagnostics$lookup_summary,
    file.path(lookup_dir, "province_lookup_actual_harmonized_summary.csv")
  )
  
  write_project_csv(
    diagnostics$unresolved,
    file.path(lookup_dir, "province_lookup_harmonized_unresolved.csv")
  )
  
  write_project_csv(
    diagnostics$harmonized_unresolved_template,
    file.path(lookup_dir, "historical_to_target_municipality_crosswalk_template.csv")
  )
  
  write_project_csv(
    diagnostics$harmonized_unresolved_summary,
    file.path(lookup_dir, "historical_to_target_municipality_crosswalk_summary.csv")
  )
  
  write_project_csv(
    diagnostics$actual_unresolved_template,
    file.path(lookup_dir, "actual_unresolved_template.csv")
  )
  
  write_project_csv(
    diagnostics$actual_unresolved_summary,
    file.path(lookup_dir, "actual_unresolved_summary.csv")
  )
  
  write_project_csv(
    diagnostics$actual_ambiguous_template,
    file.path(lookup_dir, "actual_ambiguous_template.csv")
  )
  
  write_project_csv(
    diagnostics$actual_ambiguous_summary,
    file.path(lookup_dir, "actual_ambiguous_summary.csv")
  )
  
  write_project_csv(
    diagnostics$node_counts,
    file.path(lookup_dir, "province_lookup_node_counts.csv")
  )
  
  write_project_csv(
    diagnostics$actual_name_aliases_template,
    file.path(lookup_dir, "actual_name_aliases_template.csv")
  )
  
  write_project_csv(
    diagnostics$actual_province_manual_resolutions_template,
    file.path(lookup_dir, "actual_province_manual_resolutions_template.csv")
  )
  
  write_project_csv(
    diagnostics$historical_to_target_crosswalk_all_template,
    file.path(lookup_dir, "historical_to_target_municipality_crosswalk_all_template.csv")
  )
  
  invisible(lookup_dir)
}