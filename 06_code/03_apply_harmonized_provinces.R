# 03_apply_harmonized_provinces.R
# Applies actual and harmonized province assignments to the analytical dataset.


source(here::here("06_code", "00_setup.R"))

message("Applying harmonized provinces to analytical dataset")


analytical_path <- here::here(
  "05_data",
  "processed",
  "camera_all_analytical.rds"
)

lookup_path <- here::here(
  "05_data",
  "lookup",
  "province_lookup_actual_harmonized.csv"
)

output_rds_path <- here::here(
  "05_data",
  "processed",
  "camera_all_harmonized.rds"
)

output_csv_path <- here::here(
  "05_data",
  "processed",
  "camera_all_harmonized.csv"
)

summary_dir <- here::here(
  "07_outputs",
  "tables"
)

fs::dir_create(summary_dir)

application_summary_path <- here::here(
  "07_outputs",
  "tables",
  "harmonized_province_application_summary.csv"
)

node_counts_path <- here::here(
  "07_outputs",
  "tables",
  "harmonized_province_node_counts.csv"
)

source_summary_path <- here::here(
  "07_outputs",
  "tables",
  "harmonized_province_source_summary.csv"
)

unmatched_path <- here::here(
  "07_outputs",
  "tables",
  "harmonized_province_unmatched_rows.csv"
)

validation_dir <- here::here(
  "05_data",
  "processed",
  "validation_camera_analytical_harmonized"
)


if (!file.exists(analytical_path)) {
  stop("Missing analytical dataset: ", analytical_path)
}

analytical <- load_tabular_file(analytical_path)

lookup <- load_province_lookup(lookup_path)

harmonized <- apply_harmonized_province_lookup(
  analytical = analytical,
  lookup = lookup,
  unmatched_path = unmatched_path
)

# -----------------------------------------------------------------------------
# List-name overrides
# -----------------------------------------------------------------------------

list_name_override_path <- file.path(
  path_data_lookup,
  "list_name_overrides.csv"
)

if (file.exists(list_name_override_path)) {
  list_name_overrides <- readr::read_csv(
    list_name_override_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ) |>
    dplyr::transmute(
      election_date = as.character(.data$election_date),
      list_name = as.character(.data$bad_list_name),
      list_name_override = as.character(.data$correct_list_name),
      override_reason = as.character(.data$reason)
    ) |>
    dplyr::distinct(
      .data$election_date,
      .data$list_name,
      .keep_all = TRUE
    )
  
  harmonized <- harmonized |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      list_name = as.character(.data$list_name)
    ) |>
    dplyr::left_join(
      list_name_overrides,
      by = c("election_date", "list_name")
    ) |>
    dplyr::mutate(
      list_name = dplyr::coalesce(
        .data$list_name_override,
        .data$list_name
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c("list_name_override", "override_reason"))
    )
}

validate_harmonized_analytical_dataset(
  harmonized = harmonized,
  validation_dir = validation_dir
)

summaries <- build_harmonized_application_summaries(
  harmonized = harmonized,
  lookup = lookup,
  output_rds_path = output_rds_path,
  output_csv_path = output_csv_path
)

write_harmonized_dataset_outputs(
  harmonized = harmonized,
  summaries = summaries,
  output_rds_path = output_rds_path,
  output_csv_path = output_csv_path,
  application_summary_path = application_summary_path,
  node_counts_path = node_counts_path,
  source_summary_path = source_summary_path
)





message("Harmonized province assignment summary:")
print(summaries$application_summary, width = Inf)

message("Harmonized province coverage:")
print(summaries$coverage_flags, n = Inf, width = Inf)

message("Territorial source summary:")
print(summaries$source_summary, n = Inf, width = Inf)

message("Done: harmonized analytical dataset")
