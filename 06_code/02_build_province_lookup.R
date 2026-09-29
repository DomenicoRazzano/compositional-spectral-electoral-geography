# 02_build_province_lookup.R
# Builds actual and harmonized province assignments for analytical reporting units.


source(here::here("06_code", "00_setup.R"))

message("Building actual and harmonized province lookup")


analytical_path <- here::here(
  "05_data",
  "processed",
  "camera_all_analytical.rds"
)

situas_dir <- here::here(
  "05_data",
  "lookup",
  "situas_by_election_date"
)

lookup_dir <- here::here(
  "05_data",
  "lookup"
)

fs::dir_create(lookup_dir)

target_frame_date <- "20220925"

analytical <- load_tabular_file(analytical_path)

required <- c(
  "election_date",
  "year",
  "municipality",
  "province_raw",
  "province_raw_source",
  "district_raw",
  "plurinominal_college_raw",
  "uninominal_college_raw"
)

missing_required <- setdiff(required, names(analytical))

if (length(missing_required) > 0) {
  stop(
    "Analytical dataset missing required columns for territorial lookup: ",
    paste(missing_required, collapse = ", ")
  )
}

election_dates <- analytical |>
  dplyr::distinct(election_date = as.character(.data$election_date)) |>
  dplyr::arrange(.data$election_date) |>
  dplyr::pull(.data$election_date)

check_situas_files(
  election_dates = election_dates,
  situas_dir = situas_dir
)

electoral_units <- build_electoral_units(analytical)

actual_reference <- build_actual_situas_reference(
  election_dates = election_dates,
  situas_dir = situas_dir
)

lookup_actual <- assign_actual_province(
  electoral_units = electoral_units,
  actual_reference = actual_reference,
  lookup_dir = lookup_dir,
  election_dates = election_dates
)

lookup_final <- assign_harmonized_province(
  lookup_actual = lookup_actual,
  target_frame_date = target_frame_date,
  situas_dir = situas_dir,
  lookup_dir = lookup_dir,
  election_dates = election_dates
)

diagnostics <- build_territorial_lookup_diagnostics(lookup_final)

write_territorial_lookup_outputs(
  lookup_final = lookup_final,
  diagnostics = diagnostics,
  lookup_dir = lookup_dir
)

message("Done: actual and harmonized province lookup")
print(diagnostics$lookup_summary, n = Inf, width = Inf)
print(diagnostics$node_counts, n = Inf, width = Inf)

if (nrow(diagnostics$unresolved) > 0) {
  warning("Some units remain unresolved.")
}