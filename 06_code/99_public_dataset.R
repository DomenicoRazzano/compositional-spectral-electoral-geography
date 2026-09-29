source(here::here("06_code", "00_setup.R"))

harmonized_path <- here::here("05_data", "processed", "camera_all_harmonized.rds")

public_rds_path <- here::here("05_data", "public", "camera_all_harmonized_public.rds") 

public_csv_path <- here::here("05_data", "public", "camera_all_harmonized_public.csv") 

harmonized <- load_tabular_file(harmonized_path) 

public_columns <- c(
  "election_date",
  "year",
  "source_dataset",
  "district_raw",
  "plurinominal_college_raw",
  "uninominal_college_raw",
  "municipality",
  "province_raw",
  "province_raw_source",
  "province_actual",
  "region_actual",
  "province_harmonized",
  "region_harmonized",
  "province_actual_source",
  "province_harmonized_source",
  "registered_voters",
  "voters",
  "abstentions",
  "blank_ballots",
  "null_ballots",
  "list_name",
  "list_votes",
  "supplement_type",
  "supplement_reason"
)

missing_public_columns <- setdiff(public_columns, names(harmonized)) 

if (length(missing_public_columns) > 0) {
  stop("Missing public-release columns: ", paste(missing_public_columns, collapse = ", "))
}

public_release <- harmonized |> 
  dplyr::select(dplyr::all_of(public_columns)) 

write_project_rds(public_release, public_rds_path) 
write_project_csv(public_release, public_csv_path)

message("Public release written successfully.")
