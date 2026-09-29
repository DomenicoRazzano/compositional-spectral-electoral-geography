# territorial_reference.R
# Base helpers and SITUAS reference tables for territorial matching.


normalize_name_for_lookup <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("’|`|´", "'") |>
    stringr::str_replace_all("/", " ") |>
    stringr::str_replace_all("-", " ") |>
    stringr::str_replace_all("\\+", " ") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish() |>
    dplyr::na_if("")
}


normalize_context_label <- function(x) {
  x |>
    as.character() |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("’|`|´", "'") |>
    stringr::str_squish() |>
    dplyr::na_if("")
}


normalize_province_for_lookup <- function(x) {
  x_norm <- normalize_name_for_lookup(x)
  
  dplyr::case_when(
    is.na(x_norm) ~ NA_character_,
    x_norm %in% c(
      "BOLZANO",
      "BOLZANO BOZEN",
      "BOZEN",
      "BOZEN BOLZANO",
      "BOLZANO BZ"
    ) ~ "BOLZANO",
    x_norm %in% c(
      "AOSTA",
      "VALLE D AOSTA",
      "VALLEE D AOSTE",
      "VALLE D AOSTA VALLEE D AOSTE"
    ) ~ "AOSTA",
    TRUE ~ x_norm
  )
}


extract_parent_municipality <- function(x) {
  x_clean <- normalize_name_for_lookup(x)
  
  dplyr::case_when(
    is.na(x_clean) ~ NA_character_,
    stringr::str_detect(x_clean, "^PARTE DI COMUNE DI ") ~
      stringr::str_remove(x_clean, "^PARTE DI COMUNE DI "),
    stringr::str_detect(x_clean, "^PARTE DEL COMUNE DI ") ~
      stringr::str_remove(x_clean, "^PARTE DEL COMUNE DI "),
    stringr::str_detect(x_clean, "^PARTE DI COMUNE ") ~
      stringr::str_remove(x_clean, "^PARTE DI COMUNE "),
    stringr::str_detect(x_clean, "^PARTE DEL COMUNE ") ~
      stringr::str_remove(x_clean, "^PARTE DEL COMUNE "),
    stringr::str_detect(x_clean, "^[A-Z ]+ CENTRO( STORICO)?$") ~
      stringr::str_remove(x_clean, " CENTRO( STORICO)?$"),
    stringr::str_detect(x_clean, "^[A-Z ]+ EST$") ~
      stringr::str_remove(x_clean, " EST$"),
    stringr::str_detect(x_clean, "^[A-Z ]+ OVEST$") ~
      stringr::str_remove(x_clean, " OVEST$"),
    stringr::str_detect(x_clean, "^[A-Z ]+ NORD$") ~
      stringr::str_remove(x_clean, " NORD$"),
    stringr::str_detect(x_clean, "^[A-Z ]+ SUD$") ~
      stringr::str_remove(x_clean, " SUD$"),
    stringr::str_detect(x_clean, "^[A-Z ]+ [0-9]+$") ~
      stringr::str_remove(x_clean, " [0-9]+$"),
    TRUE ~ NA_character_
  )
}


extract_parent_from_dash_label <- function(x) {
  x_clean <- normalize_context_label(x)
  
  dplyr::case_when(
    is.na(x_clean) ~ NA_character_,
    stringr::str_detect(x_clean, "^PARTE DI COMUNE DI ") ~
      stringr::str_remove(x_clean, "^PARTE DI COMUNE DI "),
    stringr::str_detect(x_clean, "^PARTE DEL COMUNE DI ") ~
      stringr::str_remove(x_clean, "^PARTE DEL COMUNE DI "),
    stringr::str_detect(x_clean, "^PARTE DI COMUNE ") ~
      stringr::str_remove(x_clean, "^PARTE DI COMUNE "),
    stringr::str_detect(x_clean, "^PARTE DEL COMUNE ") ~
      stringr::str_remove(x_clean, "^PARTE DEL COMUNE "),
    stringr::str_detect(x_clean, " - ") ~
      stringr::str_extract(x_clean, "^[^-]+") |>
      stringr::str_squish(),
    TRUE ~ NA_character_
  )
}


territorial_lookup_key <- function() {
  c(
    "election_date",
    "municipality",
    "province_raw",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw"
  )
}


check_situas_files <- function(election_dates, situas_dir) {
  expected_files <- file.path(
    situas_dir,
    paste0("situas_comuni_", election_dates, ".csv")
  )
  
  missing_files <- expected_files[!file.exists(expected_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing SITUAS files:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  invisible(TRUE)
}


check_unique_lookup_key <- function(x, key, object_name) {
  duplicated_keys <- x |>
    dplyr::count(
      dplyr::across(dplyr::all_of(key)),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicated_keys) > 0) {
    print(duplicated_keys, n = 50)
    stop(object_name, " contains duplicate lookup keys.")
  }
  
  invisible(TRUE)
}


expand_all_election_dates <- function(tbl, election_dates) {
  if (!"election_date" %in% names(tbl)) {
    stop("Manual table must contain election_date.")
  }
  
  tbl <- tbl |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      manual_scope_priority = dplyr::if_else(
        .data$election_date == "ALL",
        2L,
        1L
      )
    )
  
  tbl_specific <- tbl |>
    dplyr::filter(.data$election_date != "ALL")
  
  tbl_all <- tbl |>
    dplyr::filter(.data$election_date == "ALL") |>
    dplyr::select(-election_date) |>
    tidyr::crossing(election_date = election_dates)
  
  dplyr::bind_rows(tbl_specific, tbl_all) |>
    dplyr::mutate(election_date = as.character(.data$election_date))
}


check_unique_manual_key <- function(tbl, key, object_name) {
  duplicated_keys <- tbl |>
    dplyr::count(
      dplyr::across(dplyr::all_of(key)),
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicated_keys) > 0) {
    print(duplicated_keys, n = 50)
    stop(object_name, " contains duplicate manual keys.")
  }
  
  invisible(TRUE)
}


standardize_situas_file <- function(file_path, election_date) {
  raw <- load_tabular_file(file_path, delimiter = ";")
  
  required <- c(
    "Codice Regione",
    "Codice Provincia/Uts",
    "Codice Comune (alfanumerico)",
    "Codice Comune (numerico)",
    "Comune",
    "Comune (dizione italiana)",
    "Comune (dizione straniera)",
    "Regione",
    "Provincia/Uts",
    "Sigla automobilistica",
    "Codice catasto"
  )
  
  missing_required <- setdiff(required, names(raw))
  
  if (length(missing_required) > 0) {
    stop(
      "SITUAS file is missing required columns: ",
      paste(missing_required, collapse = ", "),
      "\nFile: ", basename(file_path)
    )
  }
  
  raw |>
    dplyr::transmute(
      election_date = as.character(election_date),
      municipality_situas = as.character(.data$Comune),
      municipality_ita = as.character(.data[["Comune (dizione italiana)"]]),
      municipality_foreign = as.character(.data[["Comune (dizione straniera)"]]),
      municipality_match = normalize_name_for_lookup(.data$Comune),
      municipality_ita_match = normalize_name_for_lookup(.data[["Comune (dizione italiana)"]]),
      municipality_foreign_match = normalize_name_for_lookup(.data[["Comune (dizione straniera)"]]),
      municipality_code_alphanumeric = as.character(.data[["Codice Comune (alfanumerico)"]]),
      municipality_code_numeric = as.character(.data[["Codice Comune (numerico)"]]),
      cadastral_code = as.character(.data[["Codice catasto"]]),
      province_actual = stringr::str_to_upper(
        stringr::str_squish(as.character(.data[["Provincia/Uts"]]))
      ),
      province_actual_code = as.character(.data[["Codice Provincia/Uts"]]),
      province_actual_abbrev = stringr::str_to_upper(
        stringr::str_squish(as.character(.data[["Sigla automobilistica"]]))
      ),
      region_actual = stringr::str_to_upper(
        stringr::str_squish(as.character(.data$Regione))
      ),
      region_actual_code = as.character(.data[["Codice Regione"]]),
      source_file = basename(file_path),
      source_type = "situas_date_specific"
    ) |>
    dplyr::filter(
      !is.na(.data$municipality_match),
      !is.na(.data$province_actual)
    ) |>
    dplyr::distinct()
}


expand_situas_aliases <- function(situas_standardized) {
  base <- situas_standardized |>
    dplyr::select(
      .data$election_date,
      .data$municipality_situas,
      .data$municipality_ita,
      .data$municipality_foreign,
      .data$municipality_match,
      .data$municipality_ita_match,
      .data$municipality_foreign_match,
      .data$municipality_code_alphanumeric,
      .data$municipality_code_numeric,
      .data$cadastral_code,
      .data$province_actual,
      .data$province_actual_code,
      .data$province_actual_abbrev,
      .data$region_actual,
      .data$region_actual_code,
      .data$source_file,
      .data$source_type
    )
  
  alias_main <- base |>
    dplyr::transmute(
      election_date,
      municipality_alias_raw = municipality_situas,
      municipality_alias_match = municipality_match,
      dplyr::across(
        c(
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code,
          source_file,
          source_type
        )
      )
    )
  
  alias_ita <- base |>
    dplyr::filter(!is.na(.data$municipality_ita_match)) |>
    dplyr::transmute(
      election_date,
      municipality_alias_raw = municipality_ita,
      municipality_alias_match = municipality_ita_match,
      dplyr::across(
        c(
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code,
          source_file,
          source_type
        )
      )
    )
  
  alias_foreign <- base |>
    dplyr::filter(!is.na(.data$municipality_foreign_match)) |>
    dplyr::transmute(
      election_date,
      municipality_alias_raw = municipality_foreign,
      municipality_alias_match = municipality_foreign_match,
      dplyr::across(
        c(
          municipality_situas,
          municipality_code_alphanumeric,
          municipality_code_numeric,
          cadastral_code,
          province_actual,
          province_actual_code,
          province_actual_abbrev,
          region_actual,
          region_actual_code,
          source_file,
          source_type
        )
      )
    )
  
  alias_split <- base |>
    dplyr::select(
      election_date,
      municipality_situas,
      municipality_code_alphanumeric,
      municipality_code_numeric,
      cadastral_code,
      province_actual,
      province_actual_code,
      province_actual_abbrev,
      region_actual,
      region_actual_code,
      source_file,
      source_type
    ) |>
    tidyr::separate_longer_delim(.data$municipality_situas, delim = "/") |>
    dplyr::mutate(
      municipality_alias_raw = .data$municipality_situas,
      municipality_alias_match = normalize_name_for_lookup(.data$municipality_alias_raw)
    ) |>
    dplyr::filter(!is.na(.data$municipality_alias_match)) |>
    dplyr::select(
      .data$election_date,
      .data$municipality_alias_raw,
      .data$municipality_alias_match,
      .data$municipality_situas,
      .data$municipality_code_alphanumeric,
      .data$municipality_code_numeric,
      .data$cadastral_code,
      .data$province_actual,
      .data$province_actual_code,
      .data$province_actual_abbrev,
      .data$region_actual,
      .data$region_actual_code,
      .data$source_file,
      .data$source_type
    )
  
  dplyr::bind_rows(
    alias_main,
    alias_ita,
    alias_foreign,
    alias_split
  ) |>
    dplyr::filter(!is.na(.data$municipality_alias_match)) |>
    dplyr::distinct()
}


load_situas_reference_by_election <- function(election_dates, input_dir) {
  purrr::map_dfr(election_dates, function(edate) {
    file_path <- file.path(
      input_dir,
      paste0("situas_comuni_", edate, ".csv")
    )
    
    if (!file.exists(file_path)) {
      stop("Missing SITUAS file for election date ", edate, ": ", file_path)
    }
    
    standardize_situas_file(
      file_path = file_path,
      election_date = edate
    )
  })
}


build_situas_alias_reference <- function(election_dates, input_dir) {
  load_situas_reference_by_election(
    election_dates = election_dates,
    input_dir = input_dir
  ) |>
    expand_situas_aliases()
}