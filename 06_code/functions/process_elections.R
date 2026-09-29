# process_elections.R
# Schema registry and pre-territorial processing for Camera datasets.


get_camera_schema_registry <- function() {
  
  list(
    
    first_republic = list(
      required_raw = c(
        "PROVINCIA",
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "SCHEDE_BIANCHE",
        "LISTA",
        "VOTI_LISTA"
      ),
      rename_map = c(
        district_raw = "CIRCOSCRIZIONE",
        province = "PROVINCIA",
        municipality = "COMUNE",
        registered_voters = "ELETTORI",
        voters = "VOTANTI",
        blank_ballots = "SCHEDE_BIANCHE",
        list_name = "LISTA",
        list_votes = "VOTI_LISTA"
      ),
      province_strategy = "raw"
    ),
    
    mattarellum_prop = list(
      required_raw = c(
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "SCHEDE_BIANCHE",
        "LISTA",
        "VOTI_LISTA"
      ),
      rename_map = c(
        district_raw = "CIRCOSCRIZIONE",
        uninominal_college_raw = "COLLEGIO",
        municipality = "COMUNE",
        registered_voters = "ELETTORI",
        voters = "VOTANTI",
        blank_ballots = "SCHEDE_BIANCHE",
        list_name = "LISTA",
        list_votes = "VOTI_LISTA"
      ),
      province_strategy = "not_in_raw"
    ),
    
    porcellum_italia = list(
      required_raw = c(
        "PROVINCIA",
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "SCHEDE_BIANCHE",
        "LISTA",
        "VOTI_LISTA"
      ),
      rename_map = c(
        province = "PROVINCIA",
        municipality = "COMUNE",
        registered_voters = "ELETTORI",
        voters = "VOTANTI",
        blank_ballots = "SCHEDE_BIANCHE",
        list_name = "LISTA",
        list_votes = "VOTI_LISTA"
      ),
      province_strategy = "raw",
      aliases = list(
        VOTI_LISTA = c("VOTI_LISTA", "VOTILISTA")
      )
    ),
    
    porcellum_vda = list(
      required_raw = c(
        "COMUNE",
        "ELETTORI_TOTALI",
        "VOTANTI_TOTALI",
        "SCHEDE_BIANCHE",
        "LISTA",
        "VOTI_LISTA"
      ),
      rename_map = c(
        municipality = "COMUNE",
        registered_voters = "ELETTORI_TOTALI",
        voters = "VOTANTI_TOTALI",
        blank_ballots = "SCHEDE_BIANCHE",
        list_name = "LISTA",
        list_votes = "VOTI_LISTA"
      ),
      province_strategy = "vda_fixed_raw_proxy"
    ),
    
    rosatellum_2018 = list(
      required_raw = c(
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "SCHEDE_BIANCHE",
        "LISTA"
      ),
      rename_map = c(
        district_raw = "CIRCOSCRIZIONE",
        plurinominal_college_raw = "COLLEGIOPLURINOMINALE",
        uninominal_college_raw = "COLLEGIOUNINOMINALE",
        municipality = "COMUNE",
        registered_voters = "ELETTORI",
        voters = "VOTANTI",
        blank_ballots = "SCHEDE_BIANCHE",
        list_name = "LISTA",
        list_votes = "VOTI_LISTA"
      ),
      province_strategy = "not_in_raw",
      fallback_raw = list(
        list_votes = "VOTI_CANDIDATO"
      )
    ),
    
    rosatellum_2022_italia = list(
      required_raw = c(
        "COMUNE",
        "ELETTORITOT",
        "VOTANTITOT",
        "SKBIANCHE",
        "DESCRLISTA",
        "VOTILISTA"
      ),
      rename_map = c(
        district_raw = "CIRC-REG",
        plurinominal_college_raw = "COLLPLURI",
        uninominal_college_raw = "COLLUNINOM",
        municipality = "COMUNE",
        registered_voters = "ELETTORITOT",
        voters = "VOTANTITOT",
        blank_ballots = "SKBIANCHE",
        list_name = "DESCRLISTA",
        list_votes = "VOTILISTA"
      ),
      province_strategy = "not_in_raw"
    ),
    
    rosatellum_2022_vda = list(
      required_raw = c(
        "COMUNE",
        "CONTRASSEGNO",
        "TOTVOTI"
      ),
      rename_map = c(
        municipality = "COMUNE",
        list_name = "CONTRASSEGNO",
        list_votes = "TOTVOTI"
      ),
      province_strategy = "vda_fixed_raw_proxy"
    )
    
  )
}


get_camera_file_catalog <- function() {
  
  catalog <- tibble::tibble(
    file_stub = c(
      "camera-19480418",
      "camera-19530607",
      "camera-19580525",
      "camera-19630428",
      "camera-19680519",
      "camera-19720507",
      "camera-19760620",
      "camera-19790603",
      "camera-19830626",
      "Camera-19870614",
      "camera-19920405",
      "camera-19940327_Proporzionale",
      "camera-19960421_Proporzionale",
      "camera-20010513_Proporzionale",
      "camera_italia-20060409",
      "camera_vaosta-20060409",
      "camera_italia-20080413",
      "camera_vaosta-20080413",
      "camera_italia-20130224",
      "camera_vaosta-20130224",
      "Camera2018_livComune",
      "Camera_Italia_LivComune",
      "Camera_VAosta_LivComune"
    ),
    schema_id = c(
      rep("first_republic", 11),
      rep("mattarellum_prop", 3),
      "porcellum_italia",
      "porcellum_vda",
      "porcellum_italia",
      "porcellum_vda",
      "porcellum_italia",
      "porcellum_vda",
      "rosatellum_2018",
      "rosatellum_2022_italia",
      "rosatellum_2022_vda"
    )
  )
  
  date_overrides <- tibble::tibble(
    file_stub = c(
      "Camera2018_livComune",
      "Camera_Italia_LivComune",
      "Camera_VAosta_LivComune"
    ),
    election_date_override = c(
      "20180304",
      "20220925",
      "20220925"
    )
  )
  
  catalog |>
    dplyr::mutate(
      election_date = stringr::str_extract(.data$file_stub, "\\d{8}")
    ) |>
    dplyr::left_join(
      date_overrides,
      by = "file_stub"
    ) |>
    dplyr::mutate(
      election_date = dplyr::coalesce(
        .data$election_date,
        .data$election_date_override
      )
    ) |>
    dplyr::select(
      file_stub,
      schema_id,
      election_date
    )
}


get_special_supplement_registry <- function() {
  
  tibble::tribble(
    ~supplement_id, ~election_date, ~year, ~type, ~candidate_file, ~scrutini_file,
    "vda_mattarellum_1994", "19940327", 1994L, "mattarellum_vda_uninominale",
    "Camera_19940327_Uninom_Cand&Contr.txt",
    "Camera_19940327_Uninom_Scrutini.txt",
    
    "vda_mattarellum_1996", "19960421", 1996L, "mattarellum_vda_uninominale",
    "Camera_19960421_Uninom_Cand&Contr.txt",
    "Camera_19960421_Uninom_Scrutini.txt",
    
    "vda_mattarellum_2001", "20010513", 2001L, "mattarellum_vda_uninominale",
    "Camera_20010513_Uninom_Cand&Contr.txt",
    "Camera_20010513_Uninom_Scrutini.txt"
  )
}


parse_uninominal_number <- function(x) {
  
  raw_chr <- as.character(x)
  raw_chr <- stringr::str_squish(raw_chr)
  raw_chr[raw_chr == ""] <- NA_character_
  raw_chr[raw_chr == "NA"] <- NA_character_
  
  value <- suppressWarnings(
    readr::parse_number(
      raw_chr,
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  )
  
  non_missing <- value[!is.na(value)]
  
  # Some uninominal files encode integer counts with two trailing decimal digits.
  if (
    length(non_missing) > 0 &&
    all(abs(non_missing - round(non_missing)) < 1e-8) &&
    all(non_missing %% 100 == 0) &&
    max(non_missing, na.rm = TRUE) >= 10000
  ) {
    value <- value / 100
  }
  
  value
}


normalize_uninominal_text <- function(x) {
  
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_squish() |>
    dplyr::na_if("NA")
}


build_vda_mattarellum_one_election <- function(
    candidate_path,
    scrutini_path,
    election_date,
    year
) {
  
  election_date_chr <- as.character(election_date)
  
  if (!file.exists(candidate_path)) {
    stop("Missing VdA candidate file: ", candidate_path)
  }
  
  if (!file.exists(scrutini_path)) {
    stop("Missing VdA scrutini file: ", scrutini_path)
  }
  
  cand_raw <- load_tabular_file(candidate_path) |>
    dplyr::rename_with(tolower)
  
  scrutini_raw <- load_tabular_file(scrutini_path) |>
    dplyr::rename_with(tolower)
  
  required_cand <- c(
    "circ",
    "coll",
    "comune",
    "cognome",
    "nome",
    "totvoti",
    "descrcontrass"
  )
  
  required_scrutini <- c(
    "circ",
    "coll",
    "comune",
    "elettoritot",
    "numvotantitotali",
    "votivalidi",
    "skbianche"
  )
  
  missing_cand <- setdiff(required_cand, names(cand_raw))
  missing_scrutini <- setdiff(required_scrutini, names(scrutini_raw))
  
  if (length(missing_cand) > 0) {
    stop(
      "Candidate file missing columns: ",
      paste(missing_cand, collapse = ", "),
      ". File: ",
      basename(candidate_path)
    )
  }
  
  if (length(missing_scrutini) > 0) {
    stop(
      "Scrutini file missing columns: ",
      paste(missing_scrutini, collapse = ", "),
      ". File: ",
      basename(scrutini_path)
    )
  }
  
  cand <- cand_raw |>
    dplyr::mutate(
      circ = normalize_uninominal_text(.data$circ),
      coll = normalize_uninominal_text(.data$coll),
      comune = normalize_uninominal_text(.data$comune),
      cognome = normalize_uninominal_text(.data$cognome),
      nome = normalize_uninominal_text(.data$nome),
      descrcontrass = normalize_uninominal_text(.data$descrcontrass),
      totvoti = parse_uninominal_number(.data$totvoti)
    ) |>
    dplyr::filter(.data$circ == "VALLE D'AOSTA")
  
  scrutini <- scrutini_raw |>
    dplyr::mutate(
      circ = normalize_uninominal_text(.data$circ),
      coll = normalize_uninominal_text(.data$coll),
      comune = normalize_uninominal_text(.data$comune),
      elettoritot = parse_uninominal_number(.data$elettoritot),
      numvotantitotali = parse_uninominal_number(.data$numvotantitotali),
      votivalidi = parse_uninominal_number(.data$votivalidi),
      skbianche = parse_uninominal_number(.data$skbianche)
    ) |>
    dplyr::filter(.data$circ == "VALLE D'AOSTA")
  
  if (nrow(cand) == 0) {
    stop("No Valle d'Aosta candidate rows found in: ", basename(candidate_path))
  }
  
  if (nrow(scrutini) == 0) {
    stop("No Valle d'Aosta scrutini rows found in: ", basename(scrutini_path))
  }
  
  # The same candidate may appear once for each supporting symbol.
  # We collapse those rows to one candidate-offer and keep the symbols
  # only for candidate-offer recoding.
  
  candidate_offers <- cand |>
    dplyr::group_by(
      .data$circ,
      .data$coll,
      .data$comune,
      .data$cognome,
      .data$nome,
      .data$totvoti
    ) |>
    dplyr::summarise(
      candidate_symbols = paste(
        sort(unique(stats::na.omit(.data$descrcontrass))),
        collapse = " + "
      ),
      n_supporting_symbols = dplyr::n_distinct(.data$descrcontrass),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      election_date = election_date_chr,
      candidate_name = stringr::str_squish(
        paste(.data$cognome, .data$nome)
      ),
      candidate_name = normalize_uninominal_text(.data$candidate_name),
      candidate_symbols = normalize_uninominal_text(.data$candidate_symbols)
    )
  
  duplicate_candidate_offers <- candidate_offers |>
    dplyr::count(
      .data$circ,
      .data$coll,
      .data$comune,
      .data$candidate_name,
      .data$candidate_symbols,
      .data$totvoti,
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicate_candidate_offers) > 0) {
    print(duplicate_candidate_offers, n = Inf)
    stop("Duplicate collapsed VdA candidate-offer rows.")
  }
  
  # Manual crosswalk from candidate-offers to list-equivalent components.
  dictionary_path <- here::here(
    "05_data",
    "lookup",
    "vda_mattarellum_candidate_offer_dictionary.csv"
  )
  
  if (!file.exists(dictionary_path)) {
    stop(
      "Missing VdA Mattarellum candidate-offer dictionary: ",
      dictionary_path
    )
  }
  
  vda_dictionary <- readr::read_csv(
    dictionary_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      election_date = readr::col_character(),
      candidate_name = readr::col_character(),
      candidate_symbols = readr::col_character(),
      list_equivalent = readr::col_character(),
      recoding_reason = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      candidate_name = normalize_uninominal_text(.data$candidate_name),
      candidate_symbols = normalize_uninominal_text(.data$candidate_symbols),
      list_equivalent = normalize_uninominal_text(.data$list_equivalent),
      recoding_reason = as.character(.data$recoding_reason)
    )
  
  duplicate_dictionary_keys <- vda_dictionary |>
    dplyr::count(
      .data$election_date,
      .data$candidate_name,
      .data$candidate_symbols,
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicate_dictionary_keys) > 0) {
    print(duplicate_dictionary_keys, n = Inf)
    stop(
      "Duplicate keys in vda_mattarellum_candidate_offer_dictionary.csv."
    )
  }
  
  candidate_offers <- candidate_offers |>
    dplyr::left_join(
      vda_dictionary |>
        dplyr::select(
          election_date,
          candidate_name,
          candidate_symbols,
          list_equivalent,
          recoding_reason
        ),
      by = c(
        "election_date",
        "candidate_name",
        "candidate_symbols"
      ),
      relationship = "many-to-one"
    )
  
  missing_dictionary <- candidate_offers |>
    dplyr::filter(is.na(.data$list_equivalent)) |>
    dplyr::distinct(
      election_date,
      candidate_name,
      candidate_symbols,
      totvoti
    )
  
  if (nrow(missing_dictionary) > 0) {
    print(missing_dictionary, n = Inf)
    stop(
      "Some VdA Mattarellum candidate-offers are missing from ",
      "vda_mattarellum_candidate_offer_dictionary.csv."
    )
  }
  
  duplicate_scrutini <- scrutini |>
    dplyr::count(
      .data$circ,
      .data$coll,
      .data$comune,
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicate_scrutini) > 0) {
    print(duplicate_scrutini, n = Inf)
    stop("Duplicate VdA scrutini rows.")
  }
  
  candidate_totals <- candidate_offers |>
    dplyr::group_by(
      .data$circ,
      .data$coll,
      .data$comune
    ) |>
    dplyr::summarise(
      total_candidate_votes = sum(.data$totvoti, na.rm = TRUE),
      n_candidate_offers = dplyr::n(),
      .groups = "drop"
    )
  
  accounting <- scrutini |>
    dplyr::left_join(
      candidate_totals,
      by = c("circ", "coll", "comune"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      total_candidate_votes = dplyr::coalesce(
        .data$total_candidate_votes,
        0
      ),
      valid_vote_gap = .data$votivalidi - .data$total_candidate_votes,
      null_ballots = .data$numvotantitotali -
        .data$skbianche -
        .data$total_candidate_votes
    )
  
  bad_valid <- accounting |>
    dplyr::filter(abs(.data$valid_vote_gap) > 1e-6)
  
  if (nrow(bad_valid) > 0) {
    print(
      bad_valid |>
        dplyr::select(
          circ,
          coll,
          comune,
          votivalidi,
          total_candidate_votes,
          valid_vote_gap
        ),
      n = Inf
    )
    stop(
      "VdA candidate votes do not sum to VOTIVALIDI in ",
      basename(candidate_path),
      ". This usually means candidate rows are not being collapsed correctly."
    )
  }
  
  bad_null <- accounting |>
    dplyr::filter(.data$null_ballots < 0)
  
  if (nrow(bad_null) > 0) {
    print(
      bad_null |>
        dplyr::select(
          circ,
          coll,
          comune,
          numvotantitotali,
          skbianche,
          total_candidate_votes,
          null_ballots
        ),
      n = Inf
    )
    stop("Negative derived null ballots in VdA supplement.")
  }
  
  candidate_offers |>
    dplyr::left_join(
      accounting |>
        dplyr::select(
          circ,
          coll,
          comune,
          registered_voters = elettoritot,
          voters = numvotantitotali,
          blank_ballots = skbianche,
          null_ballots
        ),
      by = c("circ", "coll", "comune"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      election_date = election_date_chr,
      year = as.integer(year),
      source_dataset = basename(candidate_path),
      district_raw = .data$circ,
      plurinominal_college_raw = NA_character_,
      uninominal_college_raw = .data$coll,
      province_raw = "AOSTA",
      province_raw_source = "vda_fixed_raw_proxy",
      municipality = .data$comune,
      registered_voters = .data$registered_voters,
      voters = .data$voters,
      abstentions = .data$registered_voters - .data$voters,
      blank_ballots = .data$blank_ballots,
      null_ballots = .data$null_ballots,
      list_name = .data$list_equivalent,
      list_votes = .data$totvoti,
      supplement_type = "mattarellum_vda_uninominal_candidate_offer",
      supplement_reason = "valle_d_aosta_absent_from_mattarellum_proportional_file"
    )
}


build_special_supplements <- function(raw_dir = path_data_raw) {
  
  registry <- get_special_supplement_registry()
  
  supplements <- purrr::pmap_dfr(
    registry,
    function(
    supplement_id,
    election_date,
    year,
    type,
    candidate_file,
    scrutini_file
    ) {
      
      if (type != "mattarellum_vda_uninominale") {
        stop("Unknown special supplement type: ", type)
      }
      
      candidate_path <- file.path(raw_dir, candidate_file)
      scrutini_path <- file.path(raw_dir, scrutini_file)
      
      message("Building supplement: ", supplement_id)
      
      build_vda_mattarellum_one_election(
        candidate_path = candidate_path,
        scrutini_path = scrutini_path,
        election_date = election_date,
        year = year
      )
    }
  )
  
  supplements
}


parse_numeric_strict <- function(x, dataset_name, variable_name) {
  
  raw_chr <- as.character(x)
  raw_chr <- stringr::str_squish(raw_chr)
  raw_chr[raw_chr == ""] <- NA_character_
  
  value <- suppressWarnings(
    readr::parse_number(
      raw_chr,
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      )
    )
  )
  
  n_failed <- sum(
    is.na(value) & !is.na(raw_chr)
  )
  
  if (n_failed > 0) {
    warning(
      "NAs introduced during numeric conversion in dataset ",
      dataset_name,
      " for variable ",
      variable_name,
      " (",
      n_failed,
      " values)."
    )
  }
  
  value
}


derive_raw_province_fields <- function(df, province_strategy) {
  
  if (!"province" %in% names(df)) {
    df$province <- NA_character_
  }
  
  province_raw <- stringr::str_to_upper(
    stringr::str_squish(as.character(df$province))
  )
  province_raw[province_raw == "NA"] <- NA_character_
  province_raw[province_raw == ""] <- NA_character_
  
  if (province_strategy == "vda_fixed_raw_proxy") {
    province_raw <- "AOSTA"
  }
  
  df$province_raw <- province_raw
  
  if (!province_strategy %in% c("raw", "not_in_raw", "vda_fixed_raw_proxy")) {
    stop("Unknown province strategy: ", province_strategy)
  }
  
  df$province_raw_source <- dplyr::case_when(
    province_strategy == "raw" & !is.na(df$province_raw) ~ "raw_source_field",
    province_strategy == "raw" & is.na(df$province_raw) ~ "missing_raw_source_field",
    province_strategy == "not_in_raw" ~ "not_available_in_raw_source",
    province_strategy == "vda_fixed_raw_proxy" ~ "vda_fixed_raw_proxy"
  )
  
  df$province <- NULL
  
  df
}


first_non_missing_number <- function(x) {
  
  x_non_missing <- x[!is.na(x)]
  
  if (length(x_non_missing) == 0) {
    return(NA_real_)
  }
  
  as.numeric(x_non_missing[[1]])
}


derive_null_ballots <- function(df) {
  
  reporting_unit_key <- c(
    "election_date",
    "source_dataset",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province_raw",
    "municipality"
  )
  
  required_cols <- c(
    reporting_unit_key,
    "voters",
    "blank_ballots",
    "list_votes"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "derive_null_ballots() is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  reporting_unit_totals <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(reporting_unit_key))
    ) |>
    dplyr::summarise(
      voters_ref = first_non_missing_number(.data$voters),
      blank_ballots_ref = first_non_missing_number(.data$blank_ballots),
      total_list_votes = sum(.data$list_votes, na.rm = TRUE),
      any_missing_voters = any(is.na(.data$voters)),
      any_missing_blank = any(is.na(.data$blank_ballots)),
      any_missing_list_votes = any(is.na(.data$list_votes)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      null_ballots = dplyr::if_else(
        .data$any_missing_voters |
          .data$any_missing_blank |
          .data$any_missing_list_votes,
        NA_real_,
        .data$voters_ref -
          .data$blank_ballots_ref -
          .data$total_list_votes
      )
    ) |>
    dplyr::select(
      dplyr::all_of(reporting_unit_key),
      null_ballots
    )
  
  df |>
    dplyr::select(
      -dplyr::any_of("null_ballots")
    ) |>
    dplyr::left_join(
      reporting_unit_totals,
      by = reporting_unit_key,
      relationship = "many-to-one"
    )
}


complete_vda_2022_manual_fields <- function(out, dataset_name, schema_id) {
  
  if (schema_id != "rosatellum_2022_vda") {
    return(out)
  }
  
  completion_path <- here::here(
    "05_data",
    "lookup",
    "vda_2022_manual_completion.csv"
  )
  
  if (!file.exists(completion_path)) {
    stop(
      "Missing required manual completion file for rosatellum_2022_vda: ",
      completion_path
    )
  }
  
  normalize_vda_completion_name <- function(x) {
    x |>
      as.character() |>
      iconv(from = "", to = "ASCII//TRANSLIT") |>
      stringr::str_to_upper() |>
      stringr::str_replace_all("’|`|´", "'") |>
      stringr::str_replace_all("-", " ") |>
      stringr::str_replace_all("[[:punct:]]", " ") |>
      stringr::str_squish() |>
      dplyr::na_if("")
  }
  
  completion <- readr::read_csv(
    completion_path,
    show_col_types = FALSE,
    col_types = readr::cols(
      election_date = readr::col_character(),
      municipality = readr::col_character(),
      registered_voters = readr::col_double(),
      voters = readr::col_double(),
      blank_ballots = readr::col_double(),
      null_ballots = readr::col_double(),
      raw_total_list_votes = readr::col_double(),
      idcom = readr::col_character(),
      municipality_site = readr::col_character(),
      source_url = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      municipality = stringr::str_to_upper(
        stringr::str_squish(as.character(.data$municipality))
      ),
      municipality = dplyr::na_if(.data$municipality, "NA"),
      municipality_key = normalize_vda_completion_name(.data$municipality)
    )
  
  duplicate_completion_keys <- completion |>
    dplyr::count(
      election_date,
      municipality_key,
      name = "n_rows"
    ) |>
    dplyr::filter(.data$n_rows > 1)
  
  if (nrow(duplicate_completion_keys) > 0) {
    print(duplicate_completion_keys)
    stop(
      "Duplicate normalized municipality keys found in vda_2022_manual_completion.csv."
    )
  }
  
  out_keyed <- out |>
    dplyr::mutate(
      municipality_key = normalize_vda_completion_name(.data$municipality)
    )
  
  raw_totals <- out_keyed |>
    dplyr::group_by(
      .data$election_date,
      .data$municipality_key
    ) |>
    dplyr::summarise(
      municipality_raw = dplyr::first(.data$municipality),
      observed_total_list_votes = sum(.data$list_votes, na.rm = TRUE),
      any_missing_list_votes = any(is.na(.data$list_votes)),
      .groups = "drop"
    )
  
  raw_not_in_manual <- raw_totals |>
    dplyr::anti_join(
      completion |>
        dplyr::select(
          election_date,
          municipality_key
        ),
      by = c(
        "election_date",
        "municipality_key"
      )
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality_raw
    )
  
  if (nrow(raw_not_in_manual) > 0) {
    print(
      raw_not_in_manual |>
        dplyr::select(
          election_date,
          municipality_raw,
          municipality_key
        )
    )
    stop(
      "Some rosatellum_2022_vda municipalities are missing from vda_2022_manual_completion.csv after normalized-key matching."
    )
  }
  
  municipality_manual_totals <- raw_totals |>
    dplyr::left_join(
      completion |>
        dplyr::transmute(
          election_date = .data$election_date,
          municipality_key = .data$municipality_key,
          registered_voters_manual = .data$registered_voters,
          voters_manual = .data$voters,
          blank_ballots_manual = .data$blank_ballots,
          null_ballots_manual = .data$null_ballots,
          raw_total_list_votes_manual = .data$raw_total_list_votes
        ),
      by = c(
        "election_date",
        "municipality_key"
      ),
      relationship = "one-to-one"
    )
  
  missing_manual_totals <- municipality_manual_totals |>
    dplyr::filter(
      is.na(.data$registered_voters_manual) |
        is.na(.data$voters_manual) |
        is.na(.data$blank_ballots_manual)
    )
  
  if (nrow(missing_manual_totals) > 0) {
    print(
      missing_manual_totals |>
        dplyr::select(
          election_date,
          municipality_raw,
          municipality_key,
          registered_voters_manual,
          voters_manual,
          blank_ballots_manual
        )
    )
    stop(
      "Municipality-level manual totals could not be attached for some rosatellum_2022_vda municipalities."
    )
  }
  
  mismatched_total_list_votes <- municipality_manual_totals |>
    dplyr::filter(
      !.data$any_missing_list_votes,
      !is.na(.data$raw_total_list_votes_manual),
      .data$observed_total_list_votes != .data$raw_total_list_votes_manual
    )
  
  if (nrow(mismatched_total_list_votes) > 0) {
    print(
      mismatched_total_list_votes |>
        dplyr::select(
          election_date,
          municipality_raw,
          observed_total_list_votes,
          raw_total_list_votes_manual
        ) |>
        dplyr::arrange(
          .data$election_date,
          .data$municipality_raw
        )
    )
    
    stop(
      "Mismatch between raw_total_list_votes in vda_2022_manual_completion.csv and summed list_votes in processed rosatellum_2022_vda data."
    )
  }
  
  mismatched_null_ballots <- municipality_manual_totals |>
    dplyr::mutate(
      derived_null_ballots_from_components =
        .data$voters_manual -
        .data$blank_ballots_manual -
        .data$raw_total_list_votes_manual
    ) |>
    dplyr::filter(
      !is.na(.data$null_ballots_manual),
      !is.na(.data$derived_null_ballots_from_components),
      .data$null_ballots_manual != .data$derived_null_ballots_from_components
    )
  
  if (nrow(mismatched_null_ballots) > 0) {
    message(
      "VdA 2022: manual null ballots differ from the residual in some rows; ",
      "null_ballots will be recomputed downstream."
    )
  }
  
  # Manual totals fill only missing raw accounting fields.
  out_completed <- out_keyed |>
    dplyr::left_join(
      municipality_manual_totals |>
        dplyr::select(
          election_date,
          municipality_key,
          registered_voters_manual,
          voters_manual,
          blank_ballots_manual
        ),
      by = c(
        "election_date",
        "municipality_key"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      registered_voters = dplyr::coalesce(
        .data$registered_voters,
        .data$registered_voters_manual
      ),
      voters = dplyr::coalesce(
        .data$voters,
        .data$voters_manual
      ),
      blank_ballots = dplyr::coalesce(
        .data$blank_ballots,
        .data$blank_ballots_manual
      )
    ) |>
    dplyr::select(
      -municipality_key,
      -registered_voters_manual,
      -voters_manual,
      -blank_ballots_manual
    )
  
  still_missing_after_completion <- out_completed |>
    dplyr::filter(
      is.na(.data$registered_voters) |
        is.na(.data$voters) |
        is.na(.data$blank_ballots)
    ) |>
    dplyr::distinct(
      election_date,
      municipality
    ) |>
    dplyr::arrange(
      .data$election_date,
      .data$municipality
    )
  
  if (nrow(still_missing_after_completion) > 0) {
    print(still_missing_after_completion)
    stop(
      "Manual completion for rosatellum_2022_vda did not fill all required municipality totals after municipality-level completion."
    )
  }
  
  out_completed
}


fix_1948_trento_bolzano_blank_ballots <- function(out, dataset_name, schema_id) {
  
  if (schema_id != "first_republic") {
    return(out)
  }
  
  if (!"election_date" %in% names(out)) {
    return(out)
  }
  
  if (!"province_raw" %in% names(out)) {
    stop("fix_1948_trento_bolzano_blank_ballots() requires province_raw.")
  }
  
  target_rows <- out$election_date == "19480418" &
    out$province_raw %in% c("TRENTO", "BOLZANO") &
    is.na(out$blank_ballots)
  
  n_filled <- sum(target_rows, na.rm = TRUE)
  
  if (n_filled > 0) {
    message(
      "Setting missing SCHEDE_BIANCHE to 0 for 1948 Trento/Bolzano rows (",
      n_filled,
      " rows)."
    )
    
    out$blank_ballots[target_rows] <- 0
  }
  
  out
}


process_camera_dataset <- function(df, dataset_name, schema_id) {
  
  registry <- get_camera_schema_registry()
  schema <- registry[[schema_id]]
  
  if (is.null(schema)) {
    stop("Unknown schema_id: ", schema_id)
  }
  
  if (!is.null(schema$aliases) && length(schema$aliases) > 0) {
    for (target_name in names(schema$aliases)) {
      alias_candidates <- schema$aliases[[target_name]]
      found_alias <- alias_candidates[
        alias_candidates %in% names(df)
      ][1]
      
      if (!is.na(found_alias) && !target_name %in% names(df)) {
        names(df)[names(df) == found_alias] <- target_name
      }
    }
  }
  
  missing_vars <- setdiff(
    schema$required_raw,
    names(df)
  )
  
  if (length(missing_vars) > 0) {
    stop(
      "Dataset ",
      dataset_name,
      " is missing required raw variables: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  fallback_raw_vars <- if (!is.null(schema$fallback_raw)) {
    unname(unlist(schema$fallback_raw))
  } else {
    character()
  }
  
  rename_raw_vars <- unname(schema$rename_map)
  
  selected_raw <- unique(c(
    schema$required_raw,
    rename_raw_vars,
    fallback_raw_vars
  ))
  
  out <- df[
    ,
    names(df) %in% selected_raw,
    drop = FALSE
  ]
  
  for (std_name in names(schema$rename_map)) {
    raw_name <- schema$rename_map[[std_name]]
    
    if (raw_name %in% names(out)) {
      names(out)[names(out) == raw_name] <- std_name
    } else {
      out[[std_name]] <- NA
    }
  }
  
  if (!is.null(schema$fallback_raw) && length(schema$fallback_raw) > 0) {
    for (std_name in names(schema$fallback_raw)) {
      raw_fallback_name <- schema$fallback_raw[[std_name]]
      
      if (raw_fallback_name %in% names(out)) {
        
        fallback_values <- out[[raw_fallback_name]]
        
        fallback_allowed <- rep(TRUE, nrow(out))
        
        # In 2018 Valle d'Aosta list-equivalent votes are carried by candidate votes.
        if (
          schema_id == "rosatellum_2018" &&
          std_name == "list_votes" &&
          raw_fallback_name == "VOTI_CANDIDATO"
        ) {
          fallback_allowed <- out$district_raw == "AOSTA"
        }
        
        fallback_allowed[is.na(fallback_allowed)] <- FALSE
        
        fallback_used <- is.na(out[[std_name]]) &
          !is.na(fallback_values) &
          fallback_allowed
        
        out[[std_name]][fallback_used] <- fallback_values[fallback_used]
        
        if (any(fallback_used, na.rm = TRUE)) {
          message(
            "Fallback used: ",
            dataset_name,
            " / ",
            raw_fallback_name,
            " -> ",
            std_name,
            " (",
            sum(fallback_used, na.rm = TRUE),
            " rows)"
          )
        }
        
        out[[raw_fallback_name]] <- NULL
      }
    }
  }
  
  support_columns <- c(
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province",
    "municipality",
    "registered_voters",
    "voters",
    "blank_ballots",
    "null_ballots",
    "list_name",
    "list_votes",
    "election_date_raw"
  )
  
  for (col_name in support_columns) {
    if (!col_name %in% names(out)) {
      out[[col_name]] <- NA
    }
  }
  
  text_columns <- c(
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw",
    "province",
    "municipality",
    "list_name"
  )
  
  for (col_name in text_columns) {
    out[[col_name]] <- stringr::str_to_upper(
      stringr::str_squish(as.character(out[[col_name]]))
    )
    out[[col_name]][out[[col_name]] == "NA"] <- NA_character_
  }
  
  out$registered_voters <- parse_numeric_strict(
    out$registered_voters,
    dataset_name,
    "registered_voters"
  )
  
  out$voters <- parse_numeric_strict(
    out$voters,
    dataset_name,
    "voters"
  )
  
  out$blank_ballots <- parse_numeric_strict(
    out$blank_ballots,
    dataset_name,
    "blank_ballots"
  )
  
  out$list_votes <- parse_numeric_strict(
    out$list_votes,
    dataset_name,
    "list_votes"
  )
  
  election_date <- stringr::str_extract(
    dataset_name,
    "\\d{8}"
  )
  
  if (
    is.na(election_date) &&
    "election_date_raw" %in% names(out) &&
    !all(is.na(out$election_date_raw))
  ) {
    election_date <- unique(
      stats::na.omit(as.character(out$election_date_raw))
    )[1]
  }
  
  if (is.na(election_date)) {
    catalog <- get_camera_file_catalog()
    
    catalog_match <- catalog |>
      dplyr::filter(
        .data$file_stub == tools::file_path_sans_ext(dataset_name)
      )
    
    if (nrow(catalog_match) == 1) {
      election_date <- catalog_match$election_date[[1]]
    }
  }
  
  if (is.na(election_date)) {
    stop(
      "Could not extract election date from dataset name or raw fields: ",
      dataset_name
    )
  }
  
  out$election_date <- election_date
  out$year <- as.integer(substr(election_date, 1, 4))
  out$source_dataset <- dataset_name
  
  out <- complete_vda_2022_manual_fields(
    out = out,
    dataset_name = dataset_name,
    schema_id = schema_id
  )
  
  out <- derive_raw_province_fields(
    df = out,
    province_strategy = schema$province_strategy
  )
  
  out <- fix_1948_trento_bolzano_blank_ballots(
    out = out,
    dataset_name = dataset_name,
    schema_id = schema_id
  )
  
  out$abstentions <- out$registered_voters - out$voters
  
  out <- derive_null_ballots(out)
  
  out |>
    dplyr::select(
      election_date,
      year,
      source_dataset,
      district_raw,
      plurinominal_college_raw,
      uninominal_college_raw,
      province_raw,
      province_raw_source,
      municipality,
      registered_voters,
      voters,
      abstentions,
      blank_ballots,
      null_ballots,
      list_name,
      list_votes
    )
}