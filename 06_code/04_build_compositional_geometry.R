# 04_build_compositional_geometry.R


source(here::here("06_code", "00_setup.R"))

# PATHS

input_path <- file.path(path_data_processed, "camera_all_harmonized.rds")
output_dir <- path_data_compositional
table_dir <- path_outputs_tables_compositional

manual_territorial_path <- file.path(
  path_data_lookup,
  "manual_territorial_component_lookup.csv"
)

composition_objects_path <- file.path(
  output_dir,
  "stage04_composition_distance_objects.rds"
)

# SETTINGS


max_retained_lists <- 12L
min_retained_vote_share <- 0.010

excluded_provinces <- c("VALLE D'AOSTA/VALLÉE D'AOSTE")

zero_pseudocount <- 10
zero_pseudocount_grid <- c(1, 5, 10, 25, 50)
zero_share_floor_grid <- c(1e-6, 5e-6, 1e-5, 5e-5)

alpha_selected <- 0.150
lambda_unweighted_selected <- 0.150
lambda_weighted_selected <- 0.175

power_grid <- c(0.001, 0.005, 0.010, seq(0.025, 1.000, by = 0.025))
alpha_grid <- power_grid
lambda_grid <- power_grid

power_tag <- function(x) {
  sprintf("%04d", as.integer(round(1000 * x)))
}

alpha_geometry <- paste0("alpha_", power_tag(alpha_selected))

greenacre_unweighted_geometry <- paste0(
  "greenacre_unweighted_power_",
  power_tag(lambda_unweighted_selected)
)

greenacre_weighted_geometry <- paste0(
  "greenacre_weighted_power_",
  power_tag(lambda_weighted_selected)
)

geometry_names <- c(
  "aitchison",
  "greenacre_weighted_lra",
  alpha_geometry,
  greenacre_unweighted_geometry,
  greenacre_weighted_geometry,
  "euclidean"
)



# SMALL UTILITIES 

cor_or_na <- function(x, y, method = "pearson") {
  if (stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
  stats::cor(x, y, method = method)
}

#funzione per creare un identificatore composto da più colonne. Serve dopo per evitare duplicazioni quando le variabili elettorali sono ripetute a livello lista.
make_compound_key <- function(df, cols) {
  vals <- lapply(cols, function(cl) { # Per ogni colonna in cols, applica una funzione. lapply() restituisce una lista.
    x <- as.character(df[[cl]])
    x[is.na(x)] <- "<NA>"
    paste0(cl, "=", x) # Crea stringhe tipo municipality=Maddaloni. Includere il nome della colonna è più robusto di incollare solo i valori, perché evita ambiguità fra colonne diverse.
  })
  do.call(paste, c(vals, sep = "||")) #Incolla tutte le componenti della chiave con separatore ||.
} # Output: vettore di stringhe lungo quanto il numero di righe di df. Ogni stringa identifica una riga rispetto all’insieme di colonne selezionate

close_rows <- function(X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double" # forza tipo numerico
  rs <- rowSums(X)
  if (any(rs <= 0)) {
    stop("Cannot close rows with non-positive row sums.", call. = FALSE)
  }
  sweep(X, 1, rs, "/") # lungo la dim 1 (righe) dividi per rs
}

# regolarizzazione fixed pseudo-count
regularize_zeros <- function(X, epsilon = zero_pseudocount) {
  X <- as.matrix(X)
  X[X == 0] <- epsilon
  X
}

#alternativa: stesso peso composizionale dopo chiusura
regularize_zeros_share_floor <- function(X, eta) {
  X <- as.matrix(X)
  if (!is.finite(eta) || eta <= 0) {
    stop("eta must be positive.", call. = FALSE)
  }
  
  Z <- X
  row_totals <- rowSums(X)
  zero_counts <- rowSums(X == 0)
  bad_rows <- (eta * zero_counts) >= 1
  
  if (any(bad_rows)) {
    stop(
      "Invalid share-floor zero replacement: eta * zero_count >= 1.",
      call. = FALSE
    )
  }
  
  eps_row <- eta * row_totals / (1 - eta * zero_counts)
  for (i in seq_len(nrow(Z))) {
    if (zero_counts[[i]] > 0) {
      Z[i, Z[i, ] == 0] <- eps_row[[i]]
    }
  }
  
  Z
}

# BENCHMARK LOG-RATIO REGOLARIZZATI
clr_from_regularized_amounts <- function(X_regularized) {
  P <- close_rows(X_regularized)
  logP <- log(P)
  Z <- sweep(logP, 1, rowMeans(logP), "-")
  dimnames(Z) <- dimnames(X_regularized)
  Z
}

greenacre_weighted_lra_from_regularized_amounts <- function(X_regularized, masses) {
  
  P <- close_rows(X_regularized)
  I <- nrow(P)
  
  logP <- log(P)
  centre <- as.vector(logP %*% masses)
  Z <- sweep(logP, 1, centre, "-")
  Z <- sweep(Z, 2, sqrt(masses), "*") / sqrt(I)
  
  dimnames(Z) <- dimnames(X_regularized)
  Z
}

# funzioni wrapper che restituiscono matrici coordinate
clr_from_amounts <- function(X, epsilon = zero_pseudocount) {
  clr_from_regularized_amounts(regularize_zeros(X, epsilon))
}

greenacre_weighted_lra_from_amounts <- function(X, epsilon = zero_pseudocount, masses = NULL) {
  if (is.null(masses)) masses <- colMeans(close_rows(X)) # calcolo masse prima di regolarizzare
  greenacre_weighted_lra_from_regularized_amounts(
    regularize_zeros(X, epsilon),
    masses = masses
  )
}

# GEOMETRIE POSITIVE-POWER

# trasformazione Tsagris--Preston--Wood
alpha_transform_from_amounts <- function(X, alpha) {
  if (!is.finite(alpha) || alpha <= 0) {
    stop("alpha must be positive.", call. = FALSE)
  }
  
  P <- close_rows(X)
  Pa <- close_rows(P^alpha)
  D <- ncol(Pa)
  Z <- (D * Pa - 1) / alpha
  dimnames(Z) <- dimnames(X)
  Z
}

greenacre_unweighted_from_amounts <- function(X, lambda) { # applica una trasformazione power alle quote chiuse, poi costruisce coordinate di tipo chi-square.
  if (!is.finite(lambda) || lambda <= 0) {
    stop("lambda must be positive.", call. = FALSE)
  }
  
  P <- close_rows(X)
  D <- ncol(P)
  Y <- close_rows(P^lambda)
  m <- colMeans(Y)
  
  if (any(m <= 0)) {
    stop("Some powered column masses are zero.", call. = FALSE)
  }
  
  Y_centered <- sweep(Y, 2, m, "-")
  Z <- sweep(Y_centered, 2, sqrt(m), "/")
  Z <- (sqrt(D) / lambda) * Z
  
  dimnames(Z) <- dimnames(X)
  Z
}

greenacre_weighted_from_amounts <- function(X, lambda, masses = NULL) {
  
  if (!is.finite(lambda) || lambda <= 0) {
    stop("lambda must be positive.", call. = FALSE)
  }
  
  P <- close_rows(X)
  I <- nrow(P)
  
  if (is.null(masses)) masses <- colMeans(P)
  
  Q <- sweep(P, 2, masses, "/")
  
  Q_lambda <- Q^lambda
  row_centre <- as.vector(Q_lambda %*% masses)
  Phi <- sweep(Q_lambda, 1, row_centre, "-") / lambda
  
  Z <- sweep(Phi, 2, sqrt(masses), "*")
  Z <- Z / sqrt(I)
  
  dimnames(Z) <- dimnames(X)
  Z
}

upper_vec <- function(D) { # estrae triangolo superiore matrice distanze
  M <- as.matrix(D)
  M[upper.tri(M)]
}


zero_hamming <- function(X) {
  B <- X == 0
  pairs <- utils::combn(seq_len(nrow(B)), 2)
  rowSums(B[pairs[1, ], , drop = FALSE] != B[pairs[2, ], , drop = FALSE])
} # output: numero componenti che sono zero in una provincia ma non nell'altra, per ogni coppia di province


pairwise_component_contributions <- function(Z) {
  pairs <- utils::combn(seq_len(nrow(Z)), 2)
  diffs <- Z[pairs[1, ], , drop = FALSE] - Z[pairs[2, ], , drop = FALSE]
  list(pairs = pairs, squared_diffs = diffs^2)
} # calcolacontributo quadratico elementare componente j alla distanza tra province p e q, per ogni coppia

# aggrega contributi su tutte le coppie per calcolare s_j
component_contribution_shares_from_Z <- function(Z) {
  pairwise <- pairwise_component_contributions(Z)
  squared_diffs <- pairwise$squared_diffs
  total <- rowSums(squared_diffs)
  
  denom <- sum(total, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) {
    return(stats::setNames(rep(NA_real_, ncol(Z)), colnames(Z)))
  }
  
  shares <- colSums(squared_diffs, na.rm = TRUE) / denom
  stats::setNames(as.numeric(shares), colnames(Z))
}

# Load harmonized electoral data

message("Stage 04: loading harmonized electoral data.")
raw <- load_tabular_file(input_path)

required_cols <- c(
  "election_date", "year", "municipality", "province_harmonized",
  "region_harmonized", "list_name", "list_votes", "registered_voters",
  "voters", "abstentions", "blank_ballots", "null_ballots"
)

missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

# Aggregate municipality-level records to harmonized provinces.

message("Stage 04: aggregating harmonized records to provinces.")

reporting_key_cols <- intersect(
  c(
    "election_date",
    "municipality",
    "municipality_target",
    "province_actual",
    "province_harmonized",
    "region_harmonized",
    "province_raw",
    "district_raw",
    "plurinominal_college_raw",
    "uninominal_college_raw"
  ),
  names(raw)
)

raw$.reporting_unit_id_stage04 <- make_compound_key(raw, reporting_key_cols)

base <- raw |>
  dplyr::transmute(
    election_date = as.character(.data$election_date),
    year = as.integer(.data$year),
    reporting_unit_id = .data$.reporting_unit_id_stage04,
    municipality = as.character(.data$municipality),
    territory = as.character(.data$province_harmonized),
    region = as.character(.data$region_harmonized),
    component = as.character(.data$list_name),
    list_votes = as.numeric(.data$list_votes),
    registered_voters = as.numeric(.data$registered_voters),
    voters = as.numeric(.data$voters),
    abstentions = as.numeric(.data$abstentions),
    blank_ballots = as.numeric(.data$blank_ballots),
    null_ballots = as.numeric(.data$null_ballots)
  ) |>
  dplyr::filter(!is.na(.data$territory), nzchar(.data$territory))

base_main <- base |>
  dplyr::filter(!(.data$territory %in% excluded_provinces))

if (nrow(base_main) == nrow(base)) {
  warning(
    "No rows were removed by the rule ",
    "(territory != '", paste(excluded_provinces, collapse = " | "), "'). Check province_harmonized labels.",
    call. = FALSE
  )
}

# aggregazione parte non partitica elettorato dopo aver eliminato duplicazione list-level
turnout <- base_main |>
  dplyr::distinct(.data$reporting_unit_id, .keep_all = TRUE) |>
  dplyr::group_by(.data$election_date, .data$year, .data$territory, .data$region) |>
  dplyr::summarise( # somma per ottenere tot provinciale
    registered_voters = sum(.data$registered_voters, na.rm = TRUE),
    voters = sum(.data$voters, na.rm = TRUE),
    abstentions = sum(.data$abstentions, na.rm = TRUE),
    blank_ballots = sum(.data$blank_ballots, na.rm = TRUE),
    null_ballots = sum(.data$null_ballots, na.rm = TRUE),
    .groups = "drop"
  )

#aggrehazione parte apartitica dataset: no distinct(), since list_votes è effettivamente quanittà list-specific
votes <- base_main |>
  dplyr::group_by(
    .data$election_date, .data$year, .data$territory, .data$region, .data$component
  ) |>
  dplyr::summarise(
    list_votes = sum(.data$list_votes, na.rm = TRUE),
    .groups = "drop"
  ) # tabella con una riga per ogni combinazione (elezione,provincia,regione,lista). ancora formato long, con ogni lista riga separata. matrice provincexcomponenti costruita dopo applicazione regole supporto

agg <- votes |>
  dplyr::left_join(
    turnout,
    by = c("election_date", "year", "territory", "region"),
    relationship = "many-to-one" # ogni riga lista-provincia-elezione in votes riceve le corrispondenti variabili di turnout della stessa provincia-elezione.
  ) |>
  dplyr::arrange(.data$election_date, .data$territory, .data$component)

elections <- sort(unique(agg$election_date))


# Election-specific support.

build_support_table <- function(ed) {
  d <- agg |>
    dplyr::filter(.data$election_date == .env$ed) # una elezione alla volta
  
  n_territories <- dplyr::n_distinct(d$territory)
  
  territory_component <- d |> # tabella lista-provincia per elezione corrente
    dplyr::group_by(.data$component, .data$territory, .data$region) |>
    dplyr::summarise(votes = sum(.data$list_votes, na.rm = TRUE), .groups = "drop")
  
  top_region <- territory_component |>
    dplyr::group_by(.data$component, .data$region) |>
    dplyr::summarise(region_votes = sum(.data$votes, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(.data$component) |>
    dplyr::mutate(region_share = .data$region_votes / sum(.data$region_votes, na.rm = TRUE)) |>
    dplyr::slice_max(order_by = .data$region_share, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      component = .data$component,
      top_region = .data$region,
      top_region_share = .data$region_share
    ) # diagnostiche concentrazione territoriale per eppendice eventualmente, non uso per classificazione territoriale (che è manuale)
  
  territory_component |> 
    dplyr::group_by(.data$component) |> # livello lista-elezione
    dplyr::summarise(
      national_votes = sum(.data$votes, na.rm = TRUE),
      n_positive_territories = sum(.data$votes > 0),
      max_territory_votes = max(.data$votes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_zero_territories = n_territories - .data$n_positive_territories,
      election_date = ed,
      year = dplyr::first(d$year),
      n_territories = n_territories,
      national_share = .data$national_votes / sum(.data$national_votes, na.rm = TRUE),
      national_rank = dplyr::min_rank(dplyr::desc(.data$national_votes)),
      coverage = .data$n_positive_territories / .data$n_territories,
      zero_share = .data$n_zero_territories / .data$n_territories,
      max_territory_share = dplyr::if_else(
        .data$national_votes > 0,
        .data$max_territory_votes / .data$national_votes,
        NA_real_
      ),
      retained = .data$national_rank <= max_retained_lists & .data$national_share >= min_retained_vote_share,
      drop_reason = dplyr::case_when(
        .data$retained ~ "retained_by_rule",
        .data$national_rank > max_retained_lists &
          .data$national_share < min_retained_vote_share ~ "rank_gt_top_n_and_below_min_share",
        .data$national_rank > max_retained_lists ~ "rank_gt_top_n",
        .data$national_share < min_retained_vote_share ~ "below_min_share",
        TRUE ~ "not_retained"
      )
    ) |>
    dplyr::left_join(top_region, by = "component", relationship = "one-to-one") |>
    dplyr::arrange(.data$election_date, .data$national_rank, .data$component)
}

message("Stage 04: building election-specific support.")
support_table <- purrr::map_dfr(elections, build_support_table)


# Manual territorial component classification.

read_manual_territorial_components <- function(valid_keys) { # input: tabella con tutte le coppie (election_date, component)
  
  if (!file.exists(manual_territorial_path)) {
    stop(
      "Manual territorial component lookup not found.\n",
      "Expected file: ", manual_territorial_path, "\n",
      "The lookup must contain the election-component pairs to be assigned ",
      "to TERRITORIAL_LISTS before the retain/other rule is applied.",
      call. = FALSE
    )
  }
  
  manual_raw <- load_tabular_file(manual_territorial_path)
  
  required <- c("election_date", "component")
  missing_required <- setdiff(required, names(manual_raw))
  if (length(missing_required) > 0) {
    stop(
      "Manual territorial lookup is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!"notes" %in% names(manual_raw)) { # facoltativa
    manual_raw$notes <- NA_character_
  }
  
  manual <- manual_raw |>
    dplyr::mutate(
      election_date = as.character(.data$election_date),
      component = as.character(.data$component),
      notes = as.character(.data$notes)
    ) |>
    dplyr::filter(
      !is.na(.data$component),
      nzchar(.data$component)
    ) |>
    dplyr::mutate(
      manual_territorial_component = TRUE
    ) |>
    dplyr::select(
      "election_date", "component", "manual_territorial_component", "notes"
    )
  
  duplicates <- manual |>
    dplyr::count(.data$election_date, .data$component) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(duplicates) > 0) {
    print(duplicates, n = 100)
    stop(
      "Manual territorial lookup contains duplicated election-component pairs.",
      call. = FALSE
    )
  }
  
  invalid <- manual |>
    dplyr::anti_join(valid_keys, by = c("election_date", "component"))
  
  if (nrow(invalid) > 0) {
    print(invalid, n = 100)
    stop(
      "Manual territorial lookup contains election-component pairs not found ",
      "in the current support table.",
      call. = FALSE
    )
  }
  
  manual
}

message("Stage 04: loading manual territorial component lookup.")

manual_valid_keys <- support_table |> # universo coppie elezione-component valide
  dplyr::transmute(
    election_date = as.character(.data$election_date),
    component = as.character(.data$component)
  ) |>
  dplyr::distinct()

manual_territorial <- read_manual_territorial_components(manual_valid_keys) # restituisce una tibble con: election_date, component, manual_territorial_component, notes

support_table <- support_table |> # attacco a ogni lista-elezione l’eventuale informazione manuale territoriale
  dplyr::mutate(
    election_date = as.character(.data$election_date),
    component = as.character(.data$component)
  ) |>
  dplyr::left_join(
    manual_territorial,
    by = c("election_date", "component"),
    relationship = "many-to-one" # molte righe dello support_table possono essere confrontate con il lookup, ma per ogni coppia (election_date, component) il lookup deve avere al massimo una riga
  ) |>
  dplyr::mutate( # trasfroma NA in FALSE. ho variabile booleana completa
    manual_territorial_component = dplyr::coalesce(
      .data$manual_territorial_component,
      FALSE
    ),
    final_component_class = dplyr::case_when(
      .data$manual_territorial_component ~ "TERRITORIAL_LISTS",
      .data$retained ~ "RETAIN",
      TRUE ~ "OTHER_LISTS"
    )
  )

classified_support <- support_table |> # con quale nome lista entrerà nella composzione finale
  dplyr::mutate(
    final_component_in_spec = dplyr::if_else(
      .data$final_component_class == "RETAIN",
      .data$component,
      .data$final_component_class
    )
  )


component_final_classification <- classified_support |>
  dplyr::select(
    "election_date", "year", "component", "retained",
    "manual_territorial_component", "notes",
    "final_component_class", "final_component_in_spec",
    "national_votes", "national_share", "national_rank",
    "coverage", "zero_share", "top_region", "top_region_share",
    "drop_reason"
  ) |>
  dplyr::arrange(.data$election_date, .data$national_rank, .data$component)


territorial_lookup_used <- manual_territorial |>
  dplyr::left_join(
    component_final_classification |>
      dplyr::select(
        "election_date", "year", "component", "national_share",
        "national_rank", "coverage", "zero_share",
        "top_region", "top_region_share", "retained"
      ),
    by = c("election_date", "component"),
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(.data$election_date, .data$national_rank, .data$component)

support_summary <- classified_support |>
  dplyr::group_by(.data$election_date, .data$year) |>
  dplyr::summarise(
    n_components_raw = dplyr::n(),
    n_retained_by_rule = sum(.data$retained, na.rm = TRUE),
    n_territorial_components_in_lookup = sum(.data$manual_territorial_component, na.rm = TRUE),
    n_territorial_overrides_retained_by_rule = sum( # liste che avrebbero passator egola automatica ma sono territoriali. lookup territoriale sovrascrive queste liste che altrimenti sarebbero autonome
      .data$manual_territorial_component & .data$retained,
      na.rm = TRUE
    ),
    n_autonomous_components_in_spec = sum(.data$final_component_class == "RETAIN", na.rm = TRUE),
    n_final_components_from_lists = dplyr::n_distinct(.data$final_component_in_spec), # liste retained + OTHER se presente + TERRITORIAL se presente. D_t è = a questo + 1(NON_VALID_OR_NON_VOTING)
    retained_vote_share_by_rule = sum(.data$national_share[.data$retained], na.rm = TRUE), # QUOTE NAZIONALI LISTE CHE PASSAN OREGOLA AUTOMATICA PRIMA DELL'OVERRIDE TERRITORIALE
    autonomous_vote_share_in_spec = sum(
      .data$national_share[.data$final_component_class == "RETAIN"], #dopo override
      na.rm = TRUE
    ),
    other_vote_share_in_spec = sum(
      .data$national_share[.data$final_component_class == "OTHER_LISTS"],
      na.rm = TRUE
    ),
    territorial_vote_share_in_spec = sum(
      .data$national_share[.data$final_component_class == "TERRITORIAL_LISTS"],
      na.rm = TRUE
    ),
    max_dropped_share_by_rule = max(.data$national_share[!.data$retained], na.rm = TRUE), # lista più grande tra quelle non trattenute dalla regola automatica
    max_other_share_in_spec = max( # lista più grande finita in OTHER_LISTS
      .data$national_share[.data$final_component_class == "OTHER_LISTS"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dropped_vote_share_by_rule = 1 - .data$retained_vote_share_by_rule
  )

support_dropped <- component_final_classification |> # liste finite in OTHER_LISTS ma con quota nazionale almeno 0.5%.
  dplyr::filter(
    .data$final_component_class == "OTHER_LISTS",
    .data$national_share >= 0.005
  ) |>
  dplyr::arrange(.data$election_date, dplyr::desc(.data$national_share))

support_retained <- component_final_classification |>
  dplyr::filter(.data$final_component_class == "RETAIN") |>
  dplyr::arrange(.data$election_date, .data$national_rank)


# Build province-election amount matrices and distance matrices.


apply_support <- function(d, st) { # d: sottoinsieme agg per una elezione t (contiene (provincia, lista, voti)); st: subset support_table t (contiene classe finale per ogni lista j)
  lookup <- st |>
    dplyr::select("component", "final_component_class") |>
    dplyr::distinct()
  
  d_joined <- d |> # (p,j,t) --> (p,j,t, classe)
    dplyr::left_join(lookup, by = "component", relationship = "many-to-one")
  
  unmatched <- d_joined |>
    dplyr::filter(is.na(.data$final_component_class)) |>
    dplyr::distinct(.data$component) |>
    dplyr::arrange(.data$component)
  
  if (nrow(unmatched) > 0) {
    print(unmatched, n = 100)
    stop(
      "Some components in the election data are missing from the support table.",
      call. = FALSE
    )
  }
  
  d_joined |>
    dplyr::mutate(
      component_final = dplyr::if_else(
        .data$final_component_class == "RETAIN",
        .data$component, # lista retain resta autonoma
        .data$final_component_class # se no entra enl residuale corripsondnete
      )
    )
}


wide_by_final_component <- function(d) { # dati long --> wide. 
  d |> # Dopo apply_support(), d contiene righe lista-provincia con una colonna component_final.
    dplyr::group_by(.data$territory, component = .data$component_final) |>
    dplyr::summarise(amount = sum(.data$list_votes, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "component", values_from = "amount", values_fill = 0) # se una componente non appare in una provincia, il suo valore è zero.
}

matrix_from_amounts <- function(df, id_cols) { 
  comps <- sort(setdiff(names(df), id_cols)) # id_cols contiene colonnemetadata: tutto quello che non è metadata viene trattattao ocme componente della matrice
  df <- df |>
    dplyr::arrange(.data$territory) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(comps), ~ tidyr::replace_na(as.numeric(.x), 0))) # per ogni colonna componente, forz tipo numeric o e sosctiuisce NA con zero
  
  X <- as.matrix(df[, comps, drop = FALSE])
  storage.mode(X) <- "double"
  rownames(X) <- df$territory
  
  X <- X[, colSums(X) > 0, drop = FALSE]
  X
}

make_amount_matrix <- function(ed) { # costruisce matrice X_t
  d <- agg |>
    dplyr::filter(.data$election_date == .env$ed)
  
  st <- support_table |>
    dplyr::filter(.data$election_date == .env$ed)
  
  d <- apply_support(d, st)
  
  meta_cols <- c( # variabili a livello provincia-elezione
    "election_date", "year", "territory", "region",
    "registered_voters", "voters", "abstentions", "blank_ballots", "null_ballots"
  )
  
  meta <- d |> # tabella con una riga per provincia
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::distinct() |> # in d registered_voters, abstentions, blank_ballots, ... ripetute su ogni lista della stessa provincia
    dplyr::arrange(.data$territory)
  
  out <- meta |> # tabella contenente metadata provinciali e voti aggergati per componente finale
    dplyr::left_join(wide_by_final_component(d), by = "territory") 
  
  
  for (nm in c("OTHER_LISTS", "TERRITORIAL_LISTS")) {
    if (!nm %in% names(out)) out[[nm]] <- 0
    out[[nm]] <- tidyr::replace_na(as.numeric(out[[nm]]), 0)
  }
  
  out <- out |> # composizione sull'INTERO ELETTORATO
    dplyr::mutate(
      NON_VALID_OR_NON_VOTING = .data$abstentions + .data$blank_ballots + .data$null_ballots
    )
  
  list(
    X = matrix_from_amounts(out, meta_cols),
    meta = meta
  )
}


# Build distance matrices.

distance_set <- function( # costruisce tutte geometrie a partire da matrice amounts X_t
  X,
  epsilon = zero_pseudocount,
  alpha = alpha_selected,
  lambda_unweighted = lambda_unweighted_selected,
  lambda_weighted = lambda_weighted_selected
) {
  P <- close_rows(X)
  masses <- colMeans(P) # masse colonna (masse greenacre fisse elezione. c_j: peso medio composizionale componente j trale province)
  
  Z_clr <- clr_from_amounts(X, epsilon = epsilon) #aitchison regolarizzata
  
  Z_greenacre_weighted_lra <- greenacre_weighted_lra_from_amounts( # versione log-ratio pesata con masse c_j
    X,
    epsilon = epsilon,
    masses = masses
  )
  
  Z_alpha <- alpha_transform_from_amounts(X, alpha = alpha) # Tsagris--Preston--Wood.
  
  Z_greenacre_unweighted_power <- greenacre_unweighted_from_amounts(
    X,
    lambda = lambda_unweighted
  )
  
  Z_greenacre_weighted_power <- greenacre_weighted_from_amounts(
    X,
    lambda = lambda_weighted,
    masses = masses
  )
  
  list(
    clr_matrix = Z_clr,
    greenacre_weighted_lra_matrix = Z_greenacre_weighted_lra,
    alpha_matrix = Z_alpha,
    greenacre_unweighted_power_matrix = Z_greenacre_unweighted_power,
    greenacre_weighted_power_matrix = Z_greenacre_weighted_power,
    euclidean_matrix = P, 
    component_masses = tibble::tibble(
      component = names(masses),
      mass = as.numeric(masses)
    ),
    distances = stats::setNames(
      list(
        stats::dist(Z_clr), #stats::dist() calcola distanze euclidee tra righe della matrice coordinata.
        stats::dist(Z_greenacre_weighted_lra),
        stats::dist(Z_alpha),
        stats::dist(Z_greenacre_unweighted_power),
        stats::dist(Z_greenacre_weighted_power),
        stats::dist(P)
      ),
      geometry_names
    )
  )
}

build_election_object <- function(ed) {
  m <- make_amount_matrix(ed)
  X <- m$X # matrice province x componenti (m$meta: metadata provinciali)
  D <- distance_set(
    X,
    epsilon = zero_pseudocount,
    alpha = alpha_selected,
    lambda_unweighted = lambda_unweighted_selected,
    lambda_weighted = lambda_weighted_selected
  )
  
  list( # identiifcazione elezione, metadata territoriali, matrice quantità, masse greenacre, coordinate, distanze
    election_date = ed,
    year = dplyr::first(m$meta$year),
    node_metadata = m$meta,
    amount_matrix = X,
    component_masses = D$component_masses,
    clr_matrix = D$clr_matrix,
    greenacre_weighted_lra_matrix = D$greenacre_weighted_lra_matrix,
    alpha_matrix = D$alpha_matrix,
    greenacre_unweighted_power_matrix = D$greenacre_unweighted_power_matrix,
    greenacre_weighted_power_matrix = D$greenacre_weighted_power_matrix,
    euclidean_matrix = D$euclidean_matrix,
    distance_matrices = D$distances
  )
}

message("Stage 04: building compositional distance objects.")

objects <- purrr::map(elections, build_election_object) |>
  stats::setNames(elections)
# objects[["1948-04-18"]]
# objects[["1953-06-07"]]
# ...

geometry_coords <- function(obj, geometry) { # Serve dopo nei blocchi che calcolano contributi component-wise
  if (geometry == "aitchison") return(obj$clr_matrix)
  if (geometry == "greenacre_weighted_lra") return(obj$greenacre_weighted_lra_matrix)
  if (geometry == alpha_geometry) return(obj$alpha_matrix)
  if (geometry == greenacre_unweighted_geometry) return(obj$greenacre_unweighted_power_matrix)
  if (geometry == greenacre_weighted_geometry) return(obj$greenacre_weighted_power_matrix)
  if (geometry == "euclidean") return(obj$euclidean_matrix)
  stop("Unknown geometry: ", geometry, call. = FALSE)
}


# Core diagnostics.

distance_summary <- purrr::imap_dfr(objects, function(obj, ed) {
  purrr::imap_dfr(obj$distance_matrices, function(D, metric) {
    v <- upper_vec(D) # tutte le I_t chooses 2 distanze tra coppie distinte di province
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      metric = metric,
      n_nodes = nrow(obj$amount_matrix),
      n_components = ncol(obj$amount_matrix),
      min_distance = min(v, na.rm = TRUE),
      q25_distance = stats::quantile(v, 0.25, na.rm = TRUE, names = FALSE),
      median_distance = stats::median(v, na.rm = TRUE),
      q75_distance = stats::quantile(v, 0.75, na.rm = TRUE, names = FALSE),
      max_distance = max(v, na.rm = TRUE),
      mean_distance = mean(v, na.rm = TRUE),
      sd_distance = stats::sd(v, na.rm = TRUE)
    )
  })
}) # tabella con una riga epr ogni (elezione,geometria)


# Full pairwise geometry comparison.
message("Stage 04: full pairwise geometry comparison diagnostics.")

geometry_pair_names <- c(
  "euclidean",
  alpha_geometry,
  greenacre_unweighted_geometry,
  greenacre_weighted_geometry,
  "aitchison",
  "greenacre_weighted_lra"
)

geometry_pairs <- utils::combn(geometry_pair_names, 2, simplify = FALSE)
# crea tutte le coppie non ordinate di geometrie: 6 chooses 2 = 15 confrotni per elezione


full_pairwise_geometry_comparison <- purrr::imap_dfr(objects, function(obj, ed) {
  purrr::map_dfr(geometry_pairs, function(pair) {
    g1 <- pair[[1]]
    g2 <- pair[[2]]
    
    d1 <- upper_vec(obj$distance_matrices[[g1]])
    d2 <- upper_vec(obj$distance_matrices[[g2]])
    
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      geometry_a = g1,
      geometry_b = g2,
      geometry_a_order = match(g1, geometry_pair_names),
      geometry_b_order = match(g2, geometry_pair_names),
      n_nodes = nrow(obj$amount_matrix),
      n_components = ncol(obj$amount_matrix),
      distance_pearson_cor = cor_or_na(d1, d2, method = "pearson"),
      distance_spearman_cor = cor_or_na(d1, d2, method = "spearman")
    )
  })
})

# riasusnto confronto geometrie su tutte le elezioni
full_pairwise_geometry_comparison_summary <- full_pairwise_geometry_comparison |>
  dplyr::group_by(
    .data$geometry_a,
    .data$geometry_b,
    .data$geometry_a_order,
    .data$geometry_b_order
  ) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    pearson_min = min(.data$distance_pearson_cor, na.rm = TRUE),
    pearson_median = stats::median(.data$distance_pearson_cor, na.rm = TRUE),
    pearson_max = max(.data$distance_pearson_cor, na.rm = TRUE),
    spearman_min = min(.data$distance_spearman_cor, na.rm = TRUE),
    spearman_median = stats::median(.data$distance_spearman_cor, na.rm = TRUE),
    spearman_max = max(.data$distance_spearman_cor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(.data$spearman_median),
    dplyr::desc(.data$pearson_median)
  )


final_component_zeros <- purrr::imap_dfr(objects, function(obj, ed) {
  X <- obj$amount_matrix
  tibble::tibble(
    election_date = ed,
    year = as.integer(obj$year),
    component = colnames(X),
    total_amount = colSums(X),
    national_share = colSums(X) / sum(X),
    zero_count = colSums(X == 0),
    zero_share = colMeans(X == 0)
  )
})

final_component_zero_summary <- final_component_zeros |>
  dplyr::group_by(.data$election_date, .data$year) |>
  dplyr::summarise(
    n_final_components = dplyr::n(),
    total_zero_cells = sum(.data$zero_count, na.rm = TRUE),
    max_component_zero_share = max(.data$zero_share, na.rm = TRUE),
    n_components_zero_share_gt_25 = sum(.data$zero_share > 0.25, na.rm = TRUE), # compinenti che sono 0 in più del 25% delle province
    n_components_zero_share_gt_50 = sum(.data$zero_share > 0.50, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    support_summary |>
      dplyr::select(
        "election_date",
        "retained_vote_share_by_rule", "dropped_vote_share_by_rule",
        "max_dropped_share_by_rule", "autonomous_vote_share_in_spec",
        "territorial_vote_share_in_spec", "other_vote_share_in_spec"
      ),
    by = "election_date",
    relationship = "one-to-one"
  )

zero_metric_diagnostics <- purrr::imap_dfr(objects, function(obj, ed) {
  X <- obj$amount_matrix
  h0 <- zero_hamming(X)
  h0_share <- h0 / ncol(X)
  
  purrr::imap_dfr(obj$distance_matrices, function(D, metric) {
    d <- upper_vec(D)
    
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      metric = metric,
      n_components = ncol(X),
      zero_hamming_mean = mean(h0, na.rm = TRUE),
      zero_hamming_sd = stats::sd(h0, na.rm = TRUE),
      zero_hamming_share_mean = mean(h0_share, na.rm = TRUE),
      zero_hamming_share_sd = stats::sd(h0_share, na.rm = TRUE),
      distance_zero_hamming_pearson = cor_or_na(d, h0, method = "pearson"),
      distance_zero_hamming_spearman = cor_or_na(d, h0, method = "spearman"),
      distance_zero_hamming_share_pearson = cor_or_na(d, h0_share, method = "pearson"),
      distance_zero_hamming_share_spearman = cor_or_na(d, h0_share, method = "spearman")
    )
  })
})

zero_metric_diagnostics_summary <- zero_metric_diagnostics |>
  dplyr::group_by(.data$metric) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    zero_hamming_mean_median = stats::median(.data$zero_hamming_mean, na.rm = TRUE),
    zero_hamming_share_mean_median = stats::median(.data$zero_hamming_share_mean, na.rm = TRUE),
    zero_hamming_cor_spearman_median = stats::median(
      .data$distance_zero_hamming_spearman,
      na.rm = TRUE
    ),
    zero_hamming_share_cor_spearman_median = stats::median(
      .data$distance_zero_hamming_share_spearman,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Component contributions for all implemented distance coordinates.

component_contributions <- purrr::imap_dfr(objects, function(obj, ed) {
  purrr::map_dfr(geometry_names, function(geometry) {
    Z <- geometry_coords(obj, geometry)
    shares <- component_contribution_shares_from_Z(Z)
    
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      geometry = geometry,
      component = names(shares),
      share_of_total_pairwise_distance = as.numeric(shares)
    )
  }) |>
    dplyr::arrange(.data$geometry, dplyr::desc(.data$share_of_total_pairwise_distance))
})

component_contributions_top5 <- component_contributions |>
  dplyr::group_by(.data$election_date, .data$geometry) |>
  dplyr::slice_max(.data$share_of_total_pairwise_distance, n = 5, with_ties = FALSE) |>
  dplyr::ungroup()

component_contributions_summary <- component_contributions |>
  dplyr::group_by(.data$geometry, .data$component) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    mean_share_of_total_pairwise_distance = mean(.data$share_of_total_pairwise_distance, na.rm = TRUE),
    median_share_of_total_pairwise_distance = stats::median(.data$share_of_total_pairwise_distance, na.rm = TRUE),
    max_share_of_total_pairwise_distance = max(.data$share_of_total_pairwise_distance, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$geometry, dplyr::desc(.data$median_share_of_total_pairwise_distance))


# Rare-part and Greenacre mass diagnostics.

message("Stage 04: Greenacre mass and rare-part diagnostics.")

clr_variance_by_component <- purrr::imap_dfr(objects, function(obj, ed) {
  X <- obj$amount_matrix
  masses <- colMeans(close_rows(X))
  Z <- clr_from_amounts(X, epsilon = zero_pseudocount)
  
  tibble::tibble(
    election_date = ed,
    year = as.integer(obj$year),
    component = colnames(X),
    mean_proportion = as.numeric(masses[colnames(X)]),
    clr_variance = apply(Z, 2, stats::var),
    zero_share = colMeans(X == 0),
    national_share = colSums(X) / sum(X)
  ) |>
    dplyr::mutate(
      log_mean_proportion = log(.data$mean_proportion),
      log_clr_variance = log(.data$clr_variance)
    )
})

clr_variance_weighting_summary <- clr_variance_by_component |>
  dplyr::group_by(.data$election_date, .data$year) |>
  dplyr::summarise(
    n_components = dplyr::n(),
    spearman_logmean_logvar = stats::cor(.data$log_mean_proportion, .data$log_clr_variance, method = "spearman"),
    pearson_logmean_logvar = stats::cor(.data$log_mean_proportion, .data$log_clr_variance, method = "pearson"),
    max_clr_variance = max(.data$clr_variance, na.rm = TRUE),
    component_max_clr_variance = .data$component[which.max(.data$clr_variance)][1],
    median_clr_variance = stats::median(.data$clr_variance, na.rm = TRUE),
    median_zero_share = stats::median(.data$zero_share, na.rm = TRUE),
    .groups = "drop"
  )

component_masses <- purrr::imap_dfr(objects, function(obj, ed) {
  obj$component_masses |>
    dplyr::mutate(
      election_date = ed,
      year = as.integer(obj$year),
      .before = 1
    )
})

# Tsagris alpha alignment profile.

message("Stage 04: Tsagris alpha alignment diagnostics.")

alpha_profile_one <- function(obj, ed) {
  X <- obj$amount_matrix
  d_ait <- upper_vec(obj$distance_matrices$aitchison)
  
  purrr::map_dfr(alpha_grid, function(alpha) { # itero su tutti i valori alpha_grid
    Z_alpha <- alpha_transform_from_amounts(X, alpha)
    d_alpha <- upper_vec(stats::dist(Z_alpha))
    
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      alpha = alpha,
      n_nodes = nrow(X),
      n_components = ncol(X),
      benchmark = "aitchison",
      candidate = "alpha",
      distance_pearson_cor = cor_or_na(d_ait, d_alpha, method = "pearson"),
      distance_spearman_cor = cor_or_na(d_ait, d_alpha, method = "spearman")
    )
  })
}

alpha_alignment_profile <- purrr::imap_dfr(objects, alpha_profile_one)

alpha_best_by_election <- alpha_alignment_profile |>
  dplyr::group_by(.data$benchmark, .data$election_date) |>
  dplyr::arrange( # dentro ogni elezione ordino valori di lapha prima per spearman decrescente, poi per alpha crescente (se due alpha hanno stesso spearman, scelgo più piccolo)
    dplyr::desc(.data$distance_spearman_cor),
    .data$alpha,
    .by_group = TRUE
  ) |>
  dplyr::slice(1) |> #prendo prima riga ogni gruppo
  dplyr::ungroup() |>
  dplyr::rename(
    best_alpha = "alpha",
    best_distance_pearson_cor = "distance_pearson_cor",
    best_distance_spearman_cor = "distance_spearman_cor"
  )

alpha_best_summary <- alpha_alignment_profile |>
  dplyr::group_by(.data$benchmark, .data$alpha) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    spearman_median = stats::median(.data$distance_spearman_cor, na.rm = TRUE),
    pearson_median = stats::median(.data$distance_pearson_cor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    .data$benchmark,
    dplyr::desc(.data$spearman_median),
    dplyr::desc(.data$pearson_median), # pearson median come tie-breaker
    .data$alpha
  )

alpha_best_by_spec <- alpha_best_summary |>
  dplyr::group_by(.data$benchmark) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


# Greenacre power alignment profile.

message("Stage 04: Greenacre power alignment diagnostics.")

greenacre_power_alignment_profile_one <- function(obj, ed) {
  X <- obj$amount_matrix
  masses <- colMeans(close_rows(X))
  
  d_ait <- upper_vec(obj$distance_matrices$aitchison)
  d_wlra <- upper_vec(obj$distance_matrices$greenacre_weighted_lra)
  
  purrr::map_dfr(lambda_grid, function(lambda) {
    Z_gu <- greenacre_unweighted_from_amounts(X, lambda = lambda)
    Z_gw <- greenacre_weighted_from_amounts(X, lambda = lambda, masses = masses)
    
    d_gu <- upper_vec(stats::dist(Z_gu))
    d_gw <- upper_vec(stats::dist(Z_gw))
    
    tibble::tibble(
      election_date = ed,
      year = as.integer(obj$year),
      lambda = lambda,
      n_nodes = nrow(X),
      n_components = ncol(X),
      family = "greenacre_unweighted_power",
      benchmark = "aitchison",
      distance_pearson_cor = cor_or_na(d_ait, d_gu, method = "pearson"),
      distance_spearman_cor = cor_or_na(d_ait, d_gu, method = "spearman")
    ) |>
      dplyr::bind_rows(
        tibble::tibble(
          election_date = ed,
          year = as.integer(obj$year),
          lambda = lambda,
          n_nodes = nrow(X),
          n_components = ncol(X),
          family = "greenacre_weighted_power",
          benchmark = "greenacre_weighted_lra",
          distance_pearson_cor = cor_or_na(d_wlra, d_gw, method = "pearson"),
          distance_spearman_cor = cor_or_na(d_wlra, d_gw, method = "spearman")
        )
      )
  })
}

greenacre_power_alignment_profile <- purrr::imap_dfr(
  objects,
  greenacre_power_alignment_profile_one
)

greenacre_power_best_by_election <- greenacre_power_alignment_profile |>
  dplyr::group_by(.data$family, .data$benchmark, .data$election_date) |>
  dplyr::arrange(
    dplyr::desc(.data$distance_spearman_cor),
    .data$lambda,
    .by_group = TRUE
  ) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::rename(
    best_lambda = "lambda",
    best_distance_pearson_cor = "distance_pearson_cor",
    best_distance_spearman_cor = "distance_spearman_cor"
  )

greenacre_power_best_summary <- greenacre_power_alignment_profile |>
  dplyr::group_by(.data$family, .data$benchmark, .data$lambda) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    spearman_median = stats::median(.data$distance_spearman_cor, na.rm = TRUE),
    pearson_median = stats::median(.data$distance_pearson_cor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    .data$family,
    .data$benchmark,
    dplyr::desc(.data$spearman_median),
    dplyr::desc(.data$pearson_median),
    .data$lambda
  )

greenacre_power_best_by_spec <- greenacre_power_best_summary |>
  dplyr::group_by(.data$family, .data$benchmark) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


# Zero-replacement sensitivity for log-ratio benchmarks.

message("Stage 04: zero-replacement sensitivity diagnostics.")

# tutte le regole di sostituzione zeri da testare:
replacement_specs <- dplyr::bind_rows(
  tibble::tibble(
    replacement_type = "fixed_count",
    replacement_value = zero_pseudocount_grid
  ),
  tibble::tibble(
    replacement_type = "share_floor",
    replacement_value = zero_share_floor_grid
  )
)

logratio_distances_with_replacement <- function(X, replacement_type, replacement_value, masses = NULL) {
  X <- as.matrix(X)
  
  if (is.null(masses)) {
    masses <- colMeans(close_rows(X))
  }
  
  X_regularized <- switch(
    replacement_type,
    fixed_count = regularize_zeros(X, epsilon = replacement_value),
    share_floor = regularize_zeros_share_floor(X, eta = replacement_value),
    stop("Unknown replacement_type: ", replacement_type, call. = FALSE)
  )
  
  Z_clr <- clr_from_regularized_amounts(X_regularized)
  
  Z_wlra <- greenacre_weighted_lra_from_regularized_amounts(
    X_regularized,
    masses = masses
  )
  
  list(
    aitchison = stats::dist(Z_clr),
    greenacre_weighted_lra = stats::dist(Z_wlra)
  ) # restituisce dua amtrici distanza log-ratio regoalrizzate
}

zero_replacement_sensitivity <- purrr::imap_dfr(objects, function(obj, ed) { # applico diagnostica a tutte le elezioni. imap_dfr() combina tutte le tibble finali riga per riga. 
  X <- obj$amount_matrix
  masses <- colMeans(close_rows(X))
  preferred_distance <- upper_vec(obj$distance_matrices[[greenacre_weighted_geometry]])
  
  baseline <- logratio_distances_with_replacement(
    X,
    replacement_type = "fixed_count",
    replacement_value = zero_pseudocount, # epsilon = 10
    masses = masses
  )
  
  purrr::pmap_dfr(replacement_specs, function(replacement_type, replacement_value) { # costruzione distanze alternative per ogni coppia (tipo,valore) (colonne tibble passati come argmenti funzione con pmap_dfr, che itera su tutte le righe di replacement_spacs)
    D_alt <- logratio_distances_with_replacement(
      X,
      replacement_type = replacement_type,
      replacement_value = replacement_value,
      masses = masses
    ) #D_alt lista con due elementi: D_alt$aitchison, D_alt$greenacre_weighted_lra
    
    purrr::imap_dfr(D_alt, function(D, metric) { # metric è "aitchison" oppure "greenacre_weighted_lra".
      d_alt <- upper_vec(D)
      d_base <- upper_vec(baseline[[metric]])
      
      against_baseline <- tibble::tibble(
        election_date = ed,
        year = as.integer(obj$year),
        metric = metric,
        replacement_type = replacement_type,
        replacement_value = replacement_value,
        comparison = "against_baseline_fixed_count_10",
        reference_metric = paste0(metric, "_fixed_count_10"),
        pearson_cor = cor_or_na(d_base, d_alt, method = "pearson"),
        spearman_cor = cor_or_na(d_base, d_alt, method = "spearman")
      )
      
      against_preferred <- tibble::tibble(
        election_date = ed,
        year = as.integer(obj$year),
        metric = metric,
        replacement_type = replacement_type,
        replacement_value = replacement_value,
        comparison = paste0("against_preferred_", greenacre_weighted_geometry),
        reference_metric = greenacre_weighted_geometry,
        pearson_cor = cor_or_na(preferred_distance, d_alt, method = "pearson"),
        spearman_cor = cor_or_na(preferred_distance, d_alt, method = "spearman")
      )
      
      dplyr::bind_rows(against_baseline, against_preferred)
    })
  })
})

zero_replacement_sensitivity_summary <- zero_replacement_sensitivity |>
  dplyr::group_by( #raggruppo ingorando elezione
    .data$metric,
    .data$replacement_type,
    .data$replacement_value,
    .data$comparison,
    .data$reference_metric
  ) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    pearson_median = stats::median(.data$pearson_cor, na.rm = TRUE),
    spearman_median = stats::median(.data$spearman_cor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    .data$metric,
    .data$comparison,
    .data$replacement_type,
    .data$replacement_value
  )

zero_replacement_metric_diagnostics <- purrr::imap_dfr(objects, function(obj, ed) {
  X <- obj$amount_matrix
  masses <- colMeans(close_rows(X))
  h0 <- zero_hamming(X)
  h0_share <- h0 / ncol(X)
  
  purrr::pmap_dfr(replacement_specs, function(replacement_type, replacement_value) {
    D_alt <- logratio_distances_with_replacement(
      X,
      replacement_type = replacement_type,
      replacement_value = replacement_value,
      masses = masses
    )
    
    purrr::imap_dfr(D_alt, function(D, metric) {
      d <- upper_vec(D)
      
      tibble::tibble(
        election_date = ed,
        year = as.integer(obj$year),
        metric = metric,
        replacement_type = replacement_type,
        replacement_value = replacement_value,
        zero_hamming_mean = mean(h0, na.rm = TRUE),
        zero_hamming_share_mean = mean(h0_share, na.rm = TRUE),
        distance_zero_hamming_pearson = cor_or_na(d, h0, method = "pearson"),
        distance_zero_hamming_spearman = cor_or_na(d, h0, method = "spearman")
      )
    })
  })
})

zero_replacement_metric_diagnostics_summary <-
  zero_replacement_metric_diagnostics |>
  dplyr::group_by(
    .data$metric,
    .data$replacement_type,
    .data$replacement_value
  ) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    zero_hamming_mean_median = stats::median(.data$zero_hamming_mean, na.rm = TRUE),
    zero_hamming_share_mean_median = stats::median(.data$zero_hamming_share_mean, na.rm = TRUE),
    spearman_median = stats::median(.data$distance_zero_hamming_spearman, na.rm = TRUE),
    pearson_median = stats::median(.data$distance_zero_hamming_pearson, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    .data$metric,
    .data$replacement_type,
    .data$replacement_value
  )



# Subcomposition stability diagnostics.

message("Stage 04: subcomposition stability diagnostics.")

subcomposition_sets <- function(components) { #components = colnames(X)
  components <- as.character(components)
  
  sets <- list(
    drop_non_valid_or_non_voting = setdiff(components, "NON_VALID_OR_NON_VOTING"),
    drop_other_lists = setdiff(components, "OTHER_LISTS"),
    drop_territorial_lists = setdiff(components, "TERRITORIAL_LISTS"),
    autonomous_parties_only = setdiff(components, c("OTHER_LISTS", "TERRITORIAL_LISTS", "NON_VALID_OR_NON_VOTING")),
    party_and_territorial_without_other = setdiff(components, c("OTHER_LISTS", "NON_VALID_OR_NON_VOTING"))
  )
  
  sets <- sets[vapply(sets, function(s) length(s) >= 3, logical(1))]
  sets <- sets[vapply(sets, function(s) length(setdiff(components, s)) > 0, logical(1))]
  sets # lista di vettori di componenti (sottocmposizioni)
}

subcomposition_stability <- purrr::imap_dfr(objects, function(obj, ed) {
  X <- obj$amount_matrix
  
  full_distances <- purrr::map(obj$distance_matrices, upper_vec) # prendo tutte le distanze full già calcolate nell’oggetto e trasformi ogni matrice di distanza in vettore pairwise, per ogni geometria
  
  sets <- subcomposition_sets(colnames(X))
  
  purrr::imap_dfr(sets, function(comps, subcomposition_name) { #ietro su sottocomposzioni. comps è vettore componenti da mantenere
    Xs <- X[, comps, drop = FALSE] # matrice subcomposizionale
    Ds <- distance_set( # ricolcolo tutte distanze su Xs
      Xs,
      epsilon = zero_pseudocount,
      alpha = alpha_selected,
      lambda_unweighted = lambda_unweighted_selected,
      lambda_weighted = lambda_weighted_selected
    )
    sub_distances <- purrr::map(Ds$distances, upper_vec)
    
    #confronto full vs subcomposition:
    purrr::imap_dfr(full_distances, function(d_full, geometry) {
      d_sub <- sub_distances[[geometry]]
      
      tibble::tibble(
        election_date = ed,
        year = as.integer(obj$year),
        geometry = geometry,
        subcomposition = subcomposition_name,
        n_full_components = ncol(X),
        n_sub_components = ncol(Xs),
        n_removed_components = ncol(X) - ncol(Xs),
        removed_components = paste(setdiff(colnames(X), comps), collapse = " | "),
        distance_pearson_cor = cor_or_na(d_full, d_sub, method = "pearson"),
        distance_spearman_cor = cor_or_na(d_full, d_sub, method = "spearman")
      )
    })
  })
})

subcomposition_stability_summary <- subcomposition_stability |>
  dplyr::group_by(.data$geometry, .data$subcomposition) |>
  dplyr::summarise(
    n_elections = dplyr::n(),
    spearman_min = min(.data$distance_spearman_cor, na.rm = TRUE),
    spearman_median = stats::median(.data$distance_spearman_cor, na.rm = TRUE),
    spearman_max = max(.data$distance_spearman_cor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$geometry, dplyr::desc(.data$spearman_median))

# Write outputs.

message("Stage 04: writing outputs.")

csv_outputs <- list(
  stage04_support_summary = support_summary,
  stage04_dropped_components_share_ge_005 = support_dropped,
  stage04_retained_components = support_retained,
  stage04_component_final_classification = component_final_classification,
  stage04_territorial_lookup_used = territorial_lookup_used,
  stage04_final_component_zeros = final_component_zeros,
  stage04_final_component_zero_summary = final_component_zero_summary,
  stage04_distance_summary = distance_summary,
  stage04_full_pairwise_geometry_comparison = full_pairwise_geometry_comparison,
  stage04_full_pairwise_geometry_comparison_summary = full_pairwise_geometry_comparison_summary,
  stage04_zero_metric_diagnostics_final_objects = zero_metric_diagnostics,
  stage04_zero_metric_diagnostics_final_objects_summary = zero_metric_diagnostics_summary,
  stage04_component_contributions_by_geometry = component_contributions,
  stage04_component_contributions_top5_by_geometry = component_contributions_top5,
  stage04_component_contributions_summary_by_geometry = component_contributions_summary,
  stage04_clr_variance_by_component = clr_variance_by_component,
  stage04_clr_variance_weighting_summary = clr_variance_weighting_summary,
  stage04_component_masses = component_masses,
  stage04_alpha_alignment_profile = alpha_alignment_profile,
  stage04_alpha_best_by_election = alpha_best_by_election,
  stage04_alpha_best_summary = alpha_best_summary,
  stage04_alpha_best_by_spec = alpha_best_by_spec,
  stage04_greenacre_power_alignment_profile = greenacre_power_alignment_profile,
  stage04_greenacre_power_best_by_election = greenacre_power_best_by_election,
  stage04_greenacre_power_best_summary = greenacre_power_best_summary,
  stage04_greenacre_power_best_by_spec = greenacre_power_best_by_spec,
  stage04_zero_replacement_sensitivity = zero_replacement_sensitivity,
  stage04_zero_replacement_sensitivity_summary = zero_replacement_sensitivity_summary,
  stage04_zero_replacement_metric_diagnostics = zero_replacement_metric_diagnostics,
  stage04_zero_replacement_metric_diagnostics_summary = zero_replacement_metric_diagnostics_summary,
  stage04_subcomposition_stability = subcomposition_stability,
  stage04_subcomposition_stability_summary = subcomposition_stability_summary
)

purrr::iwalk(csv_outputs, ~ write_project_csv(.x, file.path(table_dir, paste0(.y, ".csv")))) #.x è il valore della lista, cioè la tibble; .y è il nome dell’elemento, cioè il nome del file.

write_project_rds(objects, composition_objects_path)
# objects contiene per ogni elezione:
# matrice amounts;
# matrici coordinate;
# masse;
# matrici di distanza.

message("Stage 04 complete.")
message("  Objects: ", composition_objects_path)
message("  Tables:  ", table_dir)
