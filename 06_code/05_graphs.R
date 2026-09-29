source(here::here("06_code", "00_setup.R"))

# PATHS

# file input (amount matrix, composizione chiusa, coordinate, dist matrix per ogni geometria ed elezione t)
composition_objects_path <- file.path(path_data_compositional, "stage04_composition_distance_objects.rds")

spectral_output_dir <- file.path(path_data_processed, "spectral")
spectral_table_dir <- file.path(path_outputs_tables, "spectral")

fs::dir_create(c(spectral_output_dir, spectral_table_dir))

spectral_objects_path <- file.path(spectral_output_dir, "stage05_spectral_objects.rds")
# per ogni election x geometry: 


# SETTINGS
message("Stage 05: setting locally-scaled spectral parameters.")

fixed_cluster_grid <- 2:8
n_eigen_keep <- 9L
axes_to_interpret <- 2:4
axis_extreme_n <- 10L
kmeans_nstart <- 100L
kmeans_iter_max <- 200L
main_kmeans_seed <- 20260731L
preferred_geometry <- "greenacre_weighted_power_0175"

# SMALL UTILITIES

# problema indeterminazione segno eigenvectors. vogli osign reprodudicbility
orient_columns_deterministically <- function(X) {
  for (j in seq_len(ncol(X))) {
    anchor <- which.max( # per ogni colonna, prendo provincia con coord assoluta massima
      abs(X[, j])
    )
    
    if (X[anchor, j] < 0) {
      X[, j] <- -X[, j] # e ne impongo positvità
    }
  }
  
  X
}

adjusted_rand_index <- function(labels_a, labels_b) {
  tab <- table( # contingency table a parire dalle due paritions delle stesse province
    labels_a,
    labels_b
  )
  
  n <- length(labels_a)
  total_pairs <- choose(n, 2)
  sum_nij <- sum( #coppie concordanti dentro celle
    choose(tab, 2)
  )
  
  sum_ai <- sum( # margini
    choose(rowSums(tab), 2)
  )
  
  sum_bj <- sum(choose(colSums(tab), 2))
  expected <- sum_ai * sum_bj / total_pairs # agreement atteso
  maximum <- 0.5 * (sum_ai + sum_bj) #normalizzazione
  
  (sum_nij - expected) / (maximum - expected)
}

# LOCAL SCALE, AFFINITY, AND LAPLACIAN

neighborhood_size_rule <- function(n_nodes) { # computes # of nearest distances used to estimate sigma_i (10 for every election)
  as.integer(floor(sqrt(n_nodes - 1)))
}

local_scales_from_distances <- function(D) {
  n <- nrow(D) # number of provinces
  m <- neighborhood_size_rule(n)
  sigma <- vapply(
    seq_len(n),
    function(i) {
      d <- sort(D[i, -i])
      
      mean(d[seq_len(m)])
    },
    numeric(1) # vapply() specifica formato atteso output
  )
  
  if (any(sigma <= 0)) { # sanity check
    stop("Neighborhood-averaged local scale is non-positive.", call. = FALSE)
  }
  
  names(sigma) <- rownames(D)
  
  list(sigma = sigma, neighborhood_m = m)
}

locally_scaled_affinity <- function(D) {
  scale_object <- local_scales_from_distances(D)
  sigma <- scale_object$sigma # bandwidth locali
  A <- exp(
    -(D^2) / outer( # costruisce matrice [sigma_i*sigma_j]_(ij)
      sigma,
      sigma,
      "*"
    )
  )
  
  diag(A) <- 0 # se no avrei w_(ii) = e^0=1
  dimnames(A) <- dimnames(D)
  
  list(affinity = A, sigma = sigma, neighborhood_m = scale_object$neighborhood_m)
}

normalized_laplacian <- function(A) {
  degree <- rowSums(A) #weigjted degree per ogni provincia
  
  if (any(degree <= 0)) { # con d_i=0, D^(-1/2) non sarebbe definito
    stop("Affinity matrix has non-positive weighted degree.", call. = FALSE)
  }
  
  inv_sqrt_degree <- 1 / sqrt(degree)
  S <- sweep(
    A,
    1,
    inv_sqrt_degree,
    "*"
  ) # D^(-1/2)A
  
  S <- sweep(
    S,
    2,
    inv_sqrt_degree,
    "*"
  ) #D^(-1/2)AD^(-1/2)
  
  Lsym <- diag(nrow(A)) - S #I - D^(-1/2)AD^(-1/2)
  Lsym <- (Lsym + t(Lsym)) / 2 # enforcing exact simmetry also computationally
  dimnames(Lsym) <- dimnames(A)
  
  list(
    degree = degree, #vector n 
    laplacian_sym = Lsym
  )
}

affinity_diagnostics_one <- function( # summarizing graph
  A,
  sigma,
  neighborhood_m
) {
  degree <- rowSums(A)
  P <- sweep( # ogni riga è distribuzione affinity mass provincia i (normalizzata a somma 1)
    A,
    1,
    degree,
    "/"
  )
  
  neff <- 1 / rowSums(P^2)
  
  tibble::tibble( #tibble con una riga
    n_nodes = nrow(A),
    neighborhood_m = as.integer(neighborhood_m),
    sigma_median = stats::median(sigma),
    sigma_cv = stats::sd(sigma) / mean(sigma),
    weighted_degree_mean = mean(degree),
    weighted_degree_median = stats::median(degree),
    weighted_degree_cv = stats::sd(degree) / mean(degree),
    effective_neighbors_median = stats::median(neff)
  )
}


# SPECTRAL DECOMPOSITION AND FIXED-C CLUSTERING

spectral_decomposition <- function( # L_sym = UDU^T
  Lsym,
  n_keep = n_eigen_keep
) {
  eig <- eigen(Lsym, symmetric = TRUE)
  ord <- order(eig$values, decreasing = FALSE)
  values_all <- eig$values[ord]
  vectors_all <- eig$vectors[, ord, drop = FALSE]
  
  values_all[
    abs(values_all) < 1e-12
  ] <- 0 # lambda_1 = 0 per grafo connesso, forzo intrèretazione come zero numerico
  values <- values_all[
    seq_len(n_keep) # keep first 9
  ]
  
  vectors <- vectors_all[, seq_len(n_keep), drop = FALSE]
  vectors <- orient_columns_deterministically(vectors)
  rownames(vectors) <- rownames(Lsym)
  colnames(vectors) <- paste0("eigen_", seq_len(n_keep))
  
  list(laplacian_eigenvalues = values, eigenvectors = vectors)
}

spectral_embedding <- function(eigenvectors, n_clusters) {
  Y <- eigenvectors[
    ,
    seq_len(n_clusters),
    drop = FALSE
  ] # per un certo C seleziona [u1, ..., uC]
  
  norms <- sqrt(rowSums(Y^2)) #norms per ogni provicnia 
  
  if (any(norms == 0)) {
    stop("Zero row norm in spectral embedding.", call. = FALSE)
  }
  
  sweep( #row normalization
    Y,
    1,
    norms,
    "/"
  )
} #output Y in R?(nxC), matrice su cui viene eseguto k-means

run_spectral_kmeans <- function(eigenvectors, n_clusters, seed) {
  Y <- spectral_embedding(eigenvectors, n_clusters)
  
  set.seed(seed)
  
  fit <- stats::kmeans(Y, centers = n_clusters, nstart = kmeans_nstart, iter.max = kmeans_iter_max)
  labels <- as.integer(fit$cluster)
  
  list(embedding = Y, labels = labels, sizes = tabulate(labels, nbins = n_clusters))
}

# Singleton convention: individual silhouette = 0.
silhouette_mean <- function(labels, D) {
  labels <- as.integer(labels)
  D <- as.matrix(D)
  n <- length(labels)
  clusters <- sort(unique(labels))
  sil <- numeric(n)
  
  for (i in seq_len(n)) {
    own <- labels[i]
    own_idx <- which( # province che stanno nell ostesso cluster
      labels == own &
        seq_len(n) != i
    )
    
    if (length(own_idx) == 0L) {
      sil[i] <- 0
      next
    }
    
    a_i <- mean( # mean within-cluster distance
      D[i, own_idx]
    )
    
    other_clusters <- setdiff(clusters, own)
    b_i <- min( #min mean distance from i to other clusters
      vapply(other_clusters, function(cl) { mean(D[i, labels == cl]) }, numeric(1))
    )
    
    denom <- max(a_i, b_i)
    
    sil[i] <- if (denom > 0) {
      (b_i - a_i) / denom
    } else {
      0
    }
  }
  
  mean(sil)
}

graph_cut_diagnostics <- function(A, labels) {
  degree <- rowSums(A)
  clusters <- sort(unique(labels))
  ncut_terms <- numeric(length(clusters))
  conductance <- numeric(length(clusters))
  
  for (r in seq_along(clusters)) { # per ogni cluster seleziono i suoi nodi
    idx <- labels == clusters[r]
    cut_weight <- sum(A[idx, !idx, drop = FALSE])
    cluster_volume <- sum(degree[idx])
    complement_volume <- sum(degree[!idx])
    ncut_terms[r] <- cut_weight / cluster_volume
    conductance[r] <- cut_weight / min(cluster_volume, complement_volume)
  }
  
  tibble::tibble(
    normalized_cut_total = sum(ncut_terms),
    conductance_mean = mean(conductance),
    conductance_max = max(conductance)
  )
}

fixedC_quality_one <- function( # restituisce tutte le diagnostics per una singola (t,c) sotto la preferred geometry
  D,
  A,
  laplacian_eigenvalues,
  cluster_fit,
  n_clusters
) {
  C <- as.integer(n_clusters)
  spectral_D <- as.matrix(stats::dist(cluster_fit$embedding)) # spectral disatance per spectral silhouette
  cuts <- graph_cut_diagnostics(A, cluster_fit$labels)
  
  tibble::tibble( #tibble una riga
    n_clusters = C,
    laplacian_eigengap_after_C = laplacian_eigenvalues[C + 1L] - laplacian_eigenvalues[C],
    spectral_silhouette_mean = silhouette_mean(
      cluster_fit$labels,
      spectral_D # cluster seèartion nello spectral embedding
    ),
    electoral_silhouette_mean = silhouette_mean(
      cluster_fit$labels,
      D # smisura stessa partition ma nell ospazio elettorale originale
    ),
    min_cluster_size = min(cluster_fit$sizes),
    median_cluster_size = stats::median(cluster_fit$sizes),
    max_cluster_size = max(cluster_fit$sizes),
    largest_cluster_share = max(cluster_fit$sizes) / sum(cluster_fit$sizes)
  ) |>
    dplyr::bind_cols(cuts)
}


# POLITICAL INTERPRETATION — MAIN SPECIFICATION ONLY

axis_component_correlations <- function(
    eigenvectors, #coordinate province sugli assi spettrali
    Z_geometry, # coord preferred egeometry
    P_shares, #quote composizonali originali
    axes, # assi 2,3,4
    ed,
    year,
    geometry
) {
  out <- list()
  n <- 0L
  
  for (axis in axes) {
    y <- eigenvectors[, axis] # vettore lungo I_t, una spectral coord per provincia
    
    for (component in colnames(Z_geometry)) {
      n <- n + 1L
      z <- Z_geometry[, component]
      p <- P_shares[, component]
      out[[n]] <- tibble::tibble(
        election_date = ed,
        year = as.integer(year),
        geometry = geometry,
        eigen_index = axis,
        component = component,
        coordinate_pearson_cor = # 4 correlazioni per ogni coppia asse-componente
          stats::cor(y, z, method = "pearson"),
        coordinate_spearman_cor = stats::cor(y, z, method = "spearman"),
        share_pearson_cor = stats::cor(y, p, method = "pearson"),
        share_spearman_cor = stats::cor(y, p, method = "spearman"),
        mean_component_share = mean(p) # media provinciale non pesata
      )
    }
  }
  
  dplyr::bind_rows(out)
}

axis_province_extremes <- function( # province che definiscono i due estremi dell'asse
  eigenvectors,
  node_meta,
  axes,
  ed,
  year,
  geometry,
  n_extreme = axis_extreme_n
) {
  out <- list()
  
  for (axis in axes) { # per ogni asse, creo tabella (province, u_l(i), region)
    tab <- tibble::tibble(
      territory = node_meta$territory,
      spectral_coordinate = as.numeric(eigenvectors[, axis]),
      region = node_meta$region
    )
    
    negative <- tab |>
      dplyr::arrange(
        .data$spectral_coordinate
      ) |>
      dplyr::slice_head(
        n = n_extreme
      ) |>
      dplyr::mutate(side = "negative", side_rank = dplyr::row_number())
    
    positive <- tab |>
      dplyr::arrange(
        dplyr::desc(.data$spectral_coordinate)
      ) |>
      dplyr::slice_head(
        n = n_extreme
      ) |>
      dplyr::mutate(side = "positive", side_rank = dplyr::row_number())
    
    out[[length(out) + 1L]] <- dplyr::bind_rows(negative, positive) |>
      dplyr::mutate(
        election_date = ed,
        year = as.integer(year),
        geometry = geometry,
        eigen_index = axis,
        .before = 1
      )
  }
  
  dplyr::bind_rows(out) # 19*(10+10)*3 righe
}

# Arithmetic mean electoral profile, not an Aitchison centroid.
cluster_component_profiles <- function(labels, P_shares, ed, year, geometry, n_clusters) {
  overall_mean <- colMeans(P_shares)
  out <- vector("list", n_clusters) #lista lunghezza C (uno slot per cluster)
  
  for (cl in seq_len(n_clusters)) {
    idx <- labels == cl
    
    mean_share <-
      colMeans( #media per ogni componente. somma a 1 perchè ogni riga di P somma a 1, ma non è aitchison centre
        P_shares[
          idx,
          ,
          drop = FALSE
        ]
      )
    
    out[[cl]] <- tibble::tibble(
      election_date = ed,
      year = as.integer(year),
      geometry = geometry,
      n_clusters = n_clusters,
      cluster = cl,
      cluster_size = sum(idx),
      component = colnames(P_shares),
      mean_share = mean_share,
      overall_mean_share = overall_mean,
      share_difference = mean_share - overall_mean,
      share_ratio = mean_share / overall_mean
    )
  }
  
  dplyr::bind_rows(out) # per ongi cluster, una riga per componente
}

cluster_region_profiles <- function(labels, node_meta, ed, year, geometry, n_clusters) {
  base <- tibble::tibble( #una riga per provincia, con region e cluster
    territory = as.character(node_meta$territory),
    region = as.character(node_meta$region),
    cluster = as.integer(labels)
  )
  
  region_totals <- base |>
    dplyr::count(.data$region, name = "region_size")
  
  base |>
    dplyr::count( # conteggio cluster x region
      .data$cluster,
      .data$region,
      name = "n_territories"
    ) |>
    dplyr::group_by(.data$cluster) |>
    dplyr::mutate(
      cluster_size = sum(.data$n_territories),
      share_within_cluster =
        .data$n_territories / .data$cluster_size #quota province cluster r appartenenete a regione s
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      region_totals,
      by = "region",
      relationship = "many-to-one" #ogni regione compare molte volte nella table cluster-region ma solo una volta in region_totals
    ) |>
    dplyr::mutate(
      share_of_region_in_cluster =
        .data$n_territories / .data$region_size, # quoata province regione s in cluster r
      election_date = ed,
      year = as.integer(year),
      geometry = geometry,
      n_clusters = n_clusters,
      .before = 1
    )
} # output: una riga per ogni combinazione clsuter region effettivamnete osservata

cluster_assignment_table <- function( # per ongi (t,g,C), uaa riga per provincia con suo cluster
  labels,
  node_meta,
  ed,
  year,
  geometry,
  n_clusters
) {
  tibble::tibble(
    election_date = ed,
    year = as.integer(year),
    geometry = geometry,
    n_clusters = as.integer(n_clusters),
    territory = node_meta$territory,
    cluster = as.integer(labels),
    is_preferred_geometry = geometry == preferred_geometry,
    region = node_meta$region
  ) |>
    dplyr::arrange(.data$cluster, .data$territory)
}

# =============================================================================
# 7. PARTITION SIMILARITY
# =============================================================================

entropy_from_counts <- function(counts) { # input è rowSums(tab), cioè cluster sizes
  counts <- as.numeric(counts)
  counts <- counts[counts > 0] # zero-probability cells non contribuiscono perchè lim_(p-->0)plogp=0
  p <- counts / sum(counts)
  
  -sum(
    p * log(p) # shannon entropy. H=0 tutte provicne stesso cluster, H=logC con bilanciamento perfetto
  )
}

mutual_information_from_table <- function(tab) { # input contingency tables tra due aprtitions
  pij <- as.matrix(tab) / sum(tab) # joint probabilities
  expected <- outer(rowSums(pij), colSums(pij)) # joint probabilities sotto indipendenza
  positive <- pij > 0
  
  sum(pij[positive] * log(pij[positive] / expected[positive]))
}

partition_similarity_metrics <- function(labels_a, labels_b) {
  labels_a <- as.integer(labels_a)
  labels_b <- as.integer(labels_b)
  n <- length(labels_a)
  tab <- table(labels_a, labels_b)
  mi <- mutual_information_from_table(tab)
  h_a <- entropy_from_counts(rowSums(tab))
  h_b <- entropy_from_counts(colSums(tab))
  
  nmi <- if (h_a + h_b > 0) {
    2 * mi / (h_a + h_b)
  } else {
    NA_real_
  }
  
  vi <- h_a + h_b - 2 * mi
  pair_index <- upper.tri( # restituisce TRUE per elementi dopra la diagonale principale
    matrix(0, nrow = n, ncol = n)
  )
  
  ca <- as.numeric(
    outer( # matrice TRUE if provicne nelllo stesso cluster else FALSE
      labels_a,
      labels_a,
      "=="
    )[pair_index] #estrae triangolo supeirore e converte a 0/1
  )
  
  cb <- as.numeric(outer(labels_b, labels_b, "==")[pair_index])
  
  tibble::tibble(
    n_common_territories = n,
    adjusted_rand = adjusted_rand_index(labels_a, labels_b),
    normalized_mutual_information = nmi,
    variation_of_information = vi,
    coassignment_pearson = stats::cor(ca, cb)
  )
} # tibble di una riga con ARI, NMI, VI, corr coassignment

labels_join_metrics <- function(df_a, df_b) {
  joined <- df_a |>
    dplyr::select(
      "territory",
      cluster_a = "cluster"
    ) |>
    dplyr::inner_join( # conservo soltanto province presenti in enyrambe le partitions
      df_b |>
        dplyr::select("territory", cluster_b = "cluster"),
      by = "territory",
      relationship = "one-to-one" # ogni provincia max una volta in una tabella (non può stare i ndue cluster diversi)
    )
  
  partition_similarity_metrics(joined$cluster_a, joined$cluster_b)
}

cluster_stability_by_geometry <- function(assignments, preferred_geometry) {
  reference <- assignments |>
    dplyr::filter(.data$geometry == preferred_geometry)
  
  alternatives <- assignments |> #specficazioni da cofnrontare: una riga per ogni t,C, geometry != g*: 19*7*5=665 righe
    dplyr::distinct(
      .data$election_date,
      .data$year,
      .data$n_clusters,
      .data$geometry
    ) |>
    dplyr::filter(.data$geometry != preferred_geometry)
  
  out <- list()
  
  for (ii in seq_len(nrow(alternatives))) {
    sp <- alternatives[ii, ] #current (t,C,g) specification
    
    df_a <- reference |>
      dplyr::filter(.data$election_date == sp$election_date, .data$n_clusters == sp$n_clusters)
    
    df_b <- assignments |>
      dplyr::filter(
        .data$election_date == sp$election_date,
        .data$n_clusters == sp$n_clusters,
        .data$geometry == sp$geometry
      )
    
    met <- labels_join_metrics(df_a, df_b)
    
    out[[length(out) + 1L]] <- met |>
      dplyr::mutate(
        election_date = sp$election_date,
        year = sp$year,
        n_clusters = sp$n_clusters,
        reference_geometry = preferred_geometry,
        comparison_geometry = sp$geometry,
        comparison_type = "geometry_vs_preferred",
        .before = 1
      )
  }
  
  dplyr::bind_rows(out)
}


# TEMPORAL COMPARISON — MAIN SPECIFICATION ONLY

# used only to make transition matrices readable
maximum_overlap_label_map <- function(prev_labels, curr_labels, n_clusters) {
  C <- as.integer(n_clusters)
  labels <- seq_len(C)
  overlap <- as.matrix(table(factor(prev_labels, levels = labels), factor(curr_labels, levels = labels))) # CxC contingency table. da massimzizare #province sulla diagonale dopo relabeling
  best_score <- -Inf # stores max valore trovato
  best_cols <- integer(C) #stores correspondent permutation
  
  search_assignment <- function(row_index, remaining_cols, chosen_cols, running_score) { #esplora tutte possibili permutazioni
    if (row_index > C) {
      if (running_score > best_score) {
        best_score <<- running_score
        best_cols <<- chosen_cols
      }
      
      return(invisible(NULL))
    }
    
    for (col_index in remaining_cols) {
      search_assignment(
        row_index = row_index + 1L,
        remaining_cols = remaining_cols[remaining_cols != col_index],
        chosen_cols = c(chosen_cols, col_index),
        running_score = running_score + overlap[row_index, col_index]
      )
    } # complexity: prova tutte le C! eprmutazioni. con 8!=40320
    
    invisible(NULL)
  }
  
  search_assignment(row_index = 1L, remaining_cols = seq_len(C), chosen_cols = integer(0), running_score = 0)
  
  mapping <- integer(C)
  
  for (row_index in seq_len(C)) {
    mapping[
      best_cols[row_index]
    ] <- row_index
  }
  
  as.integer(mapping[curr_labels])
}

compute_temporal_outputs_main_spec <- function(assignments, preferred_geometry) {
  dmain <- assignments |>
    dplyr::filter(
      .data$geometry == preferred_geometry
    ) |>
    dplyr::select("election_date", "year", "n_clusters", "territory", "region", "cluster")
  
  stability_rows <- list()
  transition_rows <- list()
  instability_rows <- list()
  
  for (C in sort(unique(dmain$n_clusters))) { # loop sulle riosluzioni
    d <- dmain |>
      dplyr::filter(.data$n_clusters == C)
    
    elections <- d |>
      dplyr::distinct(
        .data$election_date,
        .data$year
      ) |>
      dplyr::arrange(.data$year, .data$election_date)
    
    for (tt in 2:nrow(elections)) { # coppie (t1,t2), (t2,t3), ... , (t18,t19)
      prev <- elections[tt - 1L, ]
      curr <- elections[tt, ]
      
      joined <- d |>
        dplyr::filter(
          .data$election_date == prev$election_date
        ) |>
        dplyr::select(
          "territory",
          region_previous = "region",
          cluster_previous = "cluster"
        ) |>
        dplyr::inner_join(
          d |>
            dplyr::filter(
              .data$election_date == curr$election_date
            ) |>
            dplyr::select("territory", region_current = "region", cluster_current = "cluster"),
          by = "territory",
          relationship = "one-to-one" #ogni provincia può apparire al massimo una volta in ciascuna election-partition
        )
      
      # Label-invariant partition similarity: ARI, NMI, VI, coass cor
      stability_rows[[
        length(stability_rows) + 1L
      ]] <- partition_similarity_metrics(joined$cluster_previous, joined$cluster_current) |>
        dplyr::mutate(
          election_date_previous = prev$election_date,
          year_previous = prev$year,
          election_date_current = curr$election_date,
          year_current = curr$year,
          geometry = preferred_geometry,
          n_clusters = C,
          .before = 1
        )
      
      # transition matrix
      cluster_current_matched <- maximum_overlap_label_map(
        prev_labels = joined$cluster_previous,
        curr_labels = joined$cluster_current,
        n_clusters = C
      )
      
      transition_rows[[
        length(transition_rows) + 1L
      ]] <- tibble::tibble( # una riga per provincia
        cluster_previous = joined$cluster_previous,
        cluster_current_matched = cluster_current_matched
      ) |>
        dplyr::count( # per ogni cella rs, numero province che erano in cluster r a t-1 e in s a t
          .data$cluster_previous,
          .data$cluster_current_matched,
          name = "n_territories"
        ) |>
        tidyr::complete(
          cluster_previous = seq_len(C),
          cluster_current_matched = seq_len(C),
          fill = list(n_territories = 0L) # complete forza tutte le CxC combinazioni . 0 per transizioni assenti
        ) |>
        dplyr::group_by(
          .data$cluster_previous
        ) |>
        dplyr::mutate(
          origin_cluster_size = sum(.data$n_territories),
          share_of_origin_cluster = .data$n_territories / .data$origin_cluster_size # diagonale è misura persistenza cluster
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          election_date_previous = prev$election_date,
          year_previous = prev$year,
          election_date_current = curr$election_date,
          year_current = curr$year,
          geometry = preferred_geometry,
          n_clusters = C,
          n_common_territories = nrow(joined),
          label_matching_rule = "exact_maximum_overlap",
          .before = 1
        )
      
      # province instability: province-level coassignment Jaccard.
      prev_co <- outer(joined$cluster_previous, joined$cluster_previous, "==") # boolean n x n matrix: cella ij TRUE se prov i e j erano nello stesso lcuster al tempo precedente
      curr_co <- outer(joined$cluster_current, joined$cluster_current, "==")
      diag(prev_co) <- FALSE #eliminazione self-membership
      diag(curr_co) <- FALSE
      intersection_size <- rowSums(prev_co & curr_co)
      union_size <- rowSums(prev_co | curr_co)
      jaccard_same_cluster <- ifelse(union_size == 0, 1, intersection_size / union_size) # caso speciale 0/0 impongo jaccard=1 perchè due isniemi sono uguali (entrambi vuoit)
      
      instability_rows[[
        length(instability_rows) + 1L
      ]] <- tibble::tibble(
        territory = joined$territory,
        region_previous = joined$region_previous,
        region_current = joined$region_current,
        n_clusters = C,
        election_date_previous = prev$election_date,
        year_previous = prev$year,
        election_date_current = curr$election_date,
        year_current = curr$year,
        cluster_previous_raw = joined$cluster_previous,
        cluster_current_raw = joined$cluster_current,
        jaccard_same_cluster = jaccard_same_cluster,
        coassignment_instability = 1 - jaccard_same_cluster
      ) # tabella con una riga per territorio x coppia elections x C
    }
  }
  
  list(
    stability = dplyr::bind_rows(stability_rows),
    transitions = dplyr::bind_rows(transition_rows),
    province_instability = dplyr::bind_rows(instability_rows)
  )
}

# LOAD STAGE 04 OBJECTS

message("Stage 05: loading Stage 04 compositional distance objects.")

if (!file.exists(composition_objects_path)) {
  stop("Stage 04 object not found: ", composition_objects_path, call. = FALSE)
}

objects <- readRDS(composition_objects_path)
# 
# if (!is.list(objects) || length(objects) == 0L) {
#   stop("Stage 04 object is not a non-empty list.", call. = FALSE)
# }
# 
# if (is.null(names(objects)) || any(!nzchar(names(objects)))) {
#   stop("Stage 04 objects must be named by election identifier/date.", call. = FALSE)
# }

geometry_names <- names(objects[[1]]$distance_matrices)

# if (is.null(geometry_names) || length(geometry_names) == 0L) {
#   stop("No Stage 04 distance geometries found.", call. = FALSE)
# }

# same_geometries <- vapply(
#   objects,
#   function(obj) {
#     identical(names(obj$distance_matrices), geometry_names)
#   },
#   logical(1)
# )
# 
# if (!all(same_geometries)) {
#   stop("Stage 04 election objects do not contain the same distance geometries.", call. = FALSE)
# }
# 
# if (!(preferred_geometry %in% geometry_names)) {
#   stop(
#     "Preferred geometry not found in Stage 04 objects: ",
#     preferred_geometry,
#     "\nAvailable geometries: ",
#     paste(geometry_names, collapse = " | "),
#     call. = FALSE
#   )
# }

observed_neighborhood_m <- vapply(
  objects,
  function(obj) {
    neighborhood_size_rule(nrow(obj$amount_matrix))
  },
  integer(1)
) # output integer vector lungo 19

message("Stage 05: preferred geometry = ", preferred_geometry)

# message(
#   "Stage 05: neighborhood rule = floor(sqrt(I_t - 1)); observed m_t = ",
#   paste(sort(unique(observed_neighborhood_m)), collapse = ", ")
# )

message("Stage 05: fixed cluster grid = ", paste(fixed_cluster_grid, collapse = ", "))

message("Stage 05: all geometries = ", paste(geometry_names, collapse = " | "))


# # GRAPH / OPERATOR SPECS
# 
# graph_specs <- tibble::tibble(geometry = geometry_names) |>
#   dplyr::mutate(
#     local_scaling_principle = "point_specific_local_scaling",
#     local_scaling_reference = "Zelnik-Manor_and_Perona_2004_principle_adapted",
#     local_scale_estimator = "mean_distance_to_first_m_nearest_neighbors",
#     neighborhood_size_rule = "floor(sqrt(I_t - 1))",
#     observed_neighborhood_m = paste(sort(unique(observed_neighborhood_m)), collapse = "|"),
#     affinity_formula = "exp(-d_ij^2/(sigma_i*sigma_j))",
#     graph_structure = "complete_weighted_graph_locality_through_weights",
#     operator = "symmetric_normalized_laplacian_direct_eigendecomposition",
#     cluster_count_strategy = "fixed_resolution_grid_no_automatic_selection",
#     fixed_cluster_grid = paste(fixed_cluster_grid, collapse = "|"),
#     n_eigen_keep = n_eigen_keep,
#     interpreted_axes = paste(axes_to_interpret, collapse = "|"),
#     kmeans_nstart = kmeans_nstart,
#     is_preferred_geometry = .data$geometry == preferred_geometry
#   ) |>
#   dplyr::arrange(dplyr::desc(.data$is_preferred_geometry), .data$geometry)


# MAIN LOOP

message("Stage 05: building locally scaled affinities, Laplacian spectra, and fixed-C partitions.")

# liste di accumulo, poi bind_rows(). (fare rbind dentro loop è inefficiente: ogni volta R deve copiare e ricostruire l'oggetto)
spectral_objects <- list()
affinity_diagnostics_rows <- list()
laplacian_eigenvalue_rows <- list()
spectral_coordinate_rows_main <- list()
cluster_quality_rows_main <- list()
cluster_assignment_rows <- list()
axis_component_correlation_rows_main <- list()
axis_province_extreme_rows_main <- list()
cluster_component_profile_rows_main <- list()
cluster_region_profile_rows_main <- list()

graph_row_id <- 0L
assignment_row_id <- 0L
main_profile_row_id <- 0L

for (ed in names(objects)) {
  obj <- objects[[ed]] # per ogni election, estraggo relativo oggetto stage_04
  year <- as.integer(obj$year)
  
  node_meta <- obj$node_metadata |>
    dplyr::transmute(territory = as.character(.data$territory), region = as.character(.data$region))
  
  P_shares <- obj$euclidean_matrix #quote composzionali originali chiuse
  Z_preferred <- obj$greenacre_weighted_power_matrix # coord geometriaprinciaple
  
  if (
    !identical(node_meta$territory, rownames(P_shares)) ||
    !identical(rownames(P_shares), rownames(Z_preferred))
  ) {
    stop(
      "Stage 04 node metadata and coordinate matrices are not aligned for election ",
      ed,
      ".",
      call. = FALSE
    )
  }
  
  for (geometry_index in seq_along(geometry_names)) {
    geometry <- geometry_names[[geometry_index]]
    D <- as.matrix(obj$distance_matrices[[geometry]])
    graph_row_id <- graph_row_id + 1L # output che hanno livello granularità una voce epr ogni grafo (cioè epr ogni (election,geometry))
    
    message("  election=", ed, " | geometry=", geometry)
    
    aff <- locally_scaled_affinity(D)
    A <- aff$affinity
    sigma <- aff$sigma
    neighborhood_m <- aff$neighborhood_m
    op <- normalized_laplacian(A)
    Lsym <- op$laplacian_sym
    degree <- op$degree
    eig <- spectral_decomposition(Lsym, n_keep = n_eigen_keep)
    fixed_fits <- list()
    
    for (C in fixed_cluster_grid) { #qua loop anche sui cluster
      main_fit_seed <- main_kmeans_seed + 100000L * year + 1000L * geometry_index + 10L * C #seed per ogni (t,g,C). seed(123) una volta non rende ogni isngolo fit idnipendete da prdine codice. seed (t,g,C) non dipenderebbe solo da (t,g,C) ma anche da quanti fit sono stati eseguiti pitma, che consumano parte della sequenza casuale
      fit_C <- run_spectral_kmeans(eigenvectors = eig$eigenvectors, n_clusters = C, seed = main_fit_seed)
      fixed_fits[[as.character(C)]] <- fit_C
      assignment_row_id <- assignment_row_id + 1L
      cluster_assignment_rows[[assignment_row_id]] <- cluster_assignment_table(
        labels = fit_C$labels,
        node_meta = node_meta,
        ed = ed,
        year = year,
        geometry = geometry,
        n_clusters = C
      )
      
      if (geometry == preferred_geometry) {
        cluster_quality_rows_main[[length(cluster_quality_rows_main) + 1L]] <- fixedC_quality_one(
          D = D,
          A = A,
          laplacian_eigenvalues = eig$laplacian_eigenvalues,
          cluster_fit = fit_C,
          n_clusters = C
        ) |>
          dplyr::mutate(
            election_date = ed,
            year = year,
            geometry = geometry,
            neighborhood_m = neighborhood_m,
            # cluster_count_strategy = "fixed_resolution_grid_no_automatic_selection",
            # diagnostic_note = paste(
            #   "All metrics are diagnostics indexed by C;",
            #   "none selects a unique true number of clusters."
            #),
            .before = 1
          )
        
        main_profile_row_id <- main_profile_row_id + 1L
        cluster_component_profile_rows_main[[main_profile_row_id]] <- cluster_component_profiles(
          labels = fit_C$labels,
          P_shares = P_shares,
          ed = ed,
          year = year,
          geometry = geometry,
          n_clusters = C
        )
        
        cluster_region_profile_rows_main[[main_profile_row_id]] <- cluster_region_profiles(
          labels = fit_C$labels,
          node_meta = node_meta,
          ed = ed,
          year = year,
          geometry = geometry,
          n_clusters = C
        )
      }
    }
    
    key <- paste(ed, geometry, sep = "||") #chiave etstuale epr salvare oggetto in spectal_objects list (ex 1994-03-27||greenacre_weighted_power)
    spectral_objects[[key]] <- list(
      election_date = ed,
      year = year,
      geometry = geometry,
      n_nodes = nrow(D),
      neighborhood_m = neighborhood_m,
      local_scale_rule = "mean_first_m_neighbors__m=floor(sqrt(I_t-1))",
      local_scales = sigma,
      weighted_degree = degree,
      laplacian_eigenvalues = eig$laplacian_eigenvalues,
      eigenvectors = eig$eigenvectors,
      fixedC_cluster_labels = lapply(fixed_fits, function(fit) { fit$labels }) #fixed_fits contiene rilsuatti compelti k_means per ogni C. qui prendo oslo labels
    )
    
    affinity_diagnostics_rows[[graph_row_id]] <- affinity_diagnostics_one(
      A = A,
      sigma = sigma,
      neighborhood_m = neighborhood_m
    ) |>
      dplyr::mutate(
        election_date = ed,
        year = year,
        geometry = geometry,
        is_preferred_geometry = geometry == preferred_geometry,
        .before = 1
      )
    
    laplacian_eigenvalue_rows[[graph_row_id]] <- tibble::tibble(
      election_date = ed,
      year = year,
      geometry = geometry,
      neighborhood_m = neighborhood_m,
      eigen_index = seq_along(eig$laplacian_eigenvalues),
      laplacian_eigenvalue = eig$laplacian_eigenvalues,
      is_preferred_geometry = geometry == preferred_geometry
    )
    
    if (geometry == preferred_geometry) {
      coords <- as.data.frame(eig$eigenvectors)
      coords$territory <- node_meta$territory
      coords$region <- node_meta$region
      
      spectral_coordinate_rows_main[[length(spectral_coordinate_rows_main) + 1L]] <- coords |>
        tidyr::pivot_longer( # formato lungo: territory | region | eigen_name | spectral_coordinate da tabella larga territory | region | eigen_1 | eigen_2 | eigen_3
          cols = dplyr::starts_with("eigen_"),
          names_to = "eigen_name",
          values_to = "spectral_coordinate"
        ) |>
        dplyr::mutate(
          eigen_index = as.integer(gsub("eigen_", "", .data$eigen_name)),
          laplacian_eigenvalue = eig$laplacian_eigenvalues[.data$eigen_index]
        ) |>
        dplyr::transmute(
          election_date = ed,
          year = year,
          geometry = geometry,
          neighborhood_m = neighborhood_m,
          territory = .data$territory,
          region = .data$region,
          eigen_index = .data$eigen_index,
          laplacian_eigenvalue = .data$laplacian_eigenvalue,
          spectral_coordinate = .data$spectral_coordinate
        )
      
      axis_component_correlation_rows_main[[length(axis_component_correlation_rows_main) + 1L]] <-
        axis_component_correlations(
          eigenvectors = eig$eigenvectors,
          Z_geometry = Z_preferred,
          P_shares = P_shares,
          axes = axes_to_interpret,
          ed = ed,
          year = year,
          geometry = geometry
        )
      
      axis_province_extreme_rows_main[[length(axis_province_extreme_rows_main) + 1L]] <-
        axis_province_extremes(
          eigenvectors = eig$eigenvectors,
          node_meta = node_meta,
          axes = axes_to_interpret,
          ed = ed,
          year = year,
          geometry = geometry,
          n_extreme = axis_extreme_n
        )
    }
  }
}

# ASSEMBLE CANONICAL TABLES

message("Stage 05: assembling canonical output tables.")

affinity_diagnostics <- dplyr::bind_rows(affinity_diagnostics_rows) |>
  dplyr::arrange(.data$election_date, dplyr::desc(.data$is_preferred_geometry), .data$geometry)

laplacian_eigenvalues <- dplyr::bind_rows(laplacian_eigenvalue_rows) |>
  dplyr::arrange(
    .data$election_date,
    dplyr::desc(.data$is_preferred_geometry),
    .data$geometry,
    .data$eigen_index
  )

spectral_coordinates_main_spec <- dplyr::bind_rows(spectral_coordinate_rows_main) |>
  dplyr::arrange(.data$election_date, .data$eigen_index, .data$territory)

fixedC_cluster_quality_main_spec <- dplyr::bind_rows(cluster_quality_rows_main) |>
  dplyr::arrange(.data$election_date, .data$n_clusters)

fixedC_cluster_assignments <- dplyr::bind_rows(cluster_assignment_rows) |>
  dplyr::arrange(
    .data$election_date,
    dplyr::desc(.data$is_preferred_geometry),
    .data$geometry,
    .data$n_clusters,
    .data$cluster,
    .data$territory
  )

axis_component_correlations_main_spec <- dplyr::bind_rows(axis_component_correlation_rows_main) |>
  dplyr::mutate(
    abs_coordinate_pearson_cor = abs(.data$coordinate_pearson_cor),
    abs_coordinate_spearman_cor = abs(.data$coordinate_spearman_cor),
    abs_share_pearson_cor = abs(.data$share_pearson_cor),
    abs_share_spearman_cor = abs(.data$share_spearman_cor)
  ) |>
  dplyr::arrange(.data$election_date, .data$eigen_index, dplyr::desc(.data$abs_coordinate_pearson_cor))

axis_province_extremes_main_spec <- dplyr::bind_rows(axis_province_extreme_rows_main) |>
  dplyr::arrange(.data$election_date, .data$eigen_index, .data$side, .data$side_rank)

cluster_component_profiles_main_spec <- dplyr::bind_rows(cluster_component_profile_rows_main) |>
  dplyr::arrange(.data$election_date, .data$n_clusters, .data$cluster, dplyr::desc(.data$share_difference))

cluster_region_profiles_main_spec <- dplyr::bind_rows(cluster_region_profile_rows_main) |>
  dplyr::arrange(
    .data$election_date,
    .data$n_clusters,
    .data$cluster,
    dplyr::desc(.data$share_within_cluster)
  )

# GEOMETRY SENSITIVITY

message("Stage 05: computing geometry-vs-preferred partition stability.")

cluster_geometry_stability <- cluster_stability_by_geometry(
  assignments = fixedC_cluster_assignments,
  preferred_geometry = preferred_geometry
) |>
  dplyr::arrange(.data$election_date, .data$n_clusters, .data$comparison_geometry)

# HISTORICAL CHANGE — MAIN SPECIFICATION ONLY

message("Stage 05: computing main-spec temporal diagnostics.")

temporal_outputs <- compute_temporal_outputs_main_spec(
  assignments = fixedC_cluster_assignments,
  preferred_geometry = preferred_geometry
)

cluster_temporal_stability_main_spec <- temporal_outputs$stability |> #una riga per (C,t-1,t)
  dplyr::arrange(.data$n_clusters, .data$year_current)

transition_matrices_main_spec <- temporal_outputs$transitions |>
  dplyr::arrange(.data$n_clusters, .data$year_current, .data$cluster_previous, .data$cluster_current_matched) # CXC celle di ogni matrice ordinate per C,t, prev cluster, current macthed clsuter

province_instability_main_spec <- temporal_outputs$province_instability |>
  dplyr::arrange(.data$n_clusters, .data$territory, .data$year_current)


# # 15. SANITY CHECK
# 
# expected_eigen_rows <- length(objects) * length(geometry_names) * n_eigen_keep
# 
# if (nrow(laplacian_eigenvalues) != expected_eigen_rows) {
#   stop("Unexpected number of rows in laplacian_eigenvalues.", call. = FALSE)
# }


# WRITING OUTPUTS


message("Stage 05: writing canonical outputs.")

csv_outputs <- list(
  #stage05_graph_specs = graph_specs,
  stage05_affinity_diagnostics = affinity_diagnostics,
  stage05_laplacian_eigenvalues = laplacian_eigenvalues,
  stage05_spectral_coordinates_main_spec = spectral_coordinates_main_spec,
  stage05_fixedC_cluster_quality_main_spec = fixedC_cluster_quality_main_spec,
  stage05_fixedC_cluster_assignments = fixedC_cluster_assignments,
  stage05_axis_component_correlations_main_spec = axis_component_correlations_main_spec,
  stage05_axis_province_extremes_main_spec = axis_province_extremes_main_spec,
  stage05_cluster_component_profiles_main_spec = cluster_component_profiles_main_spec,
  stage05_cluster_region_profiles_main_spec = cluster_region_profiles_main_spec,
  stage05_cluster_geometry_stability = cluster_geometry_stability,
  stage05_cluster_temporal_stability_main_spec = cluster_temporal_stability_main_spec,
  stage05_transition_matrices_main_spec = transition_matrices_main_spec,
  stage05_province_instability_main_spec = province_instability_main_spec
)

purrr::iwalk(csv_outputs, ~ write_project_csv(.x, file.path(spectral_table_dir, paste0(.y, ".csv"))))

write_project_rds(spectral_objects, spectral_objects_path)

message("Stage 05 complete.")
message("  Objects: ", spectral_objects_path)
message("  Tables:  ", spectral_table_dir)
message("  Canonical CSV outputs: ", length(csv_outputs))
