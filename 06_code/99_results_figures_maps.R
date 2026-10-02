# 99_results_figures_maps.R
# Chapter 6 Results: every figure that requires the ISTAT shapefiles.
#
# Main output:
#   - one provincial cluster map for EVERY election x geometry x C combination
#     available in stage05_fixedC_cluster_assignments.csv;
#   - mean province-level instability maps for every C.
#
# Display-label alignment for visualization:
#   1. For each C, preferred-geometry cluster colours are aligned sequentially
#      through time by maximum overlap with the previous election.
#   2. Within each election and C, every alternative geometry is aligned by
#      maximum overlap with that election's already time-aligned preferred
#      partition.


source(here::here("06_code", "00_setup.R"))

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.", call. = FALSE)
}
if (!requireNamespace("clue", quietly = TRUE)) {
  stop(
    "Package 'clue' is required for maximum-overlap label matching. ",
    "Install it once with install.packages('clue').",
    call. = FALSE
  )
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop(
    "Package 'patchwork' is required for composite map panels. ",
    "Install it once with install.packages('patchwork').",
    call. = FALSE
  )
}

preferred_geometry <- "greenacre_weighted_power_0175"

geometry_order <- c(
  "greenacre_weighted_power_0175",
  "greenacre_weighted_lra",
  "aitchison",
  "alpha_0150",
  "greenacre_unweighted_power_0150",
  "euclidean"
)

geometry_labels <- c(
  greenacre_weighted_power_0175 = "GW power (0.175)",
  greenacre_weighted_lra = "Weighted LRA",
  aitchison = "Aitchison",
  alpha_0150 = "TPW alpha (0.150)",
  greenacre_unweighted_power_0150 = "GU power (0.150)",
  euclidean = "Euclidean"
)

cluster_palette <- c(
  "1" = "#0072B2",
  "2" = "#D55E00",
  "3" = "#009E73",
  "4" = "#CC79A7",
  "5" = "#E69F00",
  "6" = "#56B4E9",
  "7" = "#F0E442",
  "8" = "#000000"
)

stage05_table_dir <- file.path(path_outputs_tables, "spectral")
map_root <- file.path(path_outputs_figures, "results", "maps")
cluster_map_root <- file.path(map_root, "clusters_all")
instability_map_root <- file.path(map_root, "province_instability")
geometry_comparison_root <- file.path(map_root, "geometry_comparisons_C4")
transition_pair_root <- file.path(map_root, "transition_pairs_C4")
transition_story_root <- file.path(map_root, "transition_stories_C4")
main_ready_root <- file.path(map_root, "main_ready")
results_table_dir <- file.path(path_outputs_tables, "results")

fs::dir_create(c(
  map_root,
  cluster_map_root,
  instability_map_root,
  geometry_comparison_root,
  transition_pair_root,
  transition_story_root,
  main_ready_root,
  results_table_dir
))

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

save_map <- function(plot, filename, directory, width = 5.4, height = 6.5) {
  fs::dir_create(directory)
  ggplot2::ggsave(
    file.path(directory, paste0(filename, ".pdf")),
    plot = plot, width = width, height = height, units = "in",
    device = "pdf", bg = "white"
  )
  ggplot2::ggsave(
    file.path(directory, paste0(filename, ".png")),
    plot = plot, width = width, height = height, units = "in",
    device = "png", dpi = 320, bg = "white"
  )
  invisible(plot)
}

theme_cluster_map <- function(base_size = 9) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0.5, size = base_size + 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = base_size - 0.5, colour = "grey30"
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 0.5),
      legend.text = ggplot2::element_text(size = base_size - 0.7),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

# -----------------------------------------------------------------------------
# 1. LOAD ASSIGNMENTS AND ISTAT 2022 GEOGRAPHY
# -----------------------------------------------------------------------------

cluster_assignments <- read_required_csv(
  file.path(stage05_table_dir, "stage05_fixedC_cluster_assignments.csv")
)

province_instability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_province_instability_main_spec.csv")
)

temporal_stability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_temporal_stability_main_spec.csv")
)

geometry_stability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_geometry_stability.csv")
)

required_assignment_cols <- c(
  "year", "geometry", "n_clusters", "territory", "cluster"
)
missing_assignment_cols <- setdiff(
  required_assignment_cols,
  names(cluster_assignments)
)
if (length(missing_assignment_cols) > 0L) {
  stop(
    "stage05_fixedC_cluster_assignments.csv is missing: ",
    paste(missing_assignment_cols, collapse = ", "),
    call. = FALSE
  )
}

geo_root <- here::here(
  "05_data",
  "lookup",
  "geography",
  "istat_2022_generalized"
)

province_shp <- list.files(
  geo_root,
  pattern = "^ProvCM01012022_g_WGS84\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)
region_shp <- list.files(
  geo_root,
  pattern = "^Reg01012022_g_WGS84\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(province_shp) != 1L) {
  stop("Expected exactly one ISTAT 2022 province shapefile.", call. = FALSE)
}
if (length(region_shp) != 1L) {
  stop("Expected exactly one ISTAT 2022 region shapefile.", call. = FALSE)
}

province_sf <- sf::st_read(province_shp[[1]], quiet = TRUE)
region_sf <- sf::st_read(region_shp[[1]], quiet = TRUE)

province_lookup <- read_required_csv(
  here::here(
    "05_data",
    "lookup",
    "province_lookup_actual_harmonized.csv"
  )
)

province_crosswalk <- province_lookup |>
  dplyr::transmute(
    province_harmonized = as.character(.data$province_harmonized),
    province_harmonized_code = as.integer(.data$province_harmonized_code)
  ) |>
  dplyr::distinct()

if (anyDuplicated(province_crosswalk$province_harmonized_code)) {
  stop("province_harmonized_code is not unique.", call. = FALSE)
}

province_sf <- province_sf |>
  dplyr::mutate(COD_UTS = as.integer(.data$COD_UTS)) |>
  dplyr::left_join(
    province_crosswalk,
    by = c("COD_UTS" = "province_harmonized_code"),
    relationship = "one-to-one"
  )

if (any(is.na(province_sf$province_harmonized))) {
  stop(
    "At least one ISTAT 2022 polygon does not match the harmonized lookup.",
    call. = FALSE
  )
}

region_sf <- sf::st_transform(region_sf, sf::st_crs(province_sf))

# -----------------------------------------------------------------------------
# 2. MAXIMUM-OVERLAP DISPLAY LABELS
# -----------------------------------------------------------------------------

# Return a named vector:
#   names = raw target-cluster labels
#   values = reference display-cluster labels
max_overlap_mapping <- function(reference_df, target_df, C) {
  common <- dplyr::inner_join(
    reference_df |>
      dplyr::select(
        territory,
        reference_cluster = display_cluster
      ),
    target_df |>
      dplyr::select(
        territory,
        target_cluster = cluster
      ),
    by = "territory"
  )
  
  if (nrow(common) == 0L) {
    stop("No common provinces available for cluster-label alignment.", call. = FALSE)
  }
  
  ref_levels <- seq_len(C)
  target_levels <- sort(unique(target_df$cluster))
  
  if (length(target_levels) != C) {
    stop(
      "Expected ", C, " target clusters but found ",
      length(target_levels), ".",
      call. = FALSE
    )
  }
  
  overlap <- table(
    factor(common$reference_cluster, levels = ref_levels),
    factor(common$target_cluster, levels = target_levels)
  )
  
  # For each reference row, solve_LSAP returns the target column assigned to it.
  assignment <- clue::solve_LSAP(overlap, maximum = TRUE)
  
  mapping <- rep(NA_integer_, C)
  names(mapping) <- as.character(target_levels)
  
  for (ref_cluster in ref_levels) {
    target_column <- as.integer(assignment[[ref_cluster]])
    raw_target_label <- target_levels[[target_column]]
    mapping[[as.character(raw_target_label)]] <- ref_cluster
  }
  
  mapping
}

apply_cluster_mapping <- function(raw_cluster, mapping) {
  out <- unname(mapping[as.character(raw_cluster)])
  if (anyNA(out)) {
    stop("A cluster label could not be mapped.", call. = FALSE)
  }
  as.integer(out)
}

assignments_aligned <- cluster_assignments |>
  dplyr::mutate(
    row_id_for_display = dplyr::row_number(),
    display_cluster = NA_integer_
  )

C_values <- sort(unique(assignments_aligned$n_clusters))
election_years <- sort(unique(assignments_aligned$year))
available_geometries <- geometry_order[
  geometry_order %in% unique(assignments_aligned$geometry)
]

if (!(preferred_geometry %in% available_geometries)) {
  stop("Preferred geometry not found in cluster assignments.", call. = FALSE)
}

for (C in C_values) {
  
  preferred_years <- assignments_aligned |>
    dplyr::filter(
      .data$geometry == preferred_geometry,
      .data$n_clusters == C
    ) |>
    dplyr::pull(.data$year) |>
    unique() |>
    sort()
  
  # Anchor the first election to its native k-means labels.
  first_year <- preferred_years[[1]]
  
  first_idx <- which(
    assignments_aligned$geometry == preferred_geometry &
      assignments_aligned$n_clusters == C &
      assignments_aligned$year == first_year
  )
  
  first_raw_levels <- sort(unique(assignments_aligned$cluster[first_idx]))
  if (length(first_raw_levels) != C) {
    stop("Unexpected number of preferred clusters in first year.", call. = FALSE)
  }
  
  first_mapping <- stats::setNames(
    seq_len(C),
    as.character(first_raw_levels)
  )
  
  assignments_aligned$display_cluster[first_idx] <- apply_cluster_mapping(
    assignments_aligned$cluster[first_idx],
    first_mapping
  )
  
  # Sequential temporal alignment of the preferred geometry.
  if (length(preferred_years) > 1L) {
    for (k in 2:length(preferred_years)) {
      year_previous <- preferred_years[[k - 1L]]
      year_current <- preferred_years[[k]]
      
      reference_df <- assignments_aligned |>
        dplyr::filter(
          .data$geometry == preferred_geometry,
          .data$n_clusters == C,
          .data$year == year_previous
        ) |>
        dplyr::select("territory", "display_cluster")
      
      target_df <- assignments_aligned |>
        dplyr::filter(
          .data$geometry == preferred_geometry,
          .data$n_clusters == C,
          .data$year == year_current
        ) |>
        dplyr::select("territory", "cluster")
      
      mapping <- max_overlap_mapping(reference_df, target_df, C)
      
      idx <- which(
        assignments_aligned$geometry == preferred_geometry &
          assignments_aligned$n_clusters == C &
          assignments_aligned$year == year_current
      )
      
      assignments_aligned$display_cluster[idx] <- apply_cluster_mapping(
        assignments_aligned$cluster[idx],
        mapping
      )
    }
  }
  
  # Within each election, align every alternative geometry to the already
  # time-aligned preferred partition.
  for (yr in preferred_years) {
    
    reference_df <- assignments_aligned |>
      dplyr::filter(
        .data$geometry == preferred_geometry,
        .data$n_clusters == C,
        .data$year == yr
      ) |>
      dplyr::select("territory", "display_cluster")
    
    other_geometries <- assignments_aligned |>
      dplyr::filter(
        .data$n_clusters == C,
        .data$year == yr,
        .data$geometry != preferred_geometry
      ) |>
      dplyr::pull(.data$geometry) |>
      unique()
    
    for (g in other_geometries) {
      
      target_df <- assignments_aligned |>
        dplyr::filter(
          .data$geometry == g,
          .data$n_clusters == C,
          .data$year == yr
        ) |>
        dplyr::select("territory", "cluster")
      
      mapping <- max_overlap_mapping(reference_df, target_df, C)
      
      idx <- which(
        assignments_aligned$geometry == g &
          assignments_aligned$n_clusters == C &
          assignments_aligned$year == yr
      )
      
      assignments_aligned$display_cluster[idx] <- apply_cluster_mapping(
        assignments_aligned$cluster[idx],
        mapping
      )
    }
  }
}

if (anyNA(assignments_aligned$display_cluster)) {
  stop("Some display cluster labels were not assigned.", call. = FALSE)
}

alignment_check <- assignments_aligned |>
  dplyr::group_by(.data$year, .data$geometry, .data$n_clusters) |>
  dplyr::summarise(
    n_raw_clusters = dplyr::n_distinct(.data$cluster),
    n_display_clusters = dplyr::n_distinct(.data$display_cluster),
    .groups = "drop"
  )

if (any(alignment_check$n_raw_clusters != alignment_check$n_clusters) ||
    any(alignment_check$n_display_clusters != alignment_check$n_clusters)) {
  stop("Cluster-label alignment failed an integrity check.", call. = FALSE)
}

cluster_label_map <- assignments_aligned |>
  dplyr::distinct(
    .data$year,
    .data$geometry,
    .data$n_clusters,
    raw_cluster = .data$cluster,
    .data$display_cluster
  ) |>
  dplyr::arrange(
    .data$n_clusters,
    .data$year,
    match(.data$geometry, geometry_order),
    .data$raw_cluster
  )

readr::write_csv(
  cluster_label_map,
  file.path(results_table_dir, "map_cluster_display_label_alignment.csv")
)

# -----------------------------------------------------------------------------
# 3. ONE FUNCTION FOR EVERY CLUSTER MAP
# -----------------------------------------------------------------------------

plot_cluster_map <- function(year, geometry, C) {
  
  assignment <- assignments_aligned |>
    dplyr::filter(
      .data$year == .env$year,
      .data$geometry == .env$geometry,
      .data$n_clusters == .env$C
    ) |>
    dplyr::select(
      "territory",
      "display_cluster"
    )
  
  if (anyDuplicated(assignment$territory)) {
    stop(
      "Duplicate territories in assignment for year=", year,
      ", geometry=", geometry,
      ", C=", C,
      call. = FALSE
    )
  }
  
  if (nrow(assignment) == 0L) {
    stop(
      "No assignment for year=", year,
      ", geometry=", geometry,
      ", C=", C,
      call. = FALSE
    )
  }
  
  map_sf <- province_sf |>
    dplyr::left_join(
      assignment,
      by = c("province_harmonized" = "territory"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      cluster_for_plot = factor(
        .data$display_cluster,
        levels = seq_len(C)
      )
    )
  
  geometry_title <- if (geometry %in% names(geometry_labels)) {
    unname(geometry_labels[[geometry]])
  } else {
    geometry
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_sf,
      ggplot2::aes(fill = .data$cluster_for_plot),
      colour = "white",
      linewidth = 0.12
    ) +
    ggplot2::geom_sf(
      data = region_sf,
      fill = NA,
      colour = "grey25",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = cluster_palette[as.character(seq_len(C))],
      limits = as.character(seq_len(C)),
      drop = FALSE,
      na.value = "grey90",
      name = "Cluster"
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = paste0(year, " - ", geometry_title, " - C = ", C),
      subtitle = "Colours use maximum-overlap display matching; grey = outside the analytical universe"
    ) +
    theme_cluster_map()
}

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 3B. PANEL-READY MAP HELPERS
# -----------------------------------------------------------------------------

plot_cluster_map_clean <- function(
    year,
    geometry,
    C,
    panel_title = NULL,
    show_legend = FALSE
) {
  assignment <- assignments_aligned |>
    dplyr::filter(
      .data$year == .env$year,
      .data$geometry == .env$geometry,
      .data$n_clusters == .env$C
    ) |>
    dplyr::select(
      "territory",
      "display_cluster"
    )
  
  map_sf <- province_sf |>
    dplyr::left_join(
      assignment,
      by = c("province_harmonized" = "territory"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      cluster_for_plot = factor(
        .data$display_cluster,
        levels = seq_len(C)
      )
    )
  
  if (is.null(panel_title)) {
    panel_title <- paste0(year)
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_sf,
      ggplot2::aes(fill = .data$cluster_for_plot),
      colour = "white",
      linewidth = 0.12
    ) +
    ggplot2::geom_sf(
      data = region_sf,
      fill = NA,
      colour = "grey25",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = cluster_palette[as.character(seq_len(C))],
      limits = as.character(seq_len(C)),
      drop = FALSE,
      na.value = "grey90",
      name = "Cluster"
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = panel_title
    ) +
    theme_cluster_map(base_size = 8.5) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

plot_instability_map_transition <- function(
    year_previous,
    year_current,
    C = 4L,
    show_legend = TRUE
) {
  inst <- province_instability |>
    dplyr::filter(
      .data$n_clusters == .env$C,
      .data$year_previous == .env$year_previous,
      .data$year_current == .env$year_current
    ) |>
    dplyr::select(
      "territory",
      "coassignment_instability"
    )
  
  map_sf <- province_sf |>
    dplyr::left_join(
      inst,
      by = c("province_harmonized" = "territory"),
      relationship = "one-to-one"
    )
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_sf,
      ggplot2::aes(fill = .data$coassignment_instability),
      colour = "white",
      linewidth = 0.12
    ) +
    ggplot2::geom_sf(
      data = region_sf,
      fill = NA,
      colour = "grey25",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#08306B",
      limits = c(0, 1),
      na.value = "grey90",
      name = "Coassignment\ninstability"
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = "Local instability"
    ) +
    theme_cluster_map(base_size = 8.5) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

# 4. GENERATE ALL election x geometry x C CLUSTER MAPS
# -----------------------------------------------------------------------------

map_combinations <- assignments_aligned |>
  dplyr::distinct(
    .data$year,
    .data$geometry,
    .data$n_clusters
  ) |>
  dplyr::arrange(
    .data$n_clusters,
    match(.data$geometry, geometry_order),
    .data$year
  )

message(
  "Generating ",
  nrow(map_combinations),
  " cluster maps (each as PDF and PNG)."
)

for (C in C_values) {
  for (g in available_geometries) {
    
    years_here <- map_combinations |>
      dplyr::filter(
        .data$n_clusters == .env$C,
        .data$geometry == .env$g
      ) |>
      dplyr::pull(.data$year)
    
    if (length(years_here) == 0L) next
    
    out_dir <- file.path(
      cluster_map_root,
      paste0("C", C),
      g
    )
    
    message(
      "C=", C,
      " | ", g,
      " | ", length(years_here), " elections"
    )
    
    for (yr in years_here) {
      p <- plot_cluster_map(yr, g, C)
      
      save_map(
        p,
        paste0(
          "map_clusters_",
          yr,
          "_",
          g,
          "_C",
          C
        ),
        out_dir
      )
    }
  }
}

readr::write_csv(
  map_combinations,
  file.path(results_table_dir, "map_cluster_figure_manifest.csv")
)

# -----------------------------------------------------------------------------
# 5. PROVINCE-LEVEL MEAN INSTABILITY MAPS FOR EVERY C
# -----------------------------------------------------------------------------

instability_mean <- province_instability |>
  dplyr::group_by(
    .data$n_clusters,
    .data$territory
  ) |>
  dplyr::summarise(
    mean_instability = mean(
      .data$coassignment_instability,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

for (C in sort(unique(instability_mean$n_clusters))) {
  
  map_sf <- province_sf |>
    dplyr::left_join(
      instability_mean |>
        dplyr::filter(.data$n_clusters == .env$C) |>
        dplyr::select(
          "territory",
          "mean_instability"
        ),
      by = c("province_harmonized" = "territory"),
      relationship = "one-to-one"
    )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_sf,
      ggplot2::aes(fill = .data$mean_instability),
      colour = "white",
      linewidth = 0.12
    ) +
    ggplot2::geom_sf(
      data = region_sf,
      fill = NA,
      colour = "grey25",
      linewidth = 0.35,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "#08306B",
      limits = c(0, 1),
      na.value = "grey90",
      name = "Mean instability"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth = grid::unit(4.0, "cm"),
        barheight = grid::unit(0.35, "cm")
      )
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = paste0(
        "Mean province-level coassignment instability - C = ",
        C
      ),
      subtitle = "Average across adjacent-election transitions"
    ) +
    theme_cluster_map() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        hjust = 0.5
      )
    )
  
  save_map(
    p,
    paste0("map_mean_province_instability_C", C),
    instability_map_root
  )
}


# -----------------------------------------------------------------------------
# 6. MAIN-READY PREFERRED-GEOMETRY C=4 MAP FOR EVERY ELECTION
# -----------------------------------------------------------------------------

C_interpret <- 4L

for (yr in election_years) {
  p <- plot_cluster_map_clean(
    year = yr,
    geometry = preferred_geometry,
    C = C_interpret,
    panel_title = paste0(yr, " - preferred geometry"),
    show_legend = TRUE
  )
  
  save_map(
    p,
    paste0(
      "fig_results_map_preferred_C4_",
      yr
    ),
    main_ready_root,
    width = 5.2,
    height = 6.1
  )
}

# -----------------------------------------------------------------------------
# 7. C=4 CROSS-GEOMETRY COMPARISON FOR EVERY ELECTION
#    Preferred vs classical Aitchison vs Euclidean shares.
#
# Alternative-geometry labels have already been matched by maximum overlap
# to the preferred partition within the same election. Therefore equal colours
# have the intended visual interpretation: they identify maximally overlapping
# clusters, not native k-means label numbers.
# -----------------------------------------------------------------------------

comparison_geometries <- c(
  preferred_geometry,
  "aitchison",
  "euclidean"
)

geometry_panel_titles <- c(
  greenacre_weighted_power_0175 = "Preferred GW power",
  aitchison = "Aitchison",
  euclidean = "Euclidean shares"
)

geometry_reassignments <- list()

for (yr in election_years) {
  
  panel_list <- lapply(
    comparison_geometries,
    function(g) {
      plot_cluster_map_clean(
        year = yr,
        geometry = g,
        C = C_interpret,
        panel_title = unname(geometry_panel_titles[[g]]),
        show_legend = FALSE
      )
    }
  )
  
  composite <- patchwork::wrap_plots(
    panel_list,
    nrow = 1,
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = paste0(
        yr,
        " - C = ",
        C_interpret,
        " partition under alternative electoral geometries"
      ),
      subtitle = "Colours are matched by maximum provincial overlap to the preferred partition"
    ) &
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  save_map(
    composite,
    paste0(
      "fig_results_cross_geometry_C4_",
      yr
    ),
    geometry_comparison_root,
    width = 10.2,
    height = 4.4
  )
  
  # Province-level reassignment table after display-label alignment.
  ref <- assignments_aligned |>
    dplyr::filter(
      .data$year == .env$yr,
      .data$geometry == preferred_geometry,
      .data$n_clusters == C_interpret
    ) |>
    dplyr::select(
      "territory",
      "display_cluster"
    ) |>
    dplyr::rename(
      preferred_display_cluster = display_cluster
    )
  
  for (g in c("aitchison", "euclidean")) {
    alt <- assignments_aligned |>
      dplyr::filter(
        .data$year == .env$yr,
        .data$geometry == .env$g,
        .data$n_clusters == C_interpret
      ) |>
      dplyr::select(
        "territory",
        "display_cluster"
      ) |>
      dplyr::rename(
        alternative_display_cluster = display_cluster
      )
    
    tmp <- dplyr::inner_join(
      ref,
      alt,
      by = "territory"
    ) |>
      dplyr::mutate(
        year = yr,
        comparison_geometry = g,
        changed_display_cluster =
          .data$preferred_display_cluster !=
          .data$alternative_display_cluster
      )
    
    geometry_reassignments[[length(geometry_reassignments) + 1L]] <- tmp
  }
}

geometry_reassignments <- dplyr::bind_rows(
  geometry_reassignments
) |>
  dplyr::arrange(
    .data$year,
    .data$comparison_geometry,
    dplyr::desc(.data$changed_display_cluster),
    .data$territory
  )

readr::write_csv(
  geometry_reassignments,
  file.path(
    results_table_dir,
    "map_C4_geometry_reassignments.csv"
  )
)

geometry_reassignment_summary <- geometry_reassignments |>
  dplyr::group_by(
    .data$year,
    .data$comparison_geometry
  ) |>
  dplyr::summarise(
    n_provinces = dplyr::n(),
    n_changed = sum(.data$changed_display_cluster),
    share_changed = mean(.data$changed_display_cluster),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    geometry_stability |>
      dplyr::filter(
        .data$n_clusters == C_interpret,
        .data$reference_geometry == preferred_geometry,
        .data$comparison_geometry %in% c(
          "aitchison",
          "euclidean"
        )
      ) |>
      dplyr::select(
        "year",
        "comparison_geometry",
        "adjusted_rand",
        "normalized_mutual_information",
        "variation_of_information",
        "coassignment_pearson"
      ),
    by = c(
      "year",
      "comparison_geometry"
    )
  )

readr::write_csv(
  geometry_reassignment_summary,
  file.path(
    results_table_dir,
    "map_C4_geometry_reassignment_summary.csv"
  )
)

# -----------------------------------------------------------------------------
# 8. C=4 ADJACENT-ELECTION MAP PAIRS AND TRANSITION STORIES FOR EVERY TRANSITION
#
# Preferred-geometry colours are sequentially maximum-overlap aligned through
# time. Each story contains previous partition, current partition, and a
# label-invariant province-level coassignment-instability map.
# -----------------------------------------------------------------------------

transition_C4 <- temporal_stability |>
  dplyr::filter(.data$n_clusters == C_interpret) |>
  dplyr::arrange(
    .data$year_previous,
    .data$year_current
  )

transition_change_records <- list()

for (i in seq_len(nrow(transition_C4))) {
  
  yp <- transition_C4$year_previous[[i]]
  yc <- transition_C4$year_current[[i]]
  ari_here <- transition_C4$adjusted_rand[[i]]
  
  p_prev <- plot_cluster_map_clean(
    year = yp,
    geometry = preferred_geometry,
    C = C_interpret,
    panel_title = as.character(yp),
    show_legend = FALSE
  )
  
  p_curr <- plot_cluster_map_clean(
    year = yc,
    geometry = preferred_geometry,
    C = C_interpret,
    panel_title = as.character(yc),
    show_legend = FALSE
  )
  
  pair <- patchwork::wrap_plots(
    p_prev,
    p_curr,
    nrow = 1,
    guides = "collect"
  ) +
    patchwork::plot_annotation(
      title = paste0(
        yp,
        " -> ",
        yc,
        " - preferred geometry, C = ",
        C_interpret
      ),
      subtitle = paste0(
        "Adjacent-election ARI = ",
        sprintf("%.3f", ari_here),
        "; colours are maximum-overlap aligned through time"
      )
    ) &
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  save_map(
    pair,
    paste0(
      "fig_results_transition_pair_C4_",
      yp,
      "_",
      yc
    ),
    transition_pair_root,
    width = 7.2,
    height = 4.7
  )
  
  p_inst <- plot_instability_map_transition(
    year_previous = yp,
    year_current = yc,
    C = C_interpret,
    show_legend = TRUE
  )
  
  story <- patchwork::wrap_plots(
    p_prev,
    p_curr,
    p_inst,
    nrow = 1,
    widths = c(1, 1, 1.08)
  ) +
    patchwork::plot_annotation(
      title = paste0(
        yp,
        " -> ",
        yc,
        " - territorial reconfiguration at C = ",
        C_interpret
      ),
      subtitle = paste0(
        "ARI = ",
        sprintf("%.3f", ari_here),
        "; the third panel is label-invariant coassignment instability"
      )
    )
  
  save_map(
    story,
    paste0(
      "fig_results_transition_story_C4_",
      yp,
      "_",
      yc
    ),
    transition_story_root,
    width = 10.8,
    height = 4.6
  )
  
  prev_assign <- assignments_aligned |>
    dplyr::filter(
      .data$year == .env$yp,
      .data$geometry == preferred_geometry,
      .data$n_clusters == C_interpret
    ) |>
    dplyr::select(
      "territory",
      previous_display_cluster = display_cluster
    )
  
  curr_assign <- assignments_aligned |>
    dplyr::filter(
      .data$year == .env$yc,
      .data$geometry == preferred_geometry,
      .data$n_clusters == C_interpret
    ) |>
    dplyr::select(
      "territory",
      current_display_cluster = display_cluster
    )
  
  inst_here <- province_instability |>
    dplyr::filter(
      .data$n_clusters == C_interpret,
      .data$year_previous == .env$yp,
      .data$year_current == .env$yc
    ) |>
    dplyr::select(
      "territory",
      "coassignment_instability"
    )
  
  transition_change_records[[i]] <- prev_assign |>
    dplyr::inner_join(
      curr_assign,
      by = "territory"
    ) |>
    dplyr::left_join(
      inst_here,
      by = "territory"
    ) |>
    dplyr::mutate(
      year_previous = yp,
      year_current = yc,
      adjusted_rand = ari_here,
      changed_display_cluster =
        .data$previous_display_cluster !=
        .data$current_display_cluster
    )
}

transition_change_records <- dplyr::bind_rows(
  transition_change_records
) |>
  dplyr::arrange(
    .data$year_previous,
    .data$year_current,
    dplyr::desc(.data$coassignment_instability),
    .data$territory
  )

readr::write_csv(
  transition_change_records,
  file.path(
    results_table_dir,
    "map_C4_transition_province_changes.csv"
  )
)

# -----------------------------------------------------------------------------
# 9. AUTOMATIC LOW / TYPICAL / HIGH C=4 TRANSITION SELECTION TABLE
# -----------------------------------------------------------------------------

ari_median_C4 <- stats::median(
  transition_C4$adjusted_rand,
  na.rm = TRUE
)

transition_examples_C4 <- dplyr::bind_rows(
  transition_C4 |>
    dplyr::slice_min(
      order_by = .data$adjusted_rand,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::mutate(example_type = "Low stability"),
  transition_C4 |>
    dplyr::slice_min(
      order_by = abs(.data$adjusted_rand - ari_median_C4),
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::mutate(example_type = "Typical stability"),
  transition_C4 |>
    dplyr::slice_max(
      order_by = .data$adjusted_rand,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::mutate(example_type = "High stability")
) |>
  dplyr::distinct(
    .data$year_previous,
    .data$year_current,
    .keep_all = TRUE
  )

readr::write_csv(
  transition_examples_C4,
  file.path(
    results_table_dir,
    "map_C4_transition_examples.csv"
  )
)

writeLines(
  capture.output(sessionInfo()),
  file.path(results_table_dir, "results_map_figures_sessionInfo.txt")
)

message("Map generation complete.")
message("Cluster maps: ", cluster_map_root)
message("Instability maps: ", instability_map_root)
message("C=4 geometry comparison panels: ", geometry_comparison_root)
message("C=4 transition pairs: ", transition_pair_root)
message("C=4 transition stories: ", transition_story_root)
message("Main-ready preferred C=4 maps: ", main_ready_root)
message(
  "Display-label mapping table: ",
  file.path(results_table_dir, "map_cluster_display_label_alignment.csv")
)
