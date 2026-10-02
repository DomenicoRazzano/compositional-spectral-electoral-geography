# 98_results_figures_nonmap.R
# Chapter 6 Results: all figures that do NOT require shapefiles.


source(here::here("06_code", "00_setup.R"))

preferred_geometry <- "greenacre_weighted_power_0175"
illustrative_C <- 4L

interpretation_years <- c(1948L, 1953L, 1958L, 1963L, 1968L, 1972L, 1976L, 1979L, 1983L, 1987L, 1992L, 1994L, 1996L, 2001L, 2006L, 2008L, 2013L, 2018L, 2022L)
mass_scatter_years <- c(1948L, 1976L, 1994L, 2022L)
n_transition_panels <- 5L
n_axis_extremes_table <- 5L
n_unstable_provinces_table <- 15L

stage04_table_dir <- path_outputs_tables_compositional
stage05_table_dir <- file.path(path_outputs_tables, "spectral")

figure_root <- path_outputs_figures
results_nonmap_dir <- file.path(figure_root, "results", "non_map")
results_core_dir <- file.path(results_nonmap_dir, "core")
results_robustness_dir <- file.path(results_nonmap_dir, "robustness")
results_graph_dir <- file.path(results_nonmap_dir, "graph")
results_interpretation_dir <- file.path(results_nonmap_dir, "interpretation")
results_temporal_dir <- file.path(results_nonmap_dir, "temporal")
results_appendix_dir <- file.path(results_nonmap_dir, "appendix")
results_table_dir <- file.path(path_outputs_tables, "results")

fs::dir_create(c(
  results_nonmap_dir,
  results_core_dir,
  results_robustness_dir,
  results_graph_dir,
  results_interpretation_dir,
  results_temporal_dir,
  results_appendix_dir,
  results_table_dir
))

theme_thesis_results <- function(base_size = 9.5) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0.5, size = base_size + 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = base_size - 0.5, colour = "grey25"
      ),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "black"),
      panel.grid.major = ggplot2::element_line(
        colour = "grey86", linewidth = 0.24, linetype = "dotted"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "grey94", colour = "grey55", linewidth = 0.3
      ),
      strip.text = ggplot2::element_text(
        face = "bold", size = base_size - 0.8
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(size = base_size - 0.7),
      legend.text = ggplot2::element_text(size = base_size - 0.9),
      legend.key = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 7, 5.5, 5.5)
    )
}


# Legacy theme used by the two figures that were already in the thesis
theme_thesis <- function(base_size = 9.5) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = base_size + 0.5),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major = ggplot2::element_line(colour = "grey82", linewidth = 0.25, linetype = "dotted"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey94", colour = "grey55", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      plot.margin = ggplot2::margin(5.5, 7, 5.5, 5.5)
    )
}

percent_axis <- function(x, digits = 0) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

wrap_component <- function(x, width = 18) {
  stringr::str_wrap(stringr::str_replace_all(x, "_", " "), width = width)
}

read_required_csv <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

save_thesis_plot <- function(plot, filename, width, height, directory = results_core_dir) {
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

write_result_table <- function(x, filename) {
  readr::write_csv(x, file.path(results_table_dir, filename))
  invisible(x)
}

write_latex_longtable <- function( #Prende un dataframe, arrotonda le colonne numeriche e lo converte direttamente in LaTeX longtable
  x,
  filename,
  caption,
  label,
  digits = 4
) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop(
      "Package 'knitr' is required to export appendix LaTeX tables.",
      call. = FALSE
    )
  }
  
  x_out <- x |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
  
  latex <- knitr::kable(
    x_out,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = TRUE,
    caption = caption,
    label = label
  )
  
  writeLines(
    latex,
    file.path(results_table_dir, filename)
  )
  
  invisible(x_out)
}

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

subcomposition_labels <- c(
  drop_non_valid_or_non_voting = "Drop non-valid / non-voting",
  drop_other_lists = "Drop OTHER LISTS",
  party_and_territorial_without_other = "Party + territorial, no OTHER",
  drop_territorial_lists = "Drop TERRITORIAL LISTS",
  autonomous_parties_only = "Autonomous parties only"
)


# Load outputs

support_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_support_summary.csv")
)
zero_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_final_component_zero_summary.csv")
)
clr_variance_by_component <- read_required_csv(
  file.path(stage04_table_dir, "stage04_clr_variance_by_component.csv")
)
clr_variance_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_clr_variance_weighting_summary.csv")
)
alpha_alignment <- read_required_csv(
  file.path(stage04_table_dir, "stage04_alpha_alignment_profile.csv")
)
alpha_best_spec <- read_required_csv(
  file.path(stage04_table_dir, "stage04_alpha_best_by_spec.csv")
)
greenacre_alignment <- read_required_csv(
  file.path(stage04_table_dir, "stage04_greenacre_power_alignment_profile.csv")
)
greenacre_best_spec <- read_required_csv(
  file.path(stage04_table_dir, "stage04_greenacre_power_best_by_spec.csv")
)
geometry_comparison_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_full_pairwise_geometry_comparison_summary.csv")
)
zero_replacement <- read_required_csv(
  file.path(stage04_table_dir, "stage04_zero_replacement_sensitivity.csv")
)
zero_metric_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_zero_metric_diagnostics_final_objects_summary.csv")
)
subcomposition_summary <- read_required_csv(
  file.path(stage04_table_dir, "stage04_subcomposition_stability_summary.csv")
)

component_classification <- read_required_csv(
  file.path(stage04_table_dir, "stage04_component_final_classification.csv")
)
final_component_zeros <- read_required_csv(
  file.path(stage04_table_dir, "stage04_final_component_zeros.csv")
)
component_contributions <- read_required_csv(
  file.path(stage04_table_dir, "stage04_component_contributions_by_geometry.csv")
)
geometry_comparison_full <- read_required_csv(
  file.path(stage04_table_dir, "stage04_full_pairwise_geometry_comparison.csv")
)
zero_metric_full <- read_required_csv(
  file.path(stage04_table_dir, "stage04_zero_metric_diagnostics_final_objects.csv")
)
subcomposition_full <- read_required_csv(
  file.path(stage04_table_dir, "stage04_subcomposition_stability.csv")
)

affinity_diagnostics <- read_required_csv(
  file.path(stage05_table_dir, "stage05_affinity_diagnostics.csv")
)
laplacian_eigenvalues <- read_required_csv(
  file.path(stage05_table_dir, "stage05_laplacian_eigenvalues.csv")
)
cluster_quality <- read_required_csv(
  file.path(stage05_table_dir, "stage05_fixedC_cluster_quality_main_spec.csv")
)
axis_correlations <- read_required_csv(
  file.path(stage05_table_dir, "stage05_axis_component_correlations_main_spec.csv")
)
axis_extremes <- read_required_csv(
  file.path(stage05_table_dir, "stage05_axis_province_extremes_main_spec.csv")
)
cluster_component_profiles <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_component_profiles_main_spec.csv")
)
cluster_region_profiles <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_region_profiles_main_spec.csv")
)
geometry_stability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_geometry_stability.csv")
)
temporal_stability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_cluster_temporal_stability_main_spec.csv")
)
transition_matrices <- read_required_csv(
  file.path(stage05_table_dir, "stage05_transition_matrices_main_spec.csv")
)
province_instability <- read_required_csv(
  file.path(stage05_table_dir, "stage05_province_instability_main_spec.csv")
)

#integrity check sugli anni
election_years <- sort(unique(support_summary$year))
stopifnot(length(election_years) == 19L)
stopifnot(all(interpretation_years %in% election_years))
stopifnot(all(mass_scatter_years %in% election_years))


# Compact tables used in Results

selected_power_table <- dplyr::bind_rows(
  alpha_best_spec |>
    dplyr::transmute(
      family = "Tsagris--Preston--Wood alpha",
      benchmark = "Aitchison",
      selected_parameter = .data$alpha,
      n_elections = .data$n_elections,
      median_spearman = .data$spearman_median,
      median_pearson = .data$pearson_median
    ),
  greenacre_best_spec |>
    dplyr::transmute(
      family = dplyr::case_when(
        .data$family == "greenacre_unweighted_power" ~ "Greenacre unweighted power",
        .data$family == "greenacre_weighted_power" ~ "Greenacre weighted power",
        TRUE ~ as.character(.data$family)
      ),
      benchmark = dplyr::case_when(
        .data$benchmark == "aitchison" ~ "Aitchison",
        .data$benchmark == "greenacre_weighted_lra" ~ "Weighted LRA",
        TRUE ~ as.character(.data$benchmark)
      ),
      selected_parameter = .data$lambda,
      n_elections = .data$n_elections,
      median_spearman = .data$spearman_median,
      median_pearson = .data$pearson_median
    )
)
write_result_table(selected_power_table, "table_results_selected_power_specifications.csv")

# election specific table for preferred geometry
preferred_affinity_table <- affinity_diagnostics |>
  dplyr::filter(.data$is_preferred_geometry) |>
  dplyr::select(
    "election_date", "year", "n_nodes", "neighborhood_m",
    "sigma_median", "sigma_cv", "weighted_degree_mean",
    "weighted_degree_cv", "effective_neighbors_median"
  ) |>
  dplyr::arrange(.data$year)
write_result_table(
  preferred_affinity_table,
  "table_results_affinity_diagnostics_preferred.csv"
)

# which provinces lie at the poles of the spectral coordinates?
axis_extremes_selected_table <- axis_extremes |>
  dplyr::filter(
    .data$year %in% interpretation_years,
    .data$side_rank <= n_axis_extremes_table
  ) |>
  dplyr::arrange(.data$year, .data$eigen_index, .data$side, .data$side_rank)
write_result_table(
  axis_extremes_selected_table,
  "table_results_axis_extremes_selected.csv"
)

province_instability_C <- province_instability |>
  dplyr::filter(.data$n_clusters == illustrative_C) |>
  dplyr::group_by(.data$territory) |>
  dplyr::summarise(
    n_transitions = dplyr::n(),
    mean_instability = mean(.data$coassignment_instability, na.rm = TRUE),
    median_instability = stats::median(.data$coassignment_instability, na.rm = TRUE),
    max_instability = max(.data$coassignment_instability, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(.data$mean_instability),
    dplyr::desc(.data$max_instability)
  )

write_result_table(
  province_instability_C |> dplyr::slice_head(n = n_unstable_provinces_table),
  paste0("table_results_most_unstable_provinces_C", illustrative_C, ".csv")
)


# RESULTS FIGURES


# FIGURE 1 — ELECTORAL SUPPORT DIMENSIONALITY
message("Preparing Figure 1: support dimensionality.")

support_dimension_long <- support_summary |>
  dplyr::transmute(
    year = .data$year,
    raw_list_components = .data$n_components_raw,
    final_compositional_components = .data$n_final_components_from_lists + 1L
  ) |>
  tidyr::pivot_longer(
    cols = c("raw_list_components", "final_compositional_components"),
    names_to = "series",
    values_to = "n_components"
  ) |>
  dplyr::mutate(
    series = factor(
      .data$series,
      levels = c("raw_list_components", "final_compositional_components"),
      labels = c("Raw list components", "Final compositional components")
    )
  )

fig_method_support_dimensionality <- ggplot2::ggplot(
  support_dimension_long,
  ggplot2::aes(
    x = .data$year,
    y = .data$n_components,
    group = .data$series,
    linetype = .data$series,
    shape = .data$series,
    colour = .data$series
  )
) +
  ggplot2::geom_line(linewidth = 0.55) +
  ggplot2::geom_point(size = 1.8, stroke = 0.55) +
  ggplot2::scale_x_continuous(breaks = election_years) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed")) +
  ggplot2::scale_shape_manual(values = c(16, 15)) +
  ggplot2::scale_colour_manual(values = c("black", "grey55")) +
  ggplot2::labs(
    title = "Electoral support dimensionality by election year",
    x = NULL,
    y = "Number of components"
  ) +
  theme_thesis()

save_thesis_plot(
  fig_method_support_dimensionality,
  "fig_results_01_support_dimensionality",
  6.7, 3.7
)


# FIGURE 2 — RESIDUAL SUPPORT SHARES

message("Preparing Figure 2: residual support shares.")

residual_support_long <- support_summary |>
  dplyr::select(
    "year",
    "other_vote_share_in_spec",
    "territorial_vote_share_in_spec"
  ) |>
  tidyr::pivot_longer(
    cols = c("other_vote_share_in_spec", "territorial_vote_share_in_spec"),
    names_to = "component",
    values_to = "vote_share"
  ) |>
  dplyr::mutate(
    component = factor(
      .data$component,
      levels = c("other_vote_share_in_spec", "territorial_vote_share_in_spec"),
      labels = c("OTHER_LISTS", "TERRITORIAL_LISTS")
    ),
    year = factor(.data$year, levels = election_years)
  )

fig_method_residual_support_shares <- ggplot2::ggplot(
  residual_support_long,
  ggplot2::aes(
    x = .data$year,
    y = .data$vote_share,
    fill = .data$component
  )
) +
  ggplot2::geom_col(width = 0.72, colour = "grey30", linewidth = 0.25) +
  ggplot2::scale_fill_manual(
    values = c("OTHER_LISTS" = "grey72", "TERRITORIAL_LISTS" = "grey20")
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf("%.0f", 100 * x),
    expand = ggplot2::expansion(mult = c(0, 0.04))
  ) +
  ggplot2::labs(
    title = "Vote shares assigned to residual support components",
    x = NULL,
    y = "Share of national valid votes (%)"
  ) +
  theme_thesis()

save_thesis_plot(
  fig_method_residual_support_shares,
  "fig_results_02_residual_support_shares",
  6.7, 3.9
)



# FIGURES 3a-b — ZERO STRUCTURE


message("Figures: zero structure.")

zero_pct <- zero_summary |>
  dplyr::left_join(
    preferred_affinity_table |>
      dplyr::select(year, n_nodes),
    by = "year"
  ) |>
  dplyr::mutate(
    total_cells = .data$n_nodes * .data$n_final_components,
    zero_share = dplyr::if_else(
      .data$total_cells > 0,
      .data$total_zero_cells / .data$total_cells,
      NA_real_
    )
  )

if (any(is.na(zero_pct$n_nodes))) {
  stop(
    "Could not recover the number of provinces for every election.",
    call. = FALSE
  )
}

fig_zero_cells <- ggplot2::ggplot(
  zero_pct,
  ggplot2::aes(
    x = .data$year,
    y = .data$zero_share
  )
) +
  ggplot2::geom_line(linewidth = 0.65, colour = "black") +
  ggplot2::geom_point(size = 1.8, colour = "black") +
  ggplot2::scale_x_continuous(breaks = election_years) +
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf("%.0f%%", 100 * x),
    limits = c(0, NA)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Zero-cell share"
  ) +
  theme_thesis_results() +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

save_thesis_plot(
  fig_zero_cells,
  "fig_results_03a_zero_cells",
  6.4, 3.6,
  results_core_dir
)

zero_class_data <- final_component_zeros |>
  dplyr::mutate(
    component_class = dplyr::case_when(
      .data$component == "TERRITORIAL_LISTS" ~ "Territorial lists",
      .data$component == "OTHER_LISTS" ~ "Other lists",
      .data$component == "NON_VALID_OR_NON_VOTING" ~ "Non-party electorate",
      TRUE ~ "Autonomous lists"
    )
  ) |>
  dplyr::filter(.data$component_class != "Non-party electorate") |>
  dplyr::group_by(.data$year, .data$component_class) |>
  dplyr::summarise(
    zero_count = sum(.data$zero_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(.data$year) |>
  dplyr::mutate(
    total_zero = sum(.data$zero_count),
    zero_fraction = dplyr::if_else(
      .data$total_zero > 0,
      .data$zero_count / .data$total_zero,
      NA_real_
    )
  ) |>
  dplyr::ungroup()

fig_zero_composition <- ggplot2::ggplot(
  zero_class_data |>
    dplyr::filter(!is.na(.data$zero_fraction)),
  ggplot2::aes(
    x = factor(.data$year, levels = election_years),
    y = .data$zero_fraction,
    fill = .data$component_class
  )
) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::scale_fill_grey(
    start = 0.25,
    end = 0.85,
    name = NULL
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf("%.0f%%", 100 * x),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Share of zero cells"
  ) +
  theme_thesis_results(base_size = 8.5) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    ),
    legend.position = "top"
  )

save_thesis_plot(
  fig_zero_composition,
  "fig_results_03b_zero_composition",
  6.4, 3.8,
  results_core_dir
)

# 11. FIGURES 6.4a-b — COMPONENT MASS AND CLR VARIABILITY

message("Figures: component mass and clr variability.")

clr_correlation_long <- clr_variance_summary |>
  dplyr::select(
    "year",
    "spearman_logmean_logvar",
    "pearson_logmean_logvar"
  ) |>
  tidyr::pivot_longer(
    cols = c(
      "spearman_logmean_logvar",
      "pearson_logmean_logvar"
    ),
    names_to = "correlation",
    values_to = "value"
  ) |>
  dplyr::mutate(
    correlation = factor(
      .data$correlation,
      levels = c(
        "spearman_logmean_logvar",
        "pearson_logmean_logvar"
      ),
      labels = c("Spearman", "Pearson")
    )
  )

fig_mass_variance_correlation <- ggplot2::ggplot(
  clr_correlation_long,
  ggplot2::aes(
    x = .data$year,
    y = .data$value,
    linetype = .data$correlation,
    shape = .data$correlation,
    group = .data$correlation
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "grey45"
  ) +
  ggplot2::geom_line(linewidth = 0.65, colour = "black") +
  ggplot2::geom_point(size = 1.8, colour = "black") +
  ggplot2::scale_x_continuous(breaks = election_years) +
  ggplot2::scale_y_continuous(limits = c(-1, 1)) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed")) +
  ggplot2::scale_shape_manual(values = c(16, 1)) +
  ggplot2::labs(
    x = NULL,
    y = "Correlation",
    linetype = NULL,
    shape = NULL
  ) +
  theme_thesis_results() +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    )
  )

save_thesis_plot(
  fig_mass_variance_correlation,
  "fig_results_04a_mass_clr_variance_correlation",
  4.0, 3.5,
  results_core_dir
)

clr_selected <- clr_variance_by_component |>
  dplyr::filter(
    .data$year %in% mass_scatter_years,
    is.finite(.data$log_mean_proportion),
    is.finite(.data$log_clr_variance)
  ) |>
  dplyr::mutate(
    year_factor = factor(
      .data$year,
      levels = mass_scatter_years
    )
  )

fig_mass_variance_scatter <- ggplot2::ggplot(
  clr_selected,
  ggplot2::aes(
    x = .data$log_mean_proportion,
    y = .data$log_clr_variance
  )
) +
  ggplot2::geom_point(
    size = 1.55,
    alpha = 0.75,
    colour = "grey30"
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.55,
    colour = "black"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(year_factor),
    nrow = 2
  ) +
  ggplot2::labs(
    x = expression(log(c[tj])),
    y = expression(log(hat(gamma)[jj*",t"]))
  ) +
  theme_thesis_results(base_size = 8.3) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    legend.position = "none"
  )

save_thesis_plot(
  fig_mass_variance_scatter,
  "fig_results_04b_mass_clr_variance_selected",
  4.2, 3.5,
  results_core_dir
)

clr_all <- clr_variance_by_component |>
  dplyr::filter(
    is.finite(.data$log_mean_proportion),
    is.finite(.data$log_clr_variance)
  ) |>
  dplyr::mutate(
    year_factor = factor(
      .data$year,
      levels = election_years
    )
  )

fig_mass_variance_all <- ggplot2::ggplot(
  clr_all,
  ggplot2::aes(
    x = .data$log_mean_proportion,
    y = .data$log_clr_variance
  )
) +
  ggplot2::geom_point(
    size = 1.05,
    alpha = 0.70,
    colour = "grey35"
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.45,
    colour = "black"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(year_factor),
    ncol = 5
  ) +
  ggplot2::labs(
    x = expression(log(c[tj])),
    y = expression(log(hat(gamma)[jj*",t"]))
  ) +
  theme_thesis_results(base_size = 7.3) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    legend.position = "none"
  )

save_thesis_plot(
  fig_mass_variance_all,
  "fig_appendix_clr_mass_variance_all_elections",
  8.0, 8.2,
  results_appendix_dir
)

# FIGURE 6.5 — POWER-PARAMETER CALIBRATION

message("Figure: power-parameter calibration.")

alpha_profile_plot <- alpha_alignment |>
  dplyr::group_by(.data$alpha) |>
  dplyr::summarise(
    parameter = dplyr::first(.data$alpha),
    spearman_median = stats::median(
      .data$distance_spearman_cor,
      na.rm = TRUE
    ),
    spearman_q25 = stats::quantile(
      .data$distance_spearman_cor,
      0.25,
      na.rm = TRUE
    ),
    spearman_q75 = stats::quantile(
      .data$distance_spearman_cor,
      0.75,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

greenacre_profile_plot <- greenacre_alignment |>
  dplyr::group_by(.data$family, .data$lambda) |>
  dplyr::summarise(
    parameter = dplyr::first(.data$lambda),
    spearman_median = stats::median(
      .data$distance_spearman_cor,
      na.rm = TRUE
    ),
    spearman_q25 = stats::quantile(
      .data$distance_spearman_cor,
      0.25,
      na.rm = TRUE
    ),
    spearman_q75 = stats::quantile(
      .data$distance_spearman_cor,
      0.75,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

gu_profile_plot <- greenacre_profile_plot |>
  dplyr::filter(.data$family == "greenacre_unweighted_power")

gw_profile_plot <- greenacre_profile_plot |>
  dplyr::filter(.data$family == "greenacre_weighted_power")

make_power_panel <- function(
    dat,
    panel_title,
    selected_value,
    panel_breaks,
    show_y = FALSE
) {
  selected_dat <- dat |>
    dplyr::slice_min(
      order_by = abs(.data$parameter - selected_value),
      n = 1,
      with_ties = FALSE
    )
  
  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$parameter,
      y = .data$spearman_median
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$spearman_q25,
        ymax = .data$spearman_q75
      ),
      fill = "grey80",
      alpha = 0.70,
      colour = NA
    ) +
    ggplot2::geom_line(
      linewidth = 0.65,
      colour = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = selected_value,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "grey30"
    ) +
    ggplot2::geom_point(
      data = selected_dat,
      shape = 21,
      size = 2.3,
      stroke = 0.7,
      fill = "white",
      colour = "black"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.001, 1),
      breaks = panel_breaks,
      labels = function(x) sprintf("%.3f", x)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00)
    ) +
    ggplot2::labs(
      title = panel_title,
      x = NULL,
      y = if (show_y) "Median Spearman correlation" else NULL
    ) +
    theme_thesis_results(base_size = 8.3) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = 45, hjust = 1
      ),
      axis.text.y = if (show_y) {
        ggplot2::element_text(
          size = 7.3,
          colour = "black"
        )
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.y = if (show_y) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      }
    )
}

p_alpha <- make_power_panel(
  alpha_profile_plot,
  "TPW alpha vs Aitchison",
  0.150,
  c(0.001, 0.150, 0.250, 0.500, 0.750, 1.000),
  show_y = TRUE
)

p_gu <- make_power_panel(
  gu_profile_plot,
  "GU power vs Aitchison",
  0.150,
  c(0.001, 0.150, 0.250, 0.500, 0.750, 1.000)
)

p_gw <- make_power_panel(
  gw_profile_plot,
  "GW power vs weighted LRA",
  0.175,
  c(0.001, 0.175, 0.250, 0.500, 0.750, 1.000)
)

fig_power_alignment <- patchwork::wrap_plots(
  p_alpha,
  p_gu,
  p_gw,
  nrow = 1
) +
  patchwork::plot_annotation(
    title = "Positive-power alignment with log-ratio benchmarks"
  )

save_thesis_plot(
  fig_power_alignment,
  "fig_results_05_power_alignment",
  7.4, 3.6,
  results_core_dir
)

# FIGURE 6.6 — PAIRWISE GEOMETRY AGREEMENT

message("Figure: pairwise geometry agreement.")

geometry_pairs_clean <- geometry_comparison_summary |>
  dplyr::filter(
    .data$geometry_a %in% geometry_order,
    .data$geometry_b %in% geometry_order
  ) |>
  dplyr::mutate(
    idx_a = match(.data$geometry_a, geometry_order),
    idx_b = match(.data$geometry_b, geometry_order),
    row_idx = pmax(.data$idx_a, .data$idx_b),
    col_idx = pmin(.data$idx_a, .data$idx_b)
  ) |>
  dplyr::group_by(.data$row_idx, .data$col_idx) |>
  dplyr::summarise(
    spearman_median = stats::median(
      .data$spearman_median,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    geometry_x = geometry_order[.data$col_idx],
    geometry_y = geometry_order[.data$row_idx]
  )

geometry_diag <- tibble::tibble(
  geometry_x = geometry_order,
  geometry_y = geometry_order,
  spearman_median = 1
)

geometry_heat <- dplyr::bind_rows(
  geometry_pairs_clean |>
    dplyr::select(
      geometry_x,
      geometry_y,
      spearman_median
    ),
  geometry_diag
) |>
  dplyr::distinct(
    .data$geometry_x,
    .data$geometry_y,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    geometry_x = factor(
      .data$geometry_x,
      levels = geometry_order,
      labels = unname(geometry_labels[geometry_order])
    ),
    geometry_y = factor(
      .data$geometry_y,
      levels = geometry_order,
      labels = unname(geometry_labels[geometry_order])
    ),
    text_colour = dplyr::if_else(
      .data$spearman_median >= 0.72,
      "white",
      "black"
    )
  )

fig_geometry_heatmap <- ggplot2::ggplot(
  geometry_heat,
  ggplot2::aes(
    x = .data$geometry_x,
    y = .data$geometry_y,
    fill = .data$spearman_median
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.45
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.2f", .data$spearman_median),
      colour = .data$text_colour
    ),
    size = 2.9
  ) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_fill_gradient(
    low = "white",
    high = "grey15",
    limits = c(0, 1),
    name = "Median\nSpearman"
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.5) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top",
    legend.title = ggplot2::element_text(
      margin = ggplot2::margin(r = 10)
    )
  )

save_thesis_plot(
  fig_geometry_heatmap,
  "fig_results_06_geometry_agreement_heatmap",
  6.5, 5.2,
  results_core_dir
)

# FIGURE 6.7 — ZERO-REPLACEMENT ROBUSTNESS

message("Figure: zero-replacement robustness.")

zero_robust_plot <- zero_replacement |>
  dplyr::filter(
    .data$comparison == "against_baseline_fixed_count_10"
  ) |>
  dplyr::group_by(
    .data$metric,
    .data$replacement_type,
    .data$replacement_value
  ) |>
  dplyr::summarise(
    spearman_median = stats::median(
      .data$spearman_cor,
      na.rm = TRUE
    ),
    spearman_q25 = stats::quantile(
      .data$spearman_cor,
      0.25,
      na.rm = TRUE
    ),
    spearman_q75 = stats::quantile(
      .data$spearman_cor,
      0.75,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    metric_label = factor(
      dplyr::case_when(
        .data$metric == "aitchison" ~ "Aitchison",
        .data$metric == "greenacre_weighted_lra" ~ "Weighted LRA",
        TRUE ~ as.character(.data$metric)
      ),
      levels = c("Aitchison", "Weighted LRA")
    )
  )

fixed_plot_data <- zero_robust_plot |>
  dplyr::filter(.data$replacement_type == "fixed_count") |>
  dplyr::mutate(
    x_label = factor(
      format(
        .data$replacement_value,
        scientific = FALSE,
        trim = TRUE
      ),
      levels = format(
        sort(unique(.data$replacement_value)),
        scientific = FALSE,
        trim = TRUE
      )
    )
  )

floor_values <- zero_robust_plot |>
  dplyr::filter(.data$replacement_type == "share_floor") |>
  dplyr::pull(.data$replacement_value) |>
  unique() |>
  sort()


floor_plot_data <- zero_robust_plot |>
  dplyr::filter(.data$replacement_type == "share_floor") |>
  dplyr::mutate(
    x_label = factor(
      .data$replacement_value,
      levels = floor_values
    )
  )

pd <- ggplot2::position_dodge(width = 0.35)

make_zero_panel <- function(
    dat,
    panel_title,
    x_scale = NULL,
    show_y = FALSE
) {
  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$x_label,
      y = .data$spearman_median,
      shape = .data$metric_label,
      group = .data$metric_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linewidth = 0.35,
      colour = "grey55"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin = .data$spearman_q25,
        ymax = .data$spearman_q75
      ),
      position = pd,
      linewidth = 0.65,
      colour = "grey35"
    ) +
    ggplot2::geom_point(
      position = pd,
      size = 2.2,
      colour = "black"
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Aitchison" = 16,
        "Weighted LRA" = 17
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.90, 1.005)
    ) +
    ggplot2::labs(
      title = panel_title,
      x = NULL,
      y = if (show_y) "Spearman correlation" else NULL
    ) +
    theme_thesis_results(base_size = 8.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 0,
        hjust = 0.5
      ),
      axis.text.y = if (show_y) {
        ggplot2::element_text(colour = "black")
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.y = if (show_y) {
        ggplot2::element_line()
      } else {
        ggplot2::element_blank()
      }
    )
  
  if (!is.null(x_scale)) {
    p <- p + x_scale
  }
  
  p
}

p_fixed <- make_zero_panel(
  fixed_plot_data,
  "Fixed pseudo-count",
  show_y = TRUE
)

p_floor <- make_zero_panel(
  floor_plot_data,
  "Share floor",
  x_scale = ggplot2::scale_x_discrete(
    labels = c(
      expression(10^{-6}),
      expression(5 %*% 10^{-6}),
      expression(10^{-5}),
      expression(5 %*% 10^{-5})
    )
  )
)

fig_zero_robustness <- patchwork::wrap_plots(
  p_fixed,
  p_floor,
  nrow = 1,
  guides = "collect"
) &
  ggplot2::theme(
    legend.position = "top"
  )

save_thesis_plot(
  fig_zero_robustness,
  "fig_results_07_zero_replacement_robustness",
  7.2, 3.5,
  results_robustness_dir
)

write_result_table(
  zero_metric_summary,
  "table_results_zero_pattern_diagnostics.csv"
)

#FIGURES 6.8a-b — SUBCOMPOSITION SENSITIVITY

message("Figures: subcomposition sensitivity.")

sub_pref_full <- subcomposition_full |>
  dplyr::filter(.data$geometry == preferred_geometry) |>
  dplyr::mutate(
    subcomposition_label = factor(
      .data$subcomposition,
      levels = rev(names(subcomposition_labels)),
      labels = rev(unname(subcomposition_labels))
    )
  )

sub_pref_summary <- sub_pref_full |>
  dplyr::group_by(.data$subcomposition_label) |>
  dplyr::summarise(
    median = stats::median(
      .data$distance_spearman_cor,
      na.rm = TRUE
    ),
    q25 = stats::quantile(
      .data$distance_spearman_cor,
      0.25,
      na.rm = TRUE
    ),
    q75 = stats::quantile(
      .data$distance_spearman_cor,
      0.75,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

fig_subcomposition_pref <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = sub_pref_full,
    ggplot2::aes(
      x = .data$distance_spearman_cor,
      y = .data$subcomposition_label
    ),
    size = 1.2,
    alpha = 0.30,
    colour = "grey35",
    position = ggplot2::position_jitter(
      height = 0.08,
      width = 0
    )
  ) +
  ggplot2::geom_segment(
    data = sub_pref_summary,
    ggplot2::aes(
      x = .data$q25,
      xend = .data$q75,
      y = .data$subcomposition_label,
      yend = .data$subcomposition_label
    ),
    linewidth = 1.15,
    colour = "black"
  ) +
  ggplot2::geom_point(
    data = sub_pref_summary,
    ggplot2::aes(
      x = .data$median,
      y = .data$subcomposition_label
    ),
    size = 2.5,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  ggplot2::labs(
    x = "Spearman correlation with full-composition distances",
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.7) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank()
  )

save_thesis_plot(
  fig_subcomposition_pref,
  "fig_results_08_subcomposition_preferred",
  7.0, 4.0,
  results_robustness_dir
)

sub_all <- subcomposition_summary |>
  dplyr::mutate(
    geometry_label = factor(
      .data$geometry,
      levels = geometry_order,
      labels = unname(geometry_labels[geometry_order])
    ),
    subcomposition_label = factor(
      .data$subcomposition,
      levels = rev(names(subcomposition_labels)),
      labels = rev(unname(subcomposition_labels))
    )
  )

fig_subcomposition_all <- ggplot2::ggplot(
  sub_all,
  ggplot2::aes(
    x = .data$spearman_median,
    y = .data$subcomposition_label
  )
) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = .data$spearman_min,
      xend = .data$spearman_max,
      y = .data$subcomposition_label,
      yend = .data$subcomposition_label
    ),
    linewidth = 0.55,
    colour = "grey45"
  ) +
  ggplot2::geom_point(
    size = 1.8,
    colour = "black"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(geometry_label),
    ncol = 2
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    title = "Subcomposition sensitivity across electoral geometries",
    x = "Spearman correlation with full-composition distances",
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.0)

save_thesis_plot(
  fig_subcomposition_all,
  "fig_appendix_subcomposition_all_geometries",
  7.4, 8.2,
  results_appendix_dir
)

# FIGURE 6.9 — PREFERRED-GRAPH DIAGNOSTICS

message("Figure: graph diagnostics.")

affinity_plot_data <- affinity_diagnostics |>
  dplyr::filter(.data$is_preferred_geometry) |>
  dplyr::select(
    "year",
    "sigma_cv",
    "effective_neighbors_median"
  ) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(
      .data$metric,
      levels = c(
        "sigma_cv",
        "effective_neighbors_median"
      ),
      labels = c(
        "Local-scale CV",
        "Median effective neighbors"
      )
    )
  )

affinity_plot_means <- affinity_plot_data |>
  dplyr::group_by(.data$metric) |>
  dplyr::summarise(
    mean_value = mean(.data$value, na.rm = TRUE),
    .groups = "drop"
  )

fig_affinity_diagnostics <- ggplot2::ggplot(
  affinity_plot_data,
  ggplot2::aes(
    x = .data$year,
    y = .data$value
  )
) +
  ggplot2::geom_line(
    linewidth = 0.60,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 1.6,
    colour = "black"
  ) +
  ggplot2::geom_hline(
    data = affinity_plot_means,
    ggplot2::aes(yintercept = .data$mean_value),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey35"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(metric),
    ncol = 2,
    scales = "free_y"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1948, 1976, 1994, 2013, 2022)
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.5) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank()
  )

save_thesis_plot(
  fig_affinity_diagnostics,
  "fig_results_09_affinity_diagnostics",
  7.0, 3.4,
  results_graph_dir
)

# FIGURE 6.10 — ALGEBRAIC CONNECTIVITY (LAMBDA_2)

message("Figure: lambda_2 through time.")

lambda2_data <- laplacian_eigenvalues |>
  dplyr::filter(
    .data$is_preferred_geometry,
    .data$eigen_index == 2L
  ) |>
  dplyr::arrange(.data$year)

fig_lambda2 <- ggplot2::ggplot(
  lambda2_data,
  ggplot2::aes(
    x = .data$year,
    y = .data$laplacian_eigenvalue
  )
) +
  ggplot2::geom_line(
    linewidth = 0.75,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 2,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    breaks = election_years
  ) +
  ggplot2::labs(
    x = NULL,
    y = expression(lambda[2])
  ) +
  theme_thesis_results() +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    )
  )

save_thesis_plot(
  fig_lambda2,
  "fig_results_10_lambda2_over_time",
  7.0, 4.0,
  results_graph_dir
)

# FIGURE 6.11 — LOW-FREQUENCY SPECTRUM

message("Figure: low-frequency spectrum.")

spectrum_data <- laplacian_eigenvalues |>
  dplyr::filter(
    .data$is_preferred_geometry,
    .data$eigen_index >= 2L,
    .data$eigen_index <= 9L
  ) |>
  dplyr::mutate(
    year_factor = factor(
      .data$year,
      levels = election_years
    ),
    eigen_factor = factor(
      .data$eigen_index,
      levels = rev(2:9),
      labels = paste0("lambda[", rev(2:9), "]")
    )
  )

fig_spectrum <- ggplot2::ggplot(
  spectrum_data,
  ggplot2::aes(
    x = .data$year_factor,
    y = .data$eigen_factor,
    fill = .data$laplacian_eigenvalue
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.25
  ) +
  ggplot2::scale_fill_gradient(
    low = "white",
    high = "grey10",
    name = "Eigenvalue"
  ) +
  ggplot2::scale_y_discrete(
    labels = function(x) parse(text = x)
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.6) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    )
  )

save_thesis_plot(
  fig_spectrum,
  "fig_results_11_low_frequency_spectrum",
  7.2, 4.2,
  results_graph_dir
)

#FIGURES 6.12a-e — PARTITION DIAGNOSTICS ACROSS C

message("Figures: partition diagnostics across C.")

save_quality_heatmap <- function(
    metric,
    metric_title,
    filename,
    limits = NULL,
    digits = 2
) {
  dat <- cluster_quality |>
    dplyr::transmute(
      year = .data$year,
      n_clusters = .data$n_clusters,
      value = .data[[metric]],
      year_factor = factor(
        .data$year,
        levels = election_years
      ),
      C_factor = factor(
        .data$n_clusters,
        levels = sort(unique(cluster_quality$n_clusters))
      )
    )
  
  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$year_factor,
      y = .data$C_factor,
      fill = .data$value
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.25
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "grey10",
      limits = limits,
      name = NULL
    ) +
    ggplot2::labs(
      title = metric_title,
      x = "Election year",
      y = "Number of clusters C"
    ) +
    theme_thesis_results(base_size = 8.4) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1
      )
    )
  
  save_thesis_plot(
    p,
    filename,
    7.1, 4.1,
    results_graph_dir
  )
}

save_quality_heatmap(
  "spectral_silhouette_mean",
  "Spectral silhouette across resolutions",
  "fig_results_12a_spectral_silhouette_heatmap",
  limits = c(0, 1)
)

save_quality_heatmap(
  "electoral_silhouette_mean",
  "Electoral-distance silhouette across resolutions",
  "fig_results_12b_electoral_silhouette_heatmap",
  limits = c(0, 1)
)

save_quality_heatmap(
  "normalized_cut_total",
  "Normalized cut across resolutions",
  "fig_results_12c_normalized_cut_heatmap"
)

save_quality_heatmap(
  "laplacian_eigengap_after_C",
  "Eigengap after C across resolutions",
  "fig_results_12d_eigengap_heatmap"
)

save_quality_heatmap(
  "conductance_max",
  "Maximum cluster conductance across resolutions",
  "fig_appendix_conductance_max_heatmap",
  limits = c(0, 1)
)



# MAIN-TEXT MULTIRESOLUTION SUMMARY

quality_summary_long <- cluster_quality |>
  dplyr::group_by(.data$n_clusters) |>
  dplyr::summarise(
    spectral_silhouette_median = stats::median(.data$spectral_silhouette_mean, na.rm = TRUE),
    spectral_silhouette_q25 = stats::quantile(.data$spectral_silhouette_mean, 0.25, na.rm = TRUE),
    spectral_silhouette_q75 = stats::quantile(.data$spectral_silhouette_mean, 0.75, na.rm = TRUE),
    electoral_silhouette_median = stats::median(.data$electoral_silhouette_mean, na.rm = TRUE),
    electoral_silhouette_q25 = stats::quantile(.data$electoral_silhouette_mean, 0.25, na.rm = TRUE),
    electoral_silhouette_q75 = stats::quantile(.data$electoral_silhouette_mean, 0.75, na.rm = TRUE),
    normalized_cut_median = stats::median(.data$normalized_cut_total, na.rm = TRUE),
    normalized_cut_q25 = stats::quantile(.data$normalized_cut_total, 0.25, na.rm = TRUE),
    normalized_cut_q75 = stats::quantile(.data$normalized_cut_total, 0.75, na.rm = TRUE),
    eigengap_median = stats::median(.data$laplacian_eigengap_after_C, na.rm = TRUE),
    eigengap_q25 = stats::quantile(.data$laplacian_eigengap_after_C, 0.25, na.rm = TRUE),
    eigengap_q75 = stats::quantile(.data$laplacian_eigengap_after_C, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

quality_summary_plot <- dplyr::bind_rows(
  quality_summary_long |>
    dplyr::transmute(
      n_clusters,
      diagnostic = "Spectral silhouette",
      median = spectral_silhouette_median,
      q25 = spectral_silhouette_q25,
      q75 = spectral_silhouette_q75
    ),
  quality_summary_long |>
    dplyr::transmute(
      n_clusters,
      diagnostic = "Electoral-distance silhouette",
      median = electoral_silhouette_median,
      q25 = electoral_silhouette_q25,
      q75 = electoral_silhouette_q75
    ),
  quality_summary_long |>
    dplyr::transmute(
      n_clusters,
      diagnostic = "Normalized cut",
      median = normalized_cut_median,
      q25 = normalized_cut_q25,
      q75 = normalized_cut_q75
    ),
  quality_summary_long |>
    dplyr::transmute(
      n_clusters,
      diagnostic = "Eigengap after C",
      median = eigengap_median,
      q25 = eigengap_q25,
      q75 = eigengap_q75
    )
) |>
  dplyr::mutate(
    diagnostic = factor(
      .data$diagnostic,
      levels = c(
        "Spectral silhouette",
        "Electoral-distance silhouette",
        "Normalized cut",
        "Eigengap after C"
      )
    )
  )

fig_multiresolution_summary <- ggplot2::ggplot(
  quality_summary_plot,
  ggplot2::aes(
    x = .data$n_clusters,
    y = .data$median
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$q25,
      ymax = .data$q75
    ),
    fill = "grey82",
    colour = NA
  ) +
  ggplot2::geom_line(
    linewidth = 0.65,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 1.8,
    colour = "black"
  ) +
  ggplot2::geom_vline(
    xintercept = illustrative_C,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey35"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(diagnostic),
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(cluster_quality$n_clusters))
  ) +
  ggplot2::labs(
    x = "Resolution C",
    y = NULL
  ) +
  theme_thesis_results(base_size = 8.4) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    legend.position = "none"
  )

save_thesis_plot(
  fig_multiresolution_summary,
  "fig_results_12_multiresolution_summary",
  7.2, 5.3,
  results_graph_dir
)

write_result_table(
  quality_summary_long,
  "table_results_multiresolution_summary.csv"
)

#FIGURES 6.14 — SPECTRAL-AXIS ELECTORAL INTERPRETATION
message("Figures: spectral-axis component correlations.")

for (yr in interpretation_years) { # per ogni anno e asse, calcolo  spearman correlation tra coordinata provinciale sull'asse e quota ciascun componente elettorale
  
  axis_y <- axis_correlations |>
    dplyr::filter(.data$year == .env$yr) |>
    dplyr::mutate(
      axis_label = factor(
        .data$eigen_index,
        levels = c(2, 3, 4),
        labels = c("Axis 2", "Axis 3", "Axis 4")
      )
    )
  
  component_order <- axis_y |>
    dplyr::group_by(.data$component) |>
    dplyr::summarise(
      mean_share = mean(.data$mean_component_share, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$mean_share)) |>
    dplyr::pull(.data$component)
  
  axis_y <- axis_y |>
    dplyr::mutate(
      component_label = factor(
        .data$component,
        levels = rev(component_order),
        labels = wrap_component(rev(component_order), 24)
      )
    )
  
  p_axis <- ggplot2::ggplot(
    axis_y,
    ggplot2::aes(
      x = .data$axis_label,
      y = .data$component_label,
      fill = .data$coordinate_spearman_cor
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.2f", .data$coordinate_spearman_cor)
      ),
      size = 2.45
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Spearman"
    ) +
    ggplot2::labs(
      title = paste0(
        "Electoral correlates of spectral axes, ",
        yr
      ),
      x = NULL,
      y = NULL
    ) +
    theme_thesis_results(base_size = 8.2) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  save_thesis_plot(
    p_axis,
    paste0(
      "fig_results_14_axis_component_correlations_",
      yr
    ),
    6.6, 5.2,
    results_interpretation_dir
  )
}


#FIGURES 6.15 — CLUSTER ELECTORAL PROFILES

message("Figures: cluster electoral profiles.")

for (yr in interpretation_years) {
  
  prof_y <- cluster_component_profiles |>
    dplyr::filter(
      .data$year == .env$yr,
      .data$n_clusters == illustrative_C
    )
  
  component_order <- prof_y |>
    dplyr::group_by(.data$component) |>
    dplyr::summarise(
      overall_mean_share = dplyr::first(.data$overall_mean_share),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$overall_mean_share)
    ) |>
    dplyr::pull(.data$component)
  
  prof_y <- prof_y |>
    dplyr::mutate(
      component_label = factor(
        .data$component,
        levels = component_order,
        labels = wrap_component(component_order, 16)
      ),
      cluster_factor = factor(
        .data$cluster,
        levels = seq_len(illustrative_C)
      )
    )
  
  max_abs <- max(
    abs(prof_y$share_difference),
    na.rm = TRUE
  )
  
  p_prof <- ggplot2::ggplot(
    prof_y,
    ggplot2::aes(
      x = .data$component_label,
      y = .data$cluster_factor,
      fill = .data$share_difference
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%+.1f", 100 * .data$share_difference)
      ),
      size = 2.25
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-max_abs, max_abs),
      name = "Difference\n(pp)"
    ) +
    ggplot2::labs(
      title = paste0(
        "Cluster mean electoral profiles, ",
        yr,
        " (C = ",
        illustrative_C,
        ")"
      ),
      subtitle = "Cell values are cluster mean share minus election-wide mean share, in percentage points",
      x = NULL,
      y = "Cluster"
    ) +
    theme_thesis_results(base_size = 8.0) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 55, hjust = 1
      ),
      legend.position = "right"
    )
  
  save_thesis_plot(
    p_prof,
    paste0(
      "fig_results_15_cluster_electoral_profiles_",
      yr,
      "_C",
      illustrative_C
    ),
    7.6, 4.8,
    results_interpretation_dir
  )
}


# FIGURES 6.16 — REGIONAL COMPOSITION OF CLUSTERS

message("Figures: regional composition of clusters.")

for (yr in interpretation_years) {
  
  reg_y <- cluster_region_profiles |>
    dplyr::filter(
      .data$year == .env$yr,
      .data$n_clusters == illustrative_C
    ) |>
    dplyr::select(
      "region",
      "cluster",
      "share_of_region_in_cluster"
    ) |>
    tidyr::complete(
      region,
      cluster = seq_len(illustrative_C),
      fill = list(
        share_of_region_in_cluster = 0
      )
    ) |>
    dplyr::mutate(
      cluster_factor = factor(
        .data$cluster,
        levels = seq_len(illustrative_C)
      ),
      region_factor = factor(
        .data$region,
        levels = rev(sort(unique(.data$region)))
      )
    )
  
  p_reg <- ggplot2::ggplot(
    reg_y,
    ggplot2::aes(
      x = .data$cluster_factor,
      y = .data$region_factor,
      fill = .data$share_of_region_in_cluster
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          .data$share_of_region_in_cluster > 0,
          sprintf("%.0f", 100 * .data$share_of_region_in_cluster),
          ""
        )
      ),
      size = 2.1
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "grey15",
      limits = c(0, 1),
      labels = function(x) percent_axis(x, 0),
      name = "Share of\nregion"
    ) +
    ggplot2::labs(
      title = paste0(
        "Regional allocation across spectral clusters, ",
        yr,
        " (C = ",
        illustrative_C,
        ")"
      ),
      subtitle = "Each row reports the share of a region's provinces assigned to each cluster",
      x = "Cluster",
      y = NULL
    ) +
    theme_thesis_results(base_size = 7.8) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  save_thesis_plot(
    p_reg,
    paste0(
      "fig_results_16_cluster_region_profiles_",
      yr,
      "_C",
      illustrative_C
    ),
    6.7, 6.6,
    results_interpretation_dir
  )
}


# REGIONAL PURITY THROUGH TIME AT C = 4

regional_purity_by_region <- cluster_region_profiles |>
  dplyr::filter(.data$n_clusters == illustrative_C) |>
  dplyr::group_by(.data$year, .data$region) |>
  dplyr::summarise(
    region_size = dplyr::first(.data$region_size),
    modal_cluster_share = max(.data$share_of_region_in_cluster, na.rm = TRUE),
    .groups = "drop"
  )

regional_purity_year <- regional_purity_by_region |>
  dplyr::group_by(.data$year) |>
  dplyr::summarise(
    weighted_regional_purity = stats::weighted.mean(
      .data$modal_cluster_share,
      w = .data$region_size,
      na.rm = TRUE
    ),
    median_regional_purity = stats::median(
      .data$modal_cluster_share,
      na.rm = TRUE
    ),
    q25_regional_purity = stats::quantile(
      .data$modal_cluster_share,
      0.25,
      na.rm = TRUE
    ),
    q75_regional_purity = stats::quantile(
      .data$modal_cluster_share,
      0.75,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

fig_regional_purity <- ggplot2::ggplot(
  regional_purity_year,
  ggplot2::aes(
    x = .data$year,
    y = .data$weighted_regional_purity
  )
) +
  ggplot2::geom_line(
    linewidth = 0.65,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 1.8,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    breaks = election_years
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    labels = function(x) sprintf("%.0f%%", 100 * x)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Weighted regional purity"
  ) +
  theme_thesis_results() +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

save_thesis_plot(
  fig_regional_purity,
  paste0(
    "fig_results_16b_weighted_regional_purity_C",
    illustrative_C
  ),
  7.0, 3.7,
  results_interpretation_dir
)

write_result_table(
  regional_purity_year,
  paste0(
    "table_results_weighted_regional_purity_C",
    illustrative_C,
    ".csv"
  )
)

# FIGURE 6.17 — PARTITION SENSITIVITY TO GEOMETRY

message("Figure: partition sensitivity to geometry.")

geometry_stability_plot <- geometry_stability |>
  dplyr::mutate(
    comparison_geometry_label = factor(
      .data$comparison_geometry,
      levels = setdiff(
        geometry_order,
        preferred_geometry
      ),
      labels = unname(
        geometry_labels[
          setdiff(geometry_order, preferred_geometry)
        ]
      )
    ),
    year_factor = factor(
      .data$year,
      levels = election_years
    ),
    C_factor = factor(
      .data$n_clusters,
      levels = sort(unique(.data$n_clusters))
    )
  )

fig_geometry_stability <- ggplot2::ggplot(
  geometry_stability_plot,
  ggplot2::aes(
    x = .data$year_factor,
    y = .data$C_factor,
    fill = .data$adjusted_rand
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.2
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(comparison_geometry_label),
    ncol = 1
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-0.1, 1),
    name = "ARI"
  ) +
  ggplot2::labs(
    title = "Partition sensitivity to the electoral geometry",
    subtitle = "Adjusted Rand index relative to the preferred Greenacre weighted-power partition",
    x = "Election year",
    y = "Number of clusters C"
  ) +
  theme_thesis_results(base_size = 7.5) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    ),
    legend.position = "right"
  )

save_thesis_plot(
  fig_geometry_stability,
  "fig_results_17_geometry_partition_stability",
  7.4, 10.0,
  results_robustness_dir
)


geometry_C4_plot <- geometry_stability |>
  dplyr::filter(
    .data$n_clusters == illustrative_C,
    .data$comparison_geometry %in% c(
      "greenacre_weighted_lra",
      "aitchison",
      "euclidean"
    )
  ) |>
  dplyr::mutate(
    comparison_geometry_label = factor(
      .data$comparison_geometry,
      levels = c(
        "greenacre_weighted_lra",
        "aitchison",
        "euclidean"
      ),
      labels = c(
        "Weighted LRA",
        "Aitchison",
        "Euclidean"
      )
    )
  )

fig_geometry_C4 <- ggplot2::ggplot(
  geometry_C4_plot,
  ggplot2::aes(
    x = .data$year,
    y = .data$adjusted_rand,
    group = 1
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.30,
    colour = "grey60"
  ) +
  ggplot2::geom_line(
    linewidth = 0.65,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 1.7,
    colour = "black"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(comparison_geometry_label),
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = election_years
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-0.1, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Adjusted Rand index"
  ) +
  theme_thesis_results(base_size = 8.2) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    legend.position = "none",
    panel.spacing.y = grid::unit(0.5, "lines"),
    axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

save_thesis_plot(
  fig_geometry_C4,
  paste0(
    "fig_results_17b_geometry_partition_stability_C",
    illustrative_C
  ),
  7.3, 6.4,
  results_robustness_dir
)

write_result_table(
  geometry_C4_plot |>
    dplyr::select(
      year,
      comparison_geometry,
      adjusted_rand,
      normalized_mutual_information,
      variation_of_information,
      coassignment_pearson
    ),
  paste0(
    "table_results_geometry_stability_C",
    illustrative_C,
    ".csv"
  )
)



# FIGURE 6.19 — ADJACENT-ELECTION PARTITION STABILITY

message("Figure: adjacent-election temporal stability.")

temporal_plot <- temporal_stability |>
  dplyr::mutate(
    transition = paste0(
      .data$year_previous,
      " -> ",
      .data$year_current
    )
  )

transition_levels <- temporal_plot |>
  dplyr::arrange(
    .data$year_previous,
    .data$year_current
  ) |>
  dplyr::pull(.data$transition) |>
  unique()

temporal_plot <- temporal_plot |>
  dplyr::mutate(
    transition = factor(
      .data$transition,
      levels = transition_levels
    ),
    C_factor = factor(
      .data$n_clusters,
      levels = sort(unique(.data$n_clusters))
    )
  )

fig_temporal_stability <- ggplot2::ggplot(
  temporal_plot,
  ggplot2::aes(
    x = .data$transition,
    y = .data$C_factor,
    fill = .data$adjusted_rand
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.25
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-0.1, 1),
    name = "ARI"
  ) +
  ggplot2::labs(
    title = "Stability of adjacent-election territorial partitions",
    x = "Election transition",
    y = "Number of clusters C"
  ) +
  theme_thesis_results(base_size = 8.3) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    )
  )

save_thesis_plot(
  fig_temporal_stability,
  "fig_results_19_temporal_partition_stability",
  7.4, 4.5,
  results_temporal_dir
)


# FIGURE 6.20 — LOWEST-STABILITY TRANSITION MATRICES


message("Figure: selected transition matrices.")

selected_transitions <- temporal_stability |>
  dplyr::filter(.data$n_clusters == illustrative_C) |>
  dplyr::arrange(.data$adjusted_rand) |>
  dplyr::slice_head(n = n_transition_panels) |>
  dplyr::select(
    "year_previous",
    "year_current",
    "adjusted_rand",
    "normalized_mutual_information",
    "variation_of_information",
    "coassignment_pearson"
  ) |>
  dplyr::arrange(
    .data$year_previous,
    .data$year_current
  )

write_result_table(
  selected_transitions,
  paste0(
    "table_results_selected_transitions_C",
    illustrative_C,
    ".csv"
  )
)

transition_plot_data <- transition_matrices |>
  dplyr::filter(.data$n_clusters == illustrative_C) |>
  dplyr::semi_join(
    selected_transitions,
    by = c(
      "year_previous",
      "year_current"
    )
  ) |>
  dplyr::mutate(
    transition = paste0(
      .data$year_previous,
      " -> ",
      .data$year_current
    ),
    origin_cluster = factor(
      .data$cluster_previous,
      levels = seq_len(illustrative_C)
    ),
    destination_cluster = factor(
      .data$cluster_current_matched,
      levels = seq_len(illustrative_C)
    )
  )

selected_transition_levels <- selected_transitions |>
  dplyr::transmute(
    transition = paste0(
      .data$year_previous,
      " -> ",
      .data$year_current
    )
  ) |>
  dplyr::pull(.data$transition)

transition_plot_data <- transition_plot_data |>
  dplyr::mutate(
    transition = factor(
      .data$transition,
      levels = selected_transition_levels
    )
  )

fig_transition_matrices <- ggplot2::ggplot(
  transition_plot_data,
  ggplot2::aes(
    x = .data$destination_cluster,
    y = .data$origin_cluster,
    fill = .data$share_of_origin_cluster
  )
) +
  ggplot2::geom_tile(
    colour = "white",
    linewidth = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf(
        "%.0f",
        100 * .data$share_of_origin_cluster
      )
    ),
    size = 2.4
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(transition),
    nrow = 1
  ) +
  ggplot2::scale_fill_gradient(
    low = "white",
    high = "grey10",
    limits = c(0, 1),
    labels = function(x) percent_axis(x, 0),
    name = "Share of\norigin cluster"
  ) +
  ggplot2::labs(
    title = paste0(
      "Lowest-stability adjacent transitions at C = ",
      illustrative_C
    ),
    subtitle = "Destination labels are matched only for transition-matrix readability",
    x = "Cluster in current election (matched label)",
    y = "Cluster in previous election"
  ) +
  theme_thesis_results(base_size = 7.9) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    legend.position = "right"
  )

save_thesis_plot(
  fig_transition_matrices,
  paste0(
    "fig_results_20_transition_matrices_C",
    illustrative_C
  ),
  8.0, 3.6,
  results_temporal_dir
)

# GEOMETRY-SPECIFIC DISTANCE ALIGNMENT BY YEAR

geometry_pairs_election <- read_required_csv(
  file.path(
    stage04_table_dir,
    "stage04_full_pairwise_geometry_comparison.csv"
  )
)

preferred_vs_others <- geometry_pairs_election |>
  dplyr::filter(
    .data$geometry_a == preferred_geometry |
      .data$geometry_b == preferred_geometry
  ) |>
  dplyr::mutate(
    comparison_geometry = dplyr::if_else(
      .data$geometry_a == preferred_geometry,
      .data$geometry_b,
      .data$geometry_a
    ),
    comparison_geometry_label = factor(
      .data$comparison_geometry,
      levels = setdiff(
        geometry_order,
        preferred_geometry
      ),
      labels = unname(
        geometry_labels[
          setdiff(geometry_order, preferred_geometry)
        ]
      )
    )
  )

fig_distance_alignment_year <- ggplot2::ggplot(
  preferred_vs_others,
  ggplot2::aes(
    x = .data$year,
    y = .data$distance_spearman_cor,
    group = .data$comparison_geometry_label,
    linetype = .data$comparison_geometry_label
  )
) +
  ggplot2::geom_line(
    linewidth = 0.55,
    colour = "black"
  ) +
  ggplot2::geom_point(
    size = 1.35,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    breaks = election_years
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    title = "Distance agreement with the preferred geometry by election",
    x = "Election year",
    y = "Spearman correlation",
    linetype = NULL
  ) +
  theme_thesis_results(base_size = 8.2) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 90, vjust = 0.5, hjust = 1
    ),
    legend.position = "top"
  )

save_thesis_plot(
  fig_distance_alignment_year,
  "fig_appendix_preferred_geometry_distance_alignment_by_year",
  7.4, 4.6,
  results_appendix_dir
)



# APPENDIX TABLES


# ALL C=4 TRANSITION MATRICES + REPRESENTATIVE LOW / TYPICAL / HIGH CASES


transition_all_dir <- file.path(
  results_temporal_dir,
  paste0("transition_matrices_C", illustrative_C, "_all")
)
fs::dir_create(transition_all_dir)

transition_C4 <- temporal_stability |>
  dplyr::filter(.data$n_clusters == illustrative_C) |>
  dplyr::arrange(.data$year_previous, .data$year_current)

transition_C4_median <- stats::median(
  transition_C4$adjusted_rand,
  na.rm = TRUE
)

transition_examples <- dplyr::bind_rows(
  transition_C4 |>
    dplyr::slice_min(
      order_by = .data$adjusted_rand,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::mutate(example_type = "Low stability"),
  transition_C4 |>
    dplyr::slice_min(
      order_by = abs(.data$adjusted_rand - transition_C4_median),
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

write_result_table(
  transition_C4,
  paste0(
    "table_results_all_transitions_C",
    illustrative_C,
    ".csv"
  )
)

write_result_table(
  transition_examples,
  paste0(
    "table_results_transition_examples_C",
    illustrative_C,
    ".csv"
  )
)

make_transition_matrix_plot <- function(year_previous, year_current) {
  dat <- transition_matrices |>
    dplyr::filter(
      .data$n_clusters == illustrative_C,
      .data$year_previous == .env$year_previous,
      .data$year_current == .env$year_current
    ) |>
    dplyr::mutate(
      origin_cluster = factor(
        .data$cluster_previous,
        levels = seq_len(illustrative_C)
      ),
      destination_cluster = factor(
        .data$cluster_current_matched,
        levels = seq_len(illustrative_C)
      )
    )
  
  ari_here <- transition_C4 |>
    dplyr::filter(
      .data$year_previous == .env$year_previous,
      .data$year_current == .env$year_current
    ) |>
    dplyr::pull(.data$adjusted_rand)
  
  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$destination_cluster,
      y = .data$origin_cluster,
      fill = .data$share_of_origin_cluster
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf(
          "%.0f",
          100 * .data$share_of_origin_cluster
        )
      ),
      size = 3
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "grey10",
      limits = c(0, 1),
      labels = function(x) percent_axis(x, 0),
      name = "Share of\norigin cluster"
    ) +
    ggplot2::labs(
      title = paste0(
        year_previous,
        " -> ",
        year_current,
        "  (ARI = ",
        sprintf("%.3f", ari_here),
        ")"
      ),
      x = "Current cluster (matched)",
      y = "Previous cluster"
    ) +
    theme_thesis_results(base_size = 8.4) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

for (i in seq_len(nrow(transition_C4))) {
  yp <- transition_C4$year_previous[[i]]
  yc <- transition_C4$year_current[[i]]
  p_tm <- make_transition_matrix_plot(yp, yc)
  
  save_thesis_plot(
    p_tm,
    paste0(
      "fig_transition_matrix_",
      yp,
      "_",
      yc,
      "_C",
      illustrative_C
    ),
    5.0, 4.2,
    transition_all_dir
  )
}

selected_transition_pairs <- tibble::tribble(
  ~year_previous, ~year_current,
  1976L, 1979L,
  2008L, 2013L,
  2013L, 2018L
)

selected_transition_matrix_plots <- lapply(
  seq_len(nrow(selected_transition_pairs)),
  function(i) {
    make_transition_matrix_plot(
      selected_transition_pairs$year_previous[[i]],
      selected_transition_pairs$year_current[[i]]
    )
  }
)

fig_transition_matrices_selected <- patchwork::wrap_plots(
  selected_transition_matrix_plots,
  nrow = 1,
  guides = "collect"
) &
  ggplot2::theme(
    legend.position = "right"
  )

save_thesis_plot(
  fig_transition_matrices_selected,
  "fig_results_20_selected_transition_matrices_C4",
  10.5, 3.8,
  results_temporal_dir
)

example_plots <- lapply(
  seq_len(nrow(transition_examples)),
  function(i) {
    make_transition_matrix_plot(
      transition_examples$year_previous[[i]],
      transition_examples$year_current[[i]]
    ) +
      ggplot2::labs(
        subtitle = transition_examples$example_type[[i]]
      )
  }
)

fig_transition_examples <- patchwork::wrap_plots(
  example_plots,
  nrow = 1,
  guides = "collect"
) &
  ggplot2::theme(
    legend.position = "right"
  )

save_thesis_plot(
  fig_transition_examples,
  paste0(
    "fig_results_20b_transition_examples_C",
    illustrative_C
  ),
  10.5, 3.8,
  results_temporal_dir
)

message("Preparing appendix tables.")

appendix_component_classification <- component_classification |>
  dplyr::select(
    year,
    component,
    national_share,
    national_rank,
    manual_territorial_component,
    final_component_class,
    final_component_in_spec
  ) |>
  dplyr::arrange(
    .data$year,
    .data$national_rank,
    .data$component
  )

write_latex_longtable(
  appendix_component_classification,
  "appendix_component_classification.tex",
  "Election-specific classification of raw electoral lists.",
  "app_component_classification",
  digits = 4
)

appendix_component_zeros <- final_component_zeros |>
  dplyr::select(
    year,
    component,
    national_share,
    zero_count,
    zero_share
  ) |>
  dplyr::arrange(
    .data$year,
    dplyr::desc(.data$zero_share),
    .data$component
  )

write_latex_longtable(
  appendix_component_zeros,
  "appendix_component_zeros.tex",
  "Component-level zero structure by election.",
  "app_component_zeros",
  digits = 4
)

appendix_mass_variability <- clr_variance_by_component |>
  dplyr::select(
    year,
    component,
    mean_proportion,
    clr_variance,
    zero_share,
    log_mean_proportion,
    log_clr_variance
  ) |>
  dplyr::arrange(
    .data$year,
    dplyr::desc(.data$mean_proportion)
  )

write_latex_longtable(
  appendix_mass_variability,
  "appendix_mass_variability.tex",
  "Component mass and clr variability by election.",
  "app_mass_variability",
  digits = 4
)

appendix_distance_contributions <- component_contributions |>
  dplyr::filter(
    .data$geometry %in% geometry_order
  ) |>
  dplyr::mutate(
    geometry = dplyr::recode(
      .data$geometry,
      !!!geometry_labels
    )
  ) |>
  dplyr::select(
    year,
    geometry,
    component,
    share_of_total_pairwise_distance
  ) |>
  dplyr::arrange(
    .data$year,
    .data$geometry,
    dplyr::desc(.data$share_of_total_pairwise_distance)
  )

write_latex_longtable(
  appendix_distance_contributions,
  "appendix_distance_contributions.tex",
  "Component contributions to total pairwise squared distance.",
  "app_distance_contributions",
  digits = 4
)

appendix_geometry_agreement <- geometry_comparison_full |>
  dplyr::mutate(
    geometry_a = dplyr::recode(
      .data$geometry_a,
      !!!geometry_labels
    ),
    geometry_b = dplyr::recode(
      .data$geometry_b,
      !!!geometry_labels
    )
  ) |>
  dplyr::select(
    year,
    geometry_a,
    geometry_b,
    distance_spearman_cor,
    distance_pearson_cor
  ) |>
  dplyr::arrange(
    .data$year,
    .data$geometry_a,
    .data$geometry_b
  )

write_latex_longtable(
  appendix_geometry_agreement,
  "appendix_geometry_agreement.tex",
  "Election-specific agreement between electoral distance geometries.",
  "app_geometry_agreement",
  digits = 4
)

appendix_zero_hamming <- zero_metric_full |>
  dplyr::mutate(
    metric = dplyr::recode(
      .data$metric,
      !!!geometry_labels
    )
  ) |>
  dplyr::select(
    year,
    metric,
    zero_hamming_share_mean,
    distance_zero_hamming_share_spearman,
    distance_zero_hamming_share_pearson
  ) |>
  dplyr::arrange(
    .data$year,
    .data$metric
  )

write_latex_longtable(
  appendix_zero_hamming,
  "appendix_zero_hamming.tex",
  "Election-specific association between zero-pattern mismatch and electoral distance.",
  "app_zero_hamming",
  digits = 4
)

appendix_subcomposition_preferred <- subcomposition_full |>
  dplyr::filter(
    .data$geometry == preferred_geometry
  ) |>
  dplyr::mutate(
    subcomposition = dplyr::recode(
      .data$subcomposition,
      !!!subcomposition_labels
    )
  ) |>
  dplyr::select(
    year,
    subcomposition,
    n_full_components,
    n_sub_components,
    distance_spearman_cor,
    distance_pearson_cor
  ) |>
  dplyr::arrange(
    .data$year,
    .data$subcomposition
  )

write_latex_longtable(
  appendix_subcomposition_preferred,
  "appendix_subcomposition_preferred.tex",
  "Election-specific subcomposition sensitivity under the preferred geometry.",
  "app_subcomposition_preferred",
  digits = 4
)

appendix_graph_diagnostics <- affinity_diagnostics |>
  dplyr::filter(
    .data$is_preferred_geometry
  ) |>
  dplyr::select(
    year,
    n_nodes,
    neighborhood_m,
    sigma_median,
    sigma_cv,
    weighted_degree_mean,
    weighted_degree_median,
    weighted_degree_cv,
    effective_neighbors_median
  ) |>
  dplyr::arrange(.data$year)

write_latex_longtable(
  appendix_graph_diagnostics,
  "appendix_graph_diagnostics.tex",
  "Election-specific affinity diagnostics under the preferred geometry.",
  "app_graph_diagnostics",
  digits = 4
)

#reproducibility
writeLines(
  capture.output(sessionInfo()),
  file.path(results_table_dir, "results_nonmap_figures_sessionInfo.txt") # documenta versione R e pacchetti
)

message("Non-map Results figures complete.")
message("Saved under: ", results_nonmap_dir)
message("Compact Results tables saved under: ", results_table_dir)
message("For Overleaf: copy appendix_*.tex from results_table_dir to Chapters/tables/.")
message("For Overleaf: copy the required Results/Appendix PNG or PDF figures to Chapters/.")
