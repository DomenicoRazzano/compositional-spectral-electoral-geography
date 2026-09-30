# Replication code

This directory contains the complete R implementation of the thesis analysis.

## Pipeline

- `00_setup.R` defines project paths, package requirements, and loads helper functions.
- `01_data_cleaning.R` constructs the cleaned electoral archive.
- `02_build_province_lookup.R` builds actual and harmonized territorial assignments.
- `03_apply_harmonized_provinces.R` applies the common provincial frame.
- `99_public_dataset.R` creates the public harmonized dataset.
- `04_build_compositional_geometry.R` constructs electoral compositions,
  alternative geometries, distance matrices, and robustness diagnostics.
- `05_graphs.R` constructs locally scaled affinity graphs, normalized
  Laplacians, spectral representations, partitions, and temporal diagnostics.
- `98_results_figures_nonmap.R` generates non-map Results figures and compact tables.
- `99_results_figures_maps.R` generates map-based Results outputs.
- `run_all.R` executes the full pipeline in order.

Reusable helper functions are stored under `functions/`.
