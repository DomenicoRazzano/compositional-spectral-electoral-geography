# Replication code

This directory contains the complete R implementation used to construct the harmonized electoral archive, define the compositional electoral geometries, build the spectral graphs and partitions, and generate the numerical and graphical outputs reported in the thesis **A Compositional Spectral Framework for Electoral Geography**.

The code is organized as a staged pipeline. Each main script can be run independently once its upstream inputs are available, while `run\\\_all.R` executes the complete workflow in order.

\---

## Directory contents

```text
06\\\_code/
├── 00\\\_setup.R
├── 01\\\_data\\\_cleaning.R
├── 02\\\_build\\\_province\\\_lookup.R
├── 03\\\_apply\\\_harmonized\\\_provinces.R
├── 04\\\_build\\\_compositional\\\_geometry.R
├── 05\\\_graphs.R
├── 98\\\_results\\\_figures\\\_nonmap.R
├── 99\\\_public\\\_dataset.R
├── 99\\\_results\\\_figures\\\_maps.R
├── run\\\_all.R
└── functions/
```

The `functions/` directory contains reusable helper functions for electoral-file processing, validation, exclusion auditing, territorial matching and harmonization, project I/O, and output generation. These functions are sourced automatically by `00\\\_setup.R`.

\---

## Pipeline overview

The computational workflow is

```text
raw electoral files
    ↓
cleaning and reporting-unit audit
    ↓
actual and harmonized territorial lookup
    ↓
harmonized electoral archive
    ↓
public harmonized dataset
    ↓
election-specific electoral compositions
    ↓
alternative compositional geometries and pairwise distances
    ↓
locally scaled affinity graphs
    ↓
normalized-Laplacian spectral representations
    ↓
fixed-C territorial partitions
    ↓
cross-geometry and temporal diagnostics
    ↓
tables, figures, maps, and supplementary atlases
```

The project preserves the distinction between analytical objects and display-only relabeling. Maximum-overlap cluster-label matching used in maps changes only displayed labels and colours; it does not alter graphs, eigensystems, partitions, adjusted Rand indices, or other label-invariant statistics.

\---

## Main scripts

### `00\\\_setup.R`

Initializes the project environment.

It:

* checks the packages required by the core pipeline;
* defines canonical project paths under `05\\\_data/` and `07\\\_outputs/`;
* creates missing generated-data/output directories;
* automatically sources every `.R` file in `06\\\_code/functions/`.

The core packages checked here are:

```text
here
readr
dplyr
tidyr
purrr
stringr
tibble
ggplot2
fs
compositions
```

Some downstream scripts perform additional checks for specialized packages required for spatial processing, label matching, composite figures, and related tasks.

\---

### `01\\\_data\\\_cleaning.R`

Constructs the pre-territorial processed and analytical Chamber-of-Deputies electoral archives.

Main operations include:

* matching raw electoral files to the expected election catalogue;
* harmonizing heterogeneous historical file schemas;
* constructing a common row structure across elections;
* adding special electoral supplements where ordinary proportional files do not contain the required information;
* auditing reporting units and exclusions;
* reconstructing residual null ballots where required;
* running processed and analytical validation checks.

Principal generated datasets include:

```text
05\\\_data/processed/camera\\\_all\\\_processed.rds
05\\\_data/processed/camera\\\_all\\\_processed.csv
05\\\_data/processed/camera\\\_all\\\_analytical.rds
05\\\_data/processed/camera\\\_all\\\_analytical.csv
```

Validation and exclusion-audit material is also written under `05\\\_data/processed/`.

\---

### `02\\\_build\\\_province\\\_lookup.R`

Builds the actual and harmonized province assignments used to place all elections on a common territorial support.

The script:

* reads the analytical electoral archive;
* constructs the electoral reporting-unit universe;
* loads election-date-specific territorial references;
* assigns actual provinces;
* maps historical territorial units to the harmonized target frame;
* applies manual resolutions where required;
* produces diagnostics for unresolved or ambiguous matches.

The harmonization target is the 2022 provincial frame used throughout the empirical analysis.

Principal lookup products are written under:

```text
05\\\_data/lookup/
```

\---

### `03\\\_apply\\\_harmonized\\\_provinces.R`

Applies the territorial lookup to the analytical electoral archive.

The script:

* joins actual and harmonized province assignments to the electoral data;
* applies maintained list-name corrections where specified;
* validates the harmonized analytical dataset;
* writes harmonized-data outputs and territorial coverage summaries.

Principal generated datasets include:

```text
05\\\_data/processed/camera\\\_all\\\_harmonized.rds
05\\\_data/processed/camera\\\_all\\\_harmonized.csv
```

The harmonized archive is the input to the compositional stage.

\---

### `99\\\_public\\\_dataset.R`

Creates the public-release version of the harmonized electoral archive.

It retains the variables required for reproducibility and substantive reuse while excluding unnecessary internal processing fields.

Principal outputs are:

```text
05\\\_data/public/camera\\\_all\\\_harmonized\\\_public.rds
05\\\_data/public/camera\\\_all\\\_harmonized\\\_public.csv
```

The RDS version can be stored directly in the Git repository. The larger CSV version can be distributed with a versioned repository release.

\---

### `04\\\_build\\\_compositional\\\_geometry.R`

Implements the compositional stage of the analysis.

Starting from the harmonized electoral archive, it constructs election-specific provincial electoral compositions and the alternative geometries compared in the thesis.

The current geometry set includes:

```text
Aitchison
Greenacre weighted LRA
Tsagris–Preston–Wood positive-power geometry
Greenacre unweighted positive-power geometry
Greenacre weighted positive-power geometry
Euclidean shares
```

The preferred empirical specification is the Greenacre weighted positive-power geometry with:

```text
lambda = 0.175
```

The script also implements:

* support construction and retained-component diagnostics;
* residual and territorial component classification;
* structural-zero summaries;
* fixed pseudo-count log-ratio regularization;
* share-floor zero replacement sensitivity;
* zero-pattern diagnostics;
* component-mass calculations;
* clr variability diagnostics;
* positive-power calibration against log-ratio benchmarks;
* full pairwise geometry comparisons;
* component contributions to pairwise distances;
* selected subcomposition sensitivity analyses.

The principal reusable object is:

```text
05\\\_data/processed/compositional/stage04\\\_composition\\\_distance\\\_objects.rds
```

For each election, this object stores the amount matrix, coordinate representations, component masses, node metadata, and pairwise distance matrices required by Stage 05.

Machine-readable Stage 04 diagnostics are written to:

```text
07\\\_outputs/tables/compositional/
```

including, among others:

```text
stage04\\\_support\\\_summary.csv
stage04\\\_component\\\_final\\\_classification.csv
stage04\\\_final\\\_component\\\_zeros.csv
stage04\\\_final\\\_component\\\_zero\\\_summary.csv
stage04\\\_distance\\\_summary.csv
stage04\\\_full\\\_pairwise\\\_geometry\\\_comparison.csv
stage04\\\_full\\\_pairwise\\\_geometry\\\_comparison\\\_summary.csv
stage04\\\_zero\\\_metric\\\_diagnostics\\\_final\\\_objects.csv
stage04\\\_zero\\\_replacement\\\_sensitivity.csv
stage04\\\_component\\\_contributions\\\_by\\\_geometry.csv
stage04\\\_clr\\\_variance\\\_by\\\_component.csv
stage04\\\_clr\\\_variance\\\_weighting\\\_summary.csv
stage04\\\_component\\\_masses.csv
stage04\\\_alpha\\\_alignment\\\_profile.csv
stage04\\\_greenacre\\\_power\\\_alignment\\\_profile.csv
stage04\\\_subcomposition\\\_stability.csv
stage04\\\_subcomposition\\\_stability\\\_summary.csv
```

\---

### `05\\\_graphs.R`

Implements the graph and spectral stage.

For each election and electoral geometry, the script takes the Stage 04 pairwise distance matrix and constructs a complete locally scaled affinity graph.

The main pipeline is:

```text
distance matrix
    ↓
local neighborhood scale
    ↓
Gaussian affinity matrix
    ↓
weighted degree matrix
    ↓
symmetric normalized Laplacian
    ↓
ordered eigensystem
    ↓
row-normalized spectral embedding
    ↓
fixed-C k-means partition
```

The fixed partition grid is:

```text
C = 2, ..., 8
```

The Greenacre weighted positive-power geometry at `lambda = 0.175` is used as the preferred specification for the principal temporal and interpretive analysis.

The script also computes:

* local-scale and affinity diagnostics;
* effective-neighbor diagnostics;
* normalized-Laplacian eigenvalues;
* spectral coordinates;
* fixed-C cluster assignments;
* partition-quality diagnostics;
* component and regional cluster profiles;
* cross-geometry partition stability;
* adjacent-election partition stability;
* maximum-overlap temporal label matching;
* transition matrices;
* province-level coassignment instability.

Principal Stage 05 machine-readable outputs are written to:

```text
07\\\_outputs/tables/spectral/
```

including:

```text
stage05\\\_affinity\\\_diagnostics.csv
stage05\\\_laplacian\\\_eigenvalues.csv
stage05\\\_spectral\\\_coordinates\\\_main\\\_spec.csv
stage05\\\_fixedC\\\_cluster\\\_assignments.csv
stage05\\\_fixedC\\\_cluster\\\_quality\\\_main\\\_spec.csv
stage05\\\_axis\\\_component\\\_correlations\\\_main\\\_spec.csv
stage05\\\_axis\\\_province\\\_extremes\\\_main\\\_spec.csv
stage05\\\_cluster\\\_component\\\_profiles\\\_main\\\_spec.csv
stage05\\\_cluster\\\_region\\\_profiles\\\_main\\\_spec.csv
stage05\\\_cluster\\\_geometry\\\_stability.csv
stage05\\\_cluster\\\_temporal\\\_stability\\\_main\\\_spec.csv
stage05\\\_transition\\\_matrices\\\_main\\\_spec.csv
stage05\\\_province\\\_instability\\\_main\\\_spec.csv
```

Serialized spectral objects are stored under:

```text
05\\\_data/processed/spectral/
```

\---

### `98\\\_results\\\_figures\\\_nonmap.R`

Generates the non-cartographic figures and compact result tables used in the Results chapter and supplementary material.

It does **not** re-estimate Stage 04 or Stage 05. It reads the canonical CSV outputs produced by those stages.

Outputs are organized under:

```text
07\\\_outputs/figures/results/non\\\_map/
```

with thematic subdirectories:

```text
core/
robustness/
graph/
interpretation/
temporal/
appendix/
```

Compact Results tables are written to:

```text
07\\\_outputs/tables/results/
```

The generated material covers:

* support dimensionality;
* residual support shares;
* zero structure;
* component mass and clr variability;
* power-parameter calibration;
* cross-geometry distance agreement;
* zero-replacement robustness;
* subcomposition sensitivity;
* affinity diagnostics;
* low-frequency spectral structure;
* multiresolution partition diagnostics;
* electoral and regional cluster interpretation;
* geometry-level partition sensitivity;
* temporal partition stability;
* selected transition matrices;
* province-level instability summaries.

\---

### `99\\\_results\\\_figures\\\_maps.R`

Generates all spatial Results outputs requiring the ISTAT shapefiles.

The script reads canonical Stage 05 cluster assignments and the harmonized province lookup, then joins them to the generalized 2022 ISTAT provincial and regional geometries.

Its principal outputs are stored under:

```text
07\\\_outputs/figures/results/maps/
```

including:

```text
main\\\_ready/
geometry\\\_comparisons\\\_C4/
transition\\\_pairs\\\_C4/
transition\\\_stories\\\_C4/
province\\\_instability/
clusters\\\_all/
```

The complete atlas covers every available combination of:

```text
19 elections
× 6 electoral geometries
× C = 2, ..., 8
```

The atlas is large and may be distributed as a release asset rather than stored directly in Git history.

For visualization only, preferred-geometry cluster labels are aligned sequentially through time by maximum provincial overlap. Alternative geometries are aligned within election to the preferred partition. This affects displayed labels and colours only.

\---

### `run\\\_all.R`

Executes the full replication pipeline in dependency order.

The maintained version should be:

```r
source(here::here("06\\\_code", "01\\\_data\\\_cleaning.R"))
source(here::here("06\\\_code", "02\\\_build\\\_province\\\_lookup.R"))
source(here::here("06\\\_code", "03\\\_apply\\\_harmonized\\\_provinces.R"))
source(here::here("06\\\_code", "99\\\_public\\\_dataset.R"))

source(here::here("06\\\_code", "04\\\_build\\\_compositional\\\_geometry.R"))
source(here::here("06\\\_code", "05\\\_graphs.R"))

source(here::here("06\\\_code", "98\\\_results\\\_figures\\\_nonmap.R"))
source(here::here("06\\\_code", "99\\\_results\\\_figures\\\_maps.R"))
```

`00\\\_setup.R` does not need to be sourced separately here because each main stage sources it directly.

\---

## Helper functions

The `functions/` directory contains reusable functions for:

* processing heterogeneous election files;
* project-level input/output utilities;
* reporting-unit exclusion auditing;
* validation and validation reporting;
* territorial reference construction;
* actual-province matching;
* historical-to-target territorial harmonization;
* lookup application;
* territorial output generation;
* audit plotting and audit-table export.

These helpers are loaded automatically by `00\\\_setup.R`.

\---

## Expected project structure

The scripts assume they are run from the repository root and that the project retains:

```text
05\\\_data/
06\\\_code/
07\\\_outputs/
```

Path resolution is handled with `here`. The numeric prefixes are therefore part of the maintained project structure and should not be changed without updating the scripts.

Principal data directories:

```text
05\\\_data/raw/
05\\\_data/interim/
05\\\_data/processed/
05\\\_data/public/
05\\\_data/lookup/
```

Principal output directories:

```text
07\\\_outputs/tables/compositional/
07\\\_outputs/tables/spectral/
07\\\_outputs/tables/results/

07\\\_outputs/figures/compositional/
07\\\_outputs/figures/results/
```

Generated directories are created automatically where appropriate.

\---

## Software environment

The repository includes `renv.lock` to record the R package environment.

From the repository root:

```r
install.packages("renv")
renv::restore()
```

Then run:

```r
source(here::here("06\\\_code", "run\\\_all.R"))
```

Some scripts perform additional runtime checks for packages required for spatial processing, label matching, or composite figures.

\---

## Large files and repository releases

Several generated datasets and figure collections are deliberately excluded from ordinary Git history because of their size.

These may include:

* the complete processed-data archive;
* the CSV version of the public harmonized dataset;
* the complete election-by-geometry-by-resolution cluster-map atlas.

They are distributed through versioned repository release assets. Large generated intermediates can also be reconstructed by rerunning the pipeline from the raw inputs.

\---

## Reproducibility notes

1. **Deterministic spectral orientation.** Eigenvector signs are oriented deterministically for reproducible interpretation.
2. **Fixed clustering seeds.** Spectral k-means uses fixed seeds and repeated starts so stored partitions are reproducible conditional on the same software environment.
3. **Label invariance.** Numerical cluster labels are arbitrary; partition-comparison statistics are label-invariant.
4. **Map-label alignment.** Maximum-overlap matching used in maps is a display convention only and does not alter analytical results.
5. **Generated outputs.** Results-figure scripts consume canonical Stage 04/05 outputs; they do not silently re-estimate the main analytical objects.
6. **Large release assets.** Files excluded from Git because of size are either available through the versioned release or regenerable from the pipeline.

\---

## Recommended execution modes

### Full replication

```r
source(here::here("06\\\_code", "run\\\_all.R"))
```

### Re-run only the analytical stages

If the harmonized electoral archive already exists:

```r
source(here::here("06\\\_code", "04\\\_build\\\_compositional\\\_geometry.R"))
source(here::here("06\\\_code", "05\\\_graphs.R"))
```

### Re-generate only Results figures

If the canonical Stage 04 and Stage 05 tables already exist:

```r
source(here::here("06\\\_code", "98\\\_results\\\_figures\\\_nonmap.R"))
source(here::here("06\\\_code", "99\\\_results\\\_figures\\\_maps.R"))
```

\---

## Output interpretation

This repository is the computational supplement to the thesis rather than a standalone software package.

The thesis provides the formal definitions and substantive interpretation. The code directory provides the exact implementation of those definitions, while `07\\\_outputs/` preserves the machine-readable evidence and supplementary diagnostics used to support the reported empirical results.

For a high-level description of the project, data availability, large release assets, citation information, and licensing, see the repository-level `README.md`.



