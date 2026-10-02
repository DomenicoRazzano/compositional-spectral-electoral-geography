# A Compositional Spectral Framework for Electoral Geography

Replication repository for the thesis **A Compositional Spectral Framework for Electoral Geography**.

This project studies Italian Chamber elections from 1948 to 2022 on a harmonized provincial geography. The computational pipeline constructs province-level electoral compositions, compares alternative compositional geometries, builds locally scaled affinity graphs, derives normalized-Laplacian spectral representations, and studies territorial partitions across geometries, resolutions, and elections.

## Repository structure

* [`05\\\_data/`](05_data/) — electoral source data, territorial lookup material, public harmonized data, and pointers to large processed datasets.
* [`06\\\_code/`](06_code/) — complete R replication pipeline and helper functions.
* [`07\\\_outputs/`](07_outputs/) — machine-readable tables, supplementary figures, maps, and pointers to large output archives.
* `renv.lock` — package versions for reproducibility.
* `.Rprofile` — project-level R/renv activation.
* `thesis\\\_material.Rproj` — RStudio project file.

Each main directory contains its own README with detailed documentation.

## Replication

Restore the R environment from the repository root:

```r
install.packages("renv")
renv::restore()
```

Then run:

```r
source(here::here("06\\\_code", "run\\\_all.R"))
```

For script-by-script documentation see [`06\\\_code/README.md`](06_code/README.md).

## Large files

Large generated datasets and the exhaustive territorial atlas are distributed through the [latest GitHub release](https://github.com/DomenicoRazzano/compositional-spectral-electoral-geography/releases/latest) rather than ordinary Git history.

See [`05\\\_data/README.md`](05_data/README.md) and [`07\\\_outputs/README.md`](07_outputs/README.md) for the exact contents.

## Data sources

Electoral source files derive from the Italian Ministry of the Interior electoral open-data archive. Territorial harmonization uses ISTAT administrative and historical territorial material.

Third-party data retain the attribution and reuse terms of their original providers.
