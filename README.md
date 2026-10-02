# A Compositional Spectral Framework for Electoral Geography

Replication repository for the thesis **A Compositional Spectral Framework for Electoral Geography**.

This project studies Italian Chamber elections from 1948 to 2022 on a harmonized provincial geography. The computational pipeline constructs province-level electoral compositions, compares alternative compositional geometries, builds locally scaled affinity graphs, derives normalized-Laplacian spectral representations, and studies territorial partitions across geometries, resolutions, and elections.

## Repository structure

- [`05_data/`](05_data/) — electoral source data, territorial lookup material, public harmonized data, and pointers to large processed datasets.
- [`06_code/`](06_code/) — complete R replication pipeline and helper functions.
- [`07_outputs/`](07_outputs/) — machine-readable tables, supplementary figures, maps, and pointers to large output archives.
- `renv.lock` — package versions used by the replication environment.
- `.Rprofile` — project-level `renv` activation.
- `thesis_material.Rproj` — RStudio project file.

Each main directory contains its own README with more detailed documentation.

## Replication

The project uses `renv` to record the R package environment used for the replication release.

From a clean clone of the repository, restore the project environment with:

```r
install.packages("renv")
renv::restore()
```

Then run the complete replication pipeline from the repository root:

```r
source(here::here("06_code", "run_all.R"))
```

For script-by-script documentation, see [`06_code/README.md`](06_code/README.md).

## Large files

Large generated datasets and the exhaustive territorial partition atlas are distributed through the [latest GitHub release](https://github.com/DomenicoRazzano/compositional-spectral-electoral-geography/releases/latest) rather than stored directly in ordinary Git history.

See [`05_data/README.md`](05_data/README.md) and [`07_outputs/README.md`](07_outputs/README.md) for the exact organization of the data and empirical outputs.

## Data sources

Electoral source files derive from the Italian Ministry of the Interior electoral open-data archive. Territorial harmonization uses ISTAT administrative and historical territorial material.

Third-party data retain the attribution and reuse terms of their original providers.

## Citation

The versioned replication release associated with this thesis is archived on Zenodo:

**DOI:** https://doi.org/10.5281/zenodo.23070625

Citation metadata are also provided in `CITATION.cff`.

## License

Original analysis code is released under the MIT License.

Third-party electoral data, territorial reference material, and geographic files remain subject to the attribution and reuse conditions of their original providers.
