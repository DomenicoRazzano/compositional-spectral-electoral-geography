Data
This directory contains the source data and territorial material required by the replication pipeline, together with the public harmonized dataset and documentation for large generated datasets distributed through the repository release.
Structure
```text
05_data/
├── raw/
├── lookup/
├── metadata/
├── public/
├── processed/
└── README.md
```
`raw/`
Original Chamber-of-Deputies electoral files used by `06_code/01_data_cleaning.R`, covering 1948–2022. The directory also contains the additional constituency/candidate material required for elections where the ordinary files do not contain the full analytical information.
`lookup/`
Territorial harmonization and classification material. It contains:
actual- and harmonized-province lookup tables;
historical-to-target municipality crosswalks;
election-date-specific territorial reference files;
manual resolutions for ambiguous territorial matches;
list-name corrections;
manual territorial/residual component classifications;
Valle d'Aosta completion material;
ISTAT territorial-change files;
generalized 2022 ISTAT geographic boundaries used for mapping.
Key locations include:
```text
05_data/lookup/province_lookup_actual_harmonized.csv
05_data/lookup/situas_by_election_date/
05_data/lookup/geography/istat_2022_generalized/
```
`metadata/`
Metadata and templates supporting the data-construction workflow.
`public/`
Public harmonized electoral dataset.
Stored directly in the repository:
```text
camera_all_harmonized_public.rds
```
Distributed through the latest GitHub release:
```text
camera_all_harmonized_public.csv
```
Both contain the same public-release table in different storage formats.
`processed/`
Generated intermediate and analysis-ready data. The complete directory is distributed through the latest release as:
```text
processed_data_complete.zip
```
It includes, among other objects:
```text
camera_all_processed.*
camera_all_analytical.*
camera_all_harmonized.*
compositional/
spectral/
validation outputs
audit outputs
```
The principal Stage 04 serialized object is:
```text
05_data/processed/compositional/stage04_composition_distance_objects.rds
```
Stage 05 serialized spectral objects are stored under:
```text
05_data/processed/spectral/
```
Data flow
```text
raw/
  ↓  01_data_cleaning.R
processed analytical archive
  ↓  02_build_province_lookup.R
lookup/
  ↓  03_apply_harmonized_provinces.R
harmonized electoral archive
  ↓  99_public_dataset.R
public/
  ↓  04_build_compositional_geometry.R
processed/compositional/
  ↓  05_graphs.R
processed/spectral/
```
Reconstructing processed files
After restoring the R environment, run:
```r
source(here::here("06_code", "run_all.R"))
```
or download the complete processed-data archive from the latest release.
Third-party source files and geographic material retain the attribution and reuse terms of their original providers.