Complete territorial partition atlas
The exhaustive territorial partition atlas is distributed through the latest GitHub release as:
```text
complete_cluster_atlas.zip
```
It covers:
```text
19 elections
× 6 electoral geometries
× 7 clustering resolutions (C = 2,...,8)
= 798 partitions
```
Each partition is exported in both PDF and PNG format, for 1,596 figure files in total.
The archive is organized as:
```text
clusters_all/
├── C2/
├── C3/
├── C4/
├── C5/
├── C6/
├── C7/
└── C8/
    └── <geometry>/
        └── map_clusters_<year>_<geometry>_C<resolution>.{pdf,png}
```
The six geometries are:
```text
greenacre_weighted_power_0175
greenacre_weighted_lra
aitchison
alpha_0150
greenacre_unweighted_power_0150
euclidean
```
Selected preferred-geometry, cross-geometry, transition, and instability maps remain directly browsable in the sibling directories under `07_outputs/figures/results/maps/`.
Display colours use maximum-overlap label matching for visual continuity only; this does not alter the underlying partitions or label-invariant comparison statistics.