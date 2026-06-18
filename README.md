# International Trade Network Analysis (SNA)

Social network analysis of bilateral merchandise trade between countries. The pipeline builds export and import networks, applies filtering methods to improve visualization, computes centrality measures, and exports results for downstream analysis (e.g. discriminant analysis).

## Quick start

```r
setwd("path/to/AniaLozowicka")
source("SNA_analysis.R")
```

Requires R packages: `igraph`, `ggplot2`, `ggraph`, `dplyr`, `stringr`, `ggrepel`, `patchwork`, `countrycode`, `backbone`.

## Project structure

```
AniaLozowicka/
├── SNA_analysis.R          # Main analysis script (run this)
├── AniaLozowicka.Rproj     # RStudio project file
├── data/                   # Input data
├── output/                 # Generated results (plots, tables)
└── archive/                # Legacy scripts and older outputs
    ├── scripts/            # SNA_code.R, SNA_code1.R, SNA_code_LeapSpace.R
    ├── data/               # Original smaller trade datasets
    └── output/             # Plots from earlier runs
```

## Input data (`data/`)

| File | Description |
|------|-------------|
| `eksport_agregowany.csv` | Aggregated export flows: exporting country → importing country, trade value |
| `import_agregowany.csv` | Aggregated import flows: importing country ← exporting country, trade value |

Columns are renamed internally to `Source`, `Target`, `Value`.

## Network variants

The script builds **four variants** of each network (export and import):

| Variant | Description |
|---------|-------------|
| **Full network** | All countries and trade links (~226 nodes, ~29 000 edges) |
| **After node filter** | Bottom 10% of countries removed by export/import strength |
| **After edge filter** | Top 20% of trade links kept by weight (pruned network) |
| **Backbone (disparity)** | Statistically significant links extracted via backbone filter (α = 0.05) |

Default filter settings are in the `CONFIG` block at the top of `SNA_analysis.R`.

## Output files (`output/`)

### Centrality measures (main analytical output)

Four centrality measures are computed for **each network variant**:

- **strength** — total outgoing trade volume
- **eigenv** — eigenvector centrality
- **between** — betweenness centrality
- **closen** — closeness centrality

| File | Content |
|------|---------|
| `centralities_ex.csv` | Export — all variants (long format, column `variant`) |
| `centralities_im.csv` | Import — all variants |
| `nodes_ex.csv`, `nodes_im.csv` | Node-filtered network only (recommended input for DA) |
| `nodes_ex_full.csv` … `nodes_ex_backbone.csv` | Export centralities per variant |
| `nodes_im_full.csv` … `nodes_im_backbone.csv` | Import centralities per variant |

### Network visualizations

| File | Content |
|------|---------|
| `network_ex_filtered.png` | Export after node filter |
| `network_ex_pruned.png` | Export after edge filter |
| `network_ex_backbone.png` | Export backbone network |
| `network_im_*.png` | Same for import |
| `backbone_comparison.png` | LANS vs Disparity backbone (export, nodes coloured by continent) |

### Summary and diagnostics

| File | Content |
|------|---------|
| `network_summary_slide.png` | Bar chart: nodes and edges across all four variants |
| `network_stats.csv` | Table of node/edge counts per variant |
| `top5_centralities_combined.png` | Top 5 countries by strength, betweenness, eigenvector |
| `role_map_export.png`, `role_map_import.png` | Strength vs betweenness scatter plots |
| `strength_hist.png` | Distribution of node strength (full export network) |

`SNA_analysis_results.RData` is created locally when the script runs but is not tracked in git (regenerable).

## Archive (`archive/`)

Previous versions of the code and results, kept for reference:

- **scripts/** — original pipeline (`SNA_code.R`), aggregated-data version (`SNA_code1.R`), and backbone experiments (`SNA_code_LeapSpace.R`)
- **data/** — smaller country sample (`MerchandiseTrade_Export/Import.csv`)
- **output/** — plots and tables from earlier runs

## Notes

- Betweenness and closeness use edge **distance** = 1 / (weight + ε); eigenvector centrality and strength use trade **volume**.
- Country names are mapped to continents via the `countrycode` package, with manual fixes for UN Comtrade encoding issues.
- All plot labels and table headers are in English.

## Repository

https://github.com/BarbBS/AniaLozowicka
