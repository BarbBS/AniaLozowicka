# Evaluating the Efficiency of Europe and Asia Trade Networks: A Two-Stage SNA–DEA Approach

Research project combining **Social Network Analysis (SNA)** and **Data Envelopment Analysis (DEA)** to study the structure and efficiency of international merchandise trade networks. The pipeline builds weighted trade networks from bilateral flows, extracts centrality measures under several network definitions, and prepares outputs for efficiency analysis and conference presentation.

**Authors:** Anna Łozowicka, Barbara Będowska-Sójka (Poznań University of Economics and Business)

## Overview

The workflow has two stages:

1. **SNA (`SNA_analysis.R`)** — constructs export and import networks from aggregated BACI trade data (~226 countries), applies filtering and backbone extraction to obtain readable network views, computes four centrality measures per network variant, and exports figures and CSV tables.
2. **DEA (`DEA_analysis.R`)** — work in progress; loads SNA outputs (centrality measures as DEA outputs) and merges them with macroeconomic inputs (GDP, FDI, labour, emissions, energy) for efficiency analysis.

Results are written to `output/`: figures in `plots/`, detailed tables in `csv_results/centralities/`, and summary tables for slides in `csv_results/summary/`. The Beamer presentation `output/SNA_DEA_pres.tex` uses figures from `plots/`.

## Quick start

Run from the project root (or open `AniaLozowicka.Rproj` in RStudio):

```r
setwd("path/to/AniaLozowicka")
source("SNA_analysis.R")   # Stage 1: full SNA pipeline
source("DEA_analysis.R")   # Stage 2: DEA prep (partial)
```

**Required R packages:** `igraph`, `ggplot2`, `ggraph`, `dplyr`, `stringr`, `ggrepel`, `patchwork`, `countrycode`, `backbone`.

## Project structure

```
AniaLozowicka/
├── SNA_analysis.R          # Main SNA pipeline (run first)
├── DEA_analysis.R          # DEA preparation (uses SNA outputs)
├── AniaLozowicka.Rproj
├── data/                   # Input data
│   ├── eksport_agregowany.csv
│   ├── import_agregowany.csv
│   ├── 1GDPTotal_2024.csv
│   ├── 2FDI_InwardStock_2024.csv
│   ├── 3LabourForce_Data.csv
│   ├── 4GHGemisions_Data.csv
│   ├── 5primary-energy-consumption.csv
│   └── CEPII_BACI_Readme.txt
├── output/                 # Generated results
│   ├── plots/              # All figures (PNG)
│   ├── csv_results/
│   │   ├── centralities/   # Centrality and node tables (analytical / DEA input)
│   │   └── summary/        # Summary tables for slides and reports
│   ├── SNA_DEA_pres.tex    # Beamer presentation (SNA + DEA slides)
│   └── SNA_analysis_results.RData   # Full R workspace (local, not in git)
├── DEA_results/
│   └── inputs_complete.xlsx
└── archive/                # Legacy scripts and older outputs
    ├── scripts/
    ├── data/
    └── output/
```

## Input data (`data/`)

### Trade flows (SNA)

| File | Description |
|------|-------------|
| `eksport_agregowany.csv` | Aggregated export flows: exporting country → importing country, trade value (USD) |
| `import_agregowany.csv` | Aggregated import flows: importing country ← exporting country, trade value (USD) |

Source: [CEPII BACI](https://www.cepii.fr/DATA_DOWNLOAD/baci/doc/baci_webpage.html) database, aggregated to country–country level. Columns are renamed internally to `Source`, `Target`, `Value`.

### Macroeconomic indicators (DEA)

| File | Variable |
|------|----------|
| `1GDPTotal_2024.csv` | GDP |
| `2FDI_InwardStock_2024.csv` | FDI inward stock |
| `3LabourForce_Data.csv` | Labour force |
| `4GHGemisions_Data.csv` | GHG emissions |
| `5primary-energy-consumption.csv` | Primary energy consumption |

## Network variants (SNA)

For each trade direction (export and import), the script builds **four network variants**:

| Variant | Description |
|---------|-------------|
| **Full network** | All countries and trade links (~226 nodes, ~29 000 edges) |
| **After node filter** | Bottom 10% of countries removed by export/import strength |
| **After edge filter (pruned)** | Top 20% of trade links kept by weight |
| **Backbone (disparity)** | Statistically significant links (α = 0.05, disparity filter) |

Filter thresholds are set in the `CONFIG` block at the top of `SNA_analysis.R`.

### Centrality measures

Computed for every variant and every country:

| Measure | Meaning |
|---------|---------|
| **strength** | Total outgoing trade volume |
| **eigenv** | Eigenvector centrality |
| **between** | Betweenness centrality |
| **closen** | Closeness centrality |

Betweenness and closeness use edge **distance** = 1 / (weight + ε); eigenvector centrality and strength use trade **volume**. Countries are assigned to continents via the `countrycode` package, with manual fixes for UN Comtrade encoding issues.

## Output files (`output/`)

### Directory layout

| Path | Role |
|------|------|
| `output/plots/` | All figures — network maps, summary charts, role maps, histograms |
| `output/csv_results/centralities/` | Detailed analytical tables — centralities and per-variant node lists; **input for DEA** |
| `output/csv_results/summary/` | Compact summary tables used in slides and reports |
| `output/SNA_analysis_results.RData` | Saved R objects from the SNA run (regenerable, not tracked in git) |

### CSV — analytical data (`csv_results/centralities/`)

| File | Content |
|------|---------|
| `centralities_ex.csv`, `centralities_im.csv` | All variants in long format (column `variant`) |
| `nodes_ex.csv`, `nodes_im.csv` | Node-filtered network only (default input for downstream analysis) |
| `nodes_ex_full.csv`, `nodes_ex_filtered.csv`, `nodes_ex_pruned.csv`, `nodes_ex_backbone.csv` | Export centralities per variant |
| `nodes_im_full.csv`, … | Same for import |

### CSV — summary for presentation (`csv_results/summary/`)

| File | Content |
|------|---------|
| `network_stats.csv` | Node and edge counts for each network variant (export and import) |

### Figures (`plots/`)

| File | Content |
|------|---------|
| `network_ex_filtered.png`, `network_ex_pruned.png`, `network_ex_backbone.png` | Export network visualizations |
| `network_im_*.png` | Same for import |
| `backbone_comparison.png` | LANS vs disparity backbone (export) |
| `network_summary_slide.png` | Bar chart: nodes and edges across variants |
| `top5_centralities_combined.png` | Top 5 countries by strength, betweenness, eigenvector |
| `role_map_export.png`, `role_map_import.png` | Strength vs betweenness scatter plots |
| `strength_hist.png` | Distribution of node strength (full export network) |

All plot labels and table headers are in **English**.

### Presentation

`output/SNA_DEA_pres.tex` is a Beamer slide deck (16:9). SNA figures are loaded from `plots/` via `\graphicspath`. Compile from the `output/` directory:

```bash
cd output && pdflatex SNA_DEA_pres.tex
```

## Archive (`archive/`)

Earlier versions of the code and results, kept for reference:

- **scripts/** — original pipelines (`SNA_code.R`, `SNA_code1.R`, `SNA_code_LeapSpace.R`)
- **data/** — smaller country sample (`MerchandiseTrade_Export/Import.csv`)
- **output/** — plots and tables from earlier runs

## Repository

https://github.com/BarbBS/AniaLozowicka
