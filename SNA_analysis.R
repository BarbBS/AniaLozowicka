# =============================================================================
# International trade network analysis (SNA)
# =============================================================================
# Run from project root: source("SNA_analysis.R")
#
# Project structure:
#   data/                        – input trade data (aggregated export/import)
#   output/plots/                – figures (PNG)
#   output/csv_results/centralities/ – centrality tables (all network variants)
#   output/csv_results/summary/      – summary tables for slides/reports
#   archive/                     – legacy scripts, data, and outputs
# =============================================================================

rm(list = ls())

# ---- Configuration ----
CONFIG <- list(
  data_export            = "data/eksport_agregowany.csv",
  data_import            = "data/import_agregowany.csv",
  output_dir             = "output",
  plots_dir              = "output/plots",
  csv_centralities_dir   = "output/csv_results/centralities",
  csv_summary_dir        = "output/csv_results/summary",
  weight_scale           = 1e6,          # weight scale (million USD)
  node_strength_quantile = 0.10,         # remove bottom 10% of nodes by strength
  edge_weight_quantile   = 0.80,         # keep top 20% of edges by weight
  backbone_alpha         = 0.05,
  backbone_model         = "disparity",  # "disparity" or "lans"
  seed                   = 123
)

output_path <- function(filename, config = CONFIG) {
  file.path(config$output_dir, filename)
}

plot_path <- function(filename, config = CONFIG) {
  file.path(config$plots_dir, filename)
}

csv_centralities_path <- function(filename, config = CONFIG) {
  file.path(config$csv_centralities_dir, filename)
}

csv_summary_path <- function(filename, config = CONFIG) {
  file.path(config$csv_summary_dir, filename)
}

ensure_output_dirs <- function(config = CONFIG) {
  dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$plots_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$csv_centralities_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$csv_summary_dir, showWarnings = FALSE, recursive = TRUE)
}

CONTINENT_COLORS <- c(
  "Europe"     = "skyblue",
  "Asia"       = "gold",
  "Americas"   = "palegreen3",
  "Africa"     = "tomato",
  "Oceania"    = "orchid",
  "Antarctica" = "lightgray"
)

# Manual mappings for encoding issues and UN Comtrade abbreviations
CUSTOM_MATCH_CONTINENT <- c(
  "CA´te d'Ivoire"              = "Africa",
  "CuraA§ao"                    = "Americas",
  "CuraÃ§ao"                    = "Americas",
  "Saint BarthA©lemy"           = "Americas",
  "Saint BarthÃ©lemy"           = "Americas",
  "TA1rkiye"                    = "Asia",
  "TÃ¼rkiye"                    = "Asia",
  "Br. Indian Ocean Terr."      = "Asia",
  "Br. Virgin Isds"             = "Americas",
  "FS Micronesia"               = "Oceania",
  "Other Asia, nes"             = "Asia",
  "N. Mariana Isds"             = "Oceania",
  "Wallis and Futuna Isds"      = "Oceania",
  "Cocos Isds"                  = "Oceania",
  "Fr. South Antarctic Terr."   = "Antarctica",
  "Bonaire"                     = "Americas"
)

CUSTOM_MATCH_ISO3 <- c(
  "CA´te d'Ivoire"              = "CIV",
  "CuraA§ao"                    = "CUW",
  "CuraÃ§ao"                    = "CUW",
  "Saint BarthA©lemy"           = "BLM",
  "Saint BarthÃ©lemy"           = "BLM",
  "TA1rkiye"                    = "TUR",
  "TÃ¼rkiye"                    = "TUR",
  "Br. Indian Ocean Terr."      = "IOT",
  "Br. Virgin Isds"             = "VGB",
  "FS Micronesia"               = "FSM",
  "Other Asia, nes"             = NA,
  "N. Mariana Isds"             = "MNP",
  "Wallis and Futuna Isds"      = "WLF",
  "Cocos Isds"                  = "CCK",
  "Fr. South Antarctic Terr."   = "ATF",
  "Bonaire"                     = "BES"
)

# ---- Libraries ----
required_pkgs <- c(
  "igraph", "ggplot2", "ggraph", "dplyr", "stringr",
  "ggrepel", "patchwork", "countrycode", "backbone"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall with: install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))"
  )
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

set.seed(CONFIG$seed)

# =============================================================================
# Helper functions
# =============================================================================

TRADE_LABELS <- c(ex = "Export", im = "Import")

load_trade_data <- function(export_file, import_file) {
  data_ex <- read.csv(export_file, sep = ",", dec = ".", stringsAsFactors = FALSE)
  data_im <- read.csv(import_file, sep = ",", dec = ".", stringsAsFactors = FALSE)

  names(data_ex) <- c("Source", "Target", "Value")
  names(data_im) <- c("Source", "Target", "Value")

  data_ex <- data_ex[data_ex$Source != data_ex$Target, ]
  data_im <- data_im[data_im$Source != data_im$Target, ]

  list(export = data_ex, import = data_im)
}

build_trade_graph <- function(edge_df, weight_scale = 1e6) {
  g <- graph_from_data_frame(edge_df, directed = TRUE)
  E(g)$weight <- E(g)$Value / weight_scale
  epsilon <- 1e-6
  E(g)$distance <- 1 / (E(g)$weight + epsilon)
  g
}

annotate_countries <- function(g) {
  country_nodes <- V(g)$name

  V(g)$continent <- countrycode(
    sourcevar = country_nodes,
    origin = "country.name",
    destination = "continent",
    custom_match = CUSTOM_MATCH_CONTINENT
  )

  V(g)$iso3 <- countrycode(
    sourcevar = country_nodes,
    origin = "country.name",
    destination = "iso3c",
    custom_match = CUSTOM_MATCH_ISO3
  )

  V(g)$label <- ifelse(is.na(V(g)$iso3), substr(V(g)$name, 1, 3), V(g)$iso3)
  V(g)$color <- unname(CONTINENT_COLORS[V(g)$continent])
  V(g)$strength <- strength(g, mode = "out", weights = E(g)$Value)

  unmatched <- country_nodes[is.na(V(g)$continent)]
  if (length(unmatched) > 0) {
    message("Countries without continent mapping: ", paste(unmatched, collapse = ", "))
  }

  g
}

filter_by_node_strength <- function(g, quantile_cut = 0.10) {
  threshold <- quantile(V(g)$strength, probs = quantile_cut, na.rm = TRUE)
  weak_nodes <- V(g)[V(g)$strength < threshold]
  g_filtered <- delete_vertices(g, weak_nodes)
  message(
    "Node filtering (strength < ", round(threshold, 2), "): ",
    vcount(g), " -> ", vcount(g_filtered), " nodes, ",
    ecount(g), " -> ", ecount(g_filtered), " edges"
  )
  g_filtered
}

filter_by_edge_weight <- function(g, quantile_cut = 0.80) {
  threshold <- quantile(E(g)$weight, probs = quantile_cut, na.rm = TRUE)
  g_pruned <- delete_edges(g, E(g)[weight < threshold])
  g_pruned <- delete_vertices(g_pruned, degree(g_pruned, mode = "all") == 0)
  message(
    "Edge filtering (weight >= ", round(threshold, 4), " mln): ",
    ecount(g), " -> ", ecount(g_pruned), " edges, ",
    vcount(g), " -> ", vcount(g_pruned), " nodes"
  )
  g_pruned
}

apply_backbone_filter <- function(g, model = "disparity", alpha = 0.05) {
  adj_matrix <- as.matrix(as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  bb_matrix <- backbone_from_weighted(adj_matrix, model = model, alpha = alpha)

  weighted_bb <- adj_matrix * bb_matrix
  g_bb <- graph_from_adjacency_matrix(
    weighted_bb, mode = "directed", weighted = TRUE, diag = FALSE
  )

  if (ecount(g_bb) > 0) {
    g_bb <- delete_edges(g_bb, E(g_bb)[weight == 0])
  }
  g_bb <- delete_vertices(g_bb, degree(g_bb, mode = "all") == 0)

  # Copy vertex attributes from the original network
  orig_attrs <- igraph::as_data_frame(g, what = "vertices")
  rownames(orig_attrs) <- orig_attrs$name
  for (vname in V(g_bb)$name) {
    if (vname %in% rownames(orig_attrs)) {
      idx <- which(V(g_bb)$name == vname)
      V(g_bb)[idx]$continent <- orig_attrs[vname, "continent"]
      V(g_bb)[idx]$iso3 <- orig_attrs[vname, "iso3"]
      V(g_bb)[idx]$label <- orig_attrs[vname, "label"]
      V(g_bb)[idx]$color <- orig_attrs[vname, "color"]
    }
  }
  V(g_bb)$strength <- strength(g_bb, mode = "out", weights = E(g_bb)$weight)
  epsilon <- 1e-6
  E(g_bb)$distance <- 1 / (E(g_bb)$weight + epsilon)

  message(
    "Backbone (", model, ", alpha=", alpha, "): ",
    ecount(g), " -> ", ecount(g_bb), " edges, ",
    vcount(g), " -> ", vcount(g_bb), " nodes"
  )
  g_bb
}

compare_strength_thresholds <- function(g, quantiles = c(0.05, 0.10, 0.15)) {
  strengths <- strength(g, mode = "all", weights = E(g)$weight)
  thresholds <- quantile(strengths, quantiles, na.rm = TRUE)

  results <- lapply(thresholds, function(thr) {
    keep_nodes <- which(strengths >= thr)
    g_sub <- induced_subgraph(g, keep_nodes)
    list(
      threshold = thr,
      nodes = vcount(g_sub),
      edges = ecount(g_sub),
      giant_component = max(components(g_sub)$csize)
    )
  })
  names(results) <- paste0("q", quantiles)
  results
}

compute_centralities <- function(g, weight_scale = CONFIG$weight_scale) {
  if (vcount(g) == 0) {
    return(tibble::tibble(
      name = character(), continent = character(), iso3 = character(),
      label = character(), strength = numeric(), eigenv = numeric(),
      between = numeric(), closen = numeric()
    ))
  }

  if (!"distance" %in% edge_attr_names(g)) {
    epsilon <- 1e-6
    E(g)$distance <- 1 / (E(g)$weight + epsilon)
  }

  trade_weights <- if ("Value" %in% edge_attr_names(g)) {
    E(g)$Value
  } else {
    E(g)$weight * weight_scale
  }

  nodes_df <- igraph::as_data_frame(g, what = "vertices")
  id_cols <- intersect(c("name", "continent", "iso3", "label", "color"), names(nodes_df))

  nodes_df %>%
    mutate(
      strength = igraph::strength(g, mode = "out", weights = trade_weights),
      eigenv   = eigen_centrality(g, weights = E(g)$weight)$vector,
      between  = betweenness(g, weights = E(g)$distance),
      closen   = closeness(g, weights = E(g)$distance, mode = "out")
    ) %>%
    select(all_of(id_cols), strength, eigenv, between, closen)
}

save_variant_centralities <- function(full, nodes_filtered, pruned, backbone,
                                      prefix, config = CONFIG) {
  graphs <- list(
    full = full,
    filtered = nodes_filtered,
    pruned = pruned,
    backbone = backbone
  )
  variant_labels <- c(
    full     = "1. Full network",
    filtered = "2. After node filter",
    pruned   = "3. After edge filter",
    backbone = paste0("4. Backbone (", config$backbone_model, ")")
  )

  centrality_tables <- lapply(names(graphs), function(key) {
    compute_centralities(graphs[[key]], config$weight_scale) %>%
      mutate(variant = variant_labels[[key]], .before = 1)
  })
  names(centrality_tables) <- names(graphs)

  combined <- dplyr::bind_rows(centrality_tables)
  write.csv2(
    combined,
    csv_centralities_path(paste0("centralities_", prefix, ".csv")),
    row.names = FALSE
  )

  for (key in names(graphs)) {
    write.csv2(
      centrality_tables[[key]] %>% select(-variant),
      csv_centralities_path(paste0("nodes_", prefix, "_", key, ".csv")),
      row.names = FALSE
    )
  }

  # Backward-compatible alias: node-filtered network (used for DA input)
  filtered_only <- centrality_tables$filtered %>% select(-variant)
  write.csv2(filtered_only, csv_centralities_path(paste0("nodes_", prefix, ".csv")), row.names = FALSE)

  message(
    "Saved centralities for ", length(graphs), " variants -> centralities_", prefix, ".csv"
  )

  list(combined = combined, filtered = filtered_only, by_variant = centrality_tables)
}

attach_vertex_attrs_from_ref <- function(g_bb, g_ref) {
  ref_attrs <- igraph::as_data_frame(g_ref, what = "vertices")
  rownames(ref_attrs) <- ref_attrs$name

  for (vname in V(g_bb)$name) {
    if (!vname %in% rownames(ref_attrs)) next
    idx <- which(V(g_bb)$name == vname)
    V(g_bb)[idx]$continent <- ref_attrs[vname, "continent"]
    V(g_bb)[idx]$color <- ref_attrs[vname, "color"]
    V(g_bb)[idx]$label <- ref_attrs[vname, "label"]
    V(g_bb)[idx]$strength <- ref_attrs[vname, "strength"]
  }

  missing_color <- is.na(V(g_bb)$color) | V(g_bb)$color == ""
  V(g_bb)$color[missing_color] <- "white"
  g_bb
}

build_backbone_graph <- function(g_ref, model, alpha = 0.05) {
  adj_matrix <- as.matrix(as_adjacency_matrix(g_ref, attr = "weight", sparse = FALSE))
  bb_matrix <- backbone_from_weighted(adj_matrix, model = model, alpha = alpha)
  g_bb <- graph_from_adjacency_matrix(bb_matrix, mode = "directed", diag = FALSE)
  attach_vertex_attrs_from_ref(g_bb, g_ref)
}

layout_for_graph <- function(g, layout_ref) {
  coords <- layout_ref[V(g)$name, , drop = FALSE]
  coords[is.na(coords)] <- 0
  coords
}

plot_backbone_panel <- function(g, layout_coords, title) {
  ggraph(g, layout = layout_coords) +
    geom_edge_link(color = "grey55", alpha = 0.25, arrow = arrow(length = unit(1.5, "mm"))) +
    geom_node_point(aes(size = strength, fill = continent), shape = 21, color = "black") +
    scale_fill_manual(values = CONTINENT_COLORS, name = "Continent", na.value = "white") +
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    labs(title = title) +
    theme_graph() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
}

plot_backbone_comparison <- function(g_ref, alpha = 0.05, filename = NULL) {
  set.seed(CONFIG$seed)
  layout_ref <- layout_with_fr(g_ref)
  rownames(layout_ref) <- V(g_ref)$name

  g_lans <- build_backbone_graph(g_ref, model = "lans", alpha = alpha)
  g_disp <- build_backbone_graph(g_ref, model = "disparity", alpha = alpha)

  panel_lans <- plot_backbone_panel(
    g_lans,
    layout_for_graph(g_lans, layout_ref),
    paste("Backbone LANS (alpha =", alpha, ")")
  )
  panel_disp <- plot_backbone_panel(
    g_disp,
    layout_for_graph(g_disp, layout_ref),
    paste("Backbone Disparity (alpha =", alpha, ")")
  )

  combined <- panel_lans + panel_disp + plot_layout(guides = "collect")

  if (!is.null(filename)) {
    ggsave(plot_path(basename(filename)), plot = combined, width = 16, height = 8, dpi = 300, bg = "white")
  }

  invisible(list(plot = combined, lans = g_lans, disparity = g_disp))
}

plot_trade_network <- function(g, layout = "fr", title = NULL) {
  ggraph(g, layout = layout) +
    geom_edge_link(
      aes(width = weight, alpha = weight),
      color = "grey50",
      show.legend = FALSE
    ) +
    geom_node_point(
      aes(size = strength, fill = continent),
      shape = 21,
      color = "black"
    ) +
    geom_node_text(aes(label = label), repel = TRUE, size = 2.5, max.overlaps = Inf) +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    scale_edge_alpha_continuous(range = c(0.05, 0.4)) +
    scale_size_continuous(range = c(1.5, 8), name = "Strength") +
    scale_fill_manual(values = CONTINENT_COLORS, name = "Continent", na.value = "white") +
    labs(title = title) +
    theme_graph()
}

save_network_plot <- function(g, filename, layout = "fr", title = NULL,
                              width = 14, height = 10) {
  p <- plot_trade_network(g, layout = layout, title = title)
  ggsave(plot_path(filename), plot = p, width = width, height = height, dpi = 300, bg = "white")
  invisible(p)
}

plot_top5_bars <- function(nodes_df, metric, title, fill_var = "continent") {
  nodes_df %>%
    slice_max(order_by = .data[[metric]], n = 5) %>%
    ggplot(aes(x = .data[[metric]], y = reorder(name, .data[[metric]]))) +
    geom_col(aes(fill = .data[[fill_var]]), color = "black") +
    scale_fill_manual(values = CONTINENT_COLORS, name = "Continent", na.value = "white") +
    labs(title = title, x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
}

plot_role_map <- function(nodes_df, title) {
  plot_df <- nodes_df %>%
    filter(strength > 0, between > 0)

  median_strength <- median(plot_df$strength, na.rm = TRUE)
  median_between  <- median(plot_df$between, na.rm = TRUE)

  ggplot(plot_df, aes(x = strength, y = between)) +
    geom_hline(yintercept = median_between, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = median_strength, linetype = "dashed", color = "grey50") +
    geom_point(aes(color = continent, size = strength), alpha = 0.7) +
    geom_text_repel(aes(label = name), size = 3, max.overlaps = 20) +
    scale_color_manual(values = CONTINENT_COLORS, name = "Continent", na.value = "grey") +
    scale_size(guide = "none") +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = title,
      x = "Strength (log scale)",
      y = "Betweenness (log scale)"
    ) +
    theme_minimal(base_size = 12)
}

collect_network_stats <- function(results_ex, results_im, config = CONFIG) {
  variants <- c(
    "1. Full network",
    "2. After node filter",
    "3. After edge filter",
    paste0("4. Backbone (", config$backbone_model, ")")
  )

  tibble::tibble(
    variant = rep(variants, 2),
    direction = rep(c("Export", "Import"), each = length(variants)),
    nodes = c(
      vcount(results_ex$full), vcount(results_ex$nodes_filtered), vcount(results_ex$pruned),
      vcount(results_ex$backbone),
      vcount(results_im$full), vcount(results_im$nodes_filtered), vcount(results_im$pruned),
      vcount(results_im$backbone)
    ),
    edges = c(
      ecount(results_ex$full), ecount(results_ex$nodes_filtered), ecount(results_ex$pruned),
      ecount(results_ex$backbone),
      ecount(results_im$full), ecount(results_im$nodes_filtered), ecount(results_im$pruned),
      ecount(results_im$backbone)
    )
  )
}

plot_network_summary_slide <- function(stats_df, config = CONFIG) {
  stats_df <- stats_df %>%
    mutate(
      variant = factor(variant, levels = unique(variant)),
      direction = factor(direction, levels = c("Export", "Import"))
    )

  fmt <- function(x) format(x, big.mark = " ", scientific = FALSE)

  p_nodes <- ggplot(stats_df, aes(x = variant, y = nodes, fill = direction)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "black") +
    geom_text(
      aes(label = fmt(nodes)),
      position = position_dodge(width = 0.75),
      vjust = -0.4,
      size = 3.5
    ) +
    scale_fill_manual(values = c("Export" = "steelblue", "Import" = "coral")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Number of nodes (countries)", x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 9, angle = 15, hjust = 1)
    )

  p_edges <- ggplot(stats_df, aes(x = variant, y = edges, fill = direction)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "black") +
    geom_text(
      aes(label = fmt(edges)),
      position = position_dodge(width = 0.75),
      vjust = -0.4,
      size = 3.5
    ) +
    scale_fill_manual(values = c("Export" = "steelblue", "Import" = "coral")) +
    scale_y_continuous(
      trans = "log10",
      labels = scales::label_number(big.mark = " "),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(title = "Number of edges (trade links)", x = NULL, y = "log scale") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 9, angle = 15, hjust = 1)
    )

  title <- paste0(
    "Characteristics of four trade network variants\n",
    "(node filter: bottom ", config$node_strength_quantile * 100,
    "% strength; edge filter: top ", (1 - config$edge_weight_quantile) * 100,
    "% weights; backbone: ", config$backbone_model, ", alpha = ", config$backbone_alpha, ")"
  )

  (p_nodes / p_edges) +
    plot_annotation(title = title, theme = theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)))
}

run_pipeline <- function(edge_df, prefix, config = CONFIG) {
  trade_label <- TRADE_LABELS[[prefix]]
  message("\n========== ", trade_label, " ==========")

  g_full <- build_trade_graph(edge_df, config$weight_scale)
  g_full <- annotate_countries(g_full)

  message("Full network: ", vcount(g_full), " nodes, ", ecount(g_full), " edges")

  # --- Strength threshold analysis (LeapSpace) ---
  threshold_results <- compare_strength_thresholds(g_full)
  print(do.call(rbind, lapply(threshold_results, as.data.frame)))

  # --- Node filtering (SNA_code1) ---
  g_nodes <- filter_by_node_strength(g_full, config$node_strength_quantile)

  # --- Edge filtering – visualization variant (SNA_code1) ---
  g_pruned <- filter_by_edge_weight(g_nodes, config$edge_weight_quantile)

  # --- Backbone – structural variant (LeapSpace) ---
  g_backbone <- apply_backbone_filter(
    g_nodes,
    model = config$backbone_model,
    alpha = config$backbone_alpha
  )

  # --- Centralities for all four network variants ---
  centrality_results <- save_variant_centralities(
    full = g_full,
    nodes_filtered = g_nodes,
    pruned = g_pruned,
    backbone = g_backbone,
    prefix = prefix,
    config = config
  )
  nodes_df <- centrality_results$filtered

  # --- Visualizations ---
  save_network_plot(
    g_nodes, paste0("network_", prefix, "_filtered.png"),
    layout = "kk",
    title = paste0(trade_label, " – after node filter (bottom ",
                   config$node_strength_quantile * 100, "% strength)")
  )

  save_network_plot(
    g_pruned, paste0("network_", prefix, "_pruned.png"),
    layout = "fr",
    title = paste0(trade_label, " – top ",
                   (1 - config$edge_weight_quantile) * 100, "% edges by weight")
  )

  save_network_plot(
    g_backbone, paste0("network_", prefix, "_backbone.png"),
    layout = "fr",
    title = paste0(trade_label, " – backbone ", config$backbone_model)
  )

  list(
    full = g_full,
    nodes_filtered = g_nodes,
    pruned = g_pruned,
    backbone = g_backbone,
    nodes = nodes_df,
    centralities = centrality_results$combined,
    threshold_results = threshold_results
  )
}

# =============================================================================
# Main pipeline
# =============================================================================

message("Working directory: ", getwd())
ensure_output_dirs()

trade_data <- load_trade_data(CONFIG$data_export, CONFIG$data_import)

results_ex <- run_pipeline(trade_data$export, "ex")
results_im <- run_pipeline(trade_data$import, "im")

# --- Backbone comparison (LANS vs Disparity) – LeapSpace ---
backbone_comparison <- plot_backbone_comparison(
  results_ex$nodes_filtered,
  alpha = CONFIG$backbone_alpha,
  filename = plot_path("backbone_comparison.png")
)
g_lans <- backbone_comparison$lans
g_disp <- backbone_comparison$disparity

# --- Histogram strength (LeapSpace) ---
png(plot_path("strength_hist.png"), width = 800, height = 600)
hist(
  strength(results_ex$full, mode = "all"),
  breaks = 30, col = "steelblue",
  main = "Strength distribution – export (full network)",
  xlab = "strength"
)
dev.off()

# --- Top 5 bar charts and role maps ---
top_strength_ex  <- plot_top5_bars(results_ex$nodes, "strength",  "Export – Top 5 strength")
top_between_ex   <- plot_top5_bars(results_ex$nodes, "between",   "Export – Top 5 betweenness")
top_eigen_ex     <- plot_top5_bars(results_ex$nodes, "eigenv",    "Export – Top 5 eigenvector")
top_strength_im  <- plot_top5_bars(results_im$nodes, "strength",  "Import – Top 5 strength")
top_between_im   <- plot_top5_bars(results_im$nodes, "between",   "Import – Top 5 betweenness")
top_eigen_im     <- plot_top5_bars(results_im$nodes, "eigenv",    "Import – Top 5 eigenvector")

combined_top5 <- (top_strength_ex + top_between_ex + top_eigen_ex) /
                 (top_strength_im + top_between_im + top_eigen_im)

ggsave(plot_path("top5_centralities_combined.png"), plot = combined_top5, width = 15, height = 8, dpi = 300)

role_map_ex <- plot_role_map(results_ex$nodes, "Role map – export")
role_map_im <- plot_role_map(results_im$nodes, "Role map – import")
ggsave(plot_path("role_map_export.png"), plot = role_map_ex, width = 10, height = 8, dpi = 300)
ggsave(plot_path("role_map_import.png"), plot = role_map_im, width = 10, height = 8, dpi = 300)

# --- Slide: network variant characteristics ---
network_stats <- collect_network_stats(results_ex, results_im)
network_summary_slide <- plot_network_summary_slide(network_stats)
ggsave(
  plot_path("network_summary_slide.png"),
  plot = network_summary_slide,
  width = 14, height = 8, dpi = 300, bg = "white"
)
write.csv2(network_stats, csv_summary_path("network_stats.csv"), row.names = FALSE)

# --- Summary ---
cat("\n=== SUMMARY ===\n")
cat("Export – nodes:", vcount(results_ex$nodes_filtered),
    "| edges (pruned):", ecount(results_ex$pruned),
    "| edges (backbone):", ecount(results_ex$backbone), "\n")
cat("Import – nodes:", vcount(results_im$nodes_filtered),
    "| edges (pruned):", ecount(results_im$pruned),
    "| edges (backbone):", ecount(results_im$backbone), "\n")
cat("\nOutput files:\n")
cat("  output/plots/                          – all figures (PNG)\n")
cat("  output/csv_results/centralities/       – centralities_*.csv, nodes_*.csv\n")
cat("  output/csv_results/summary/            – network_stats.csv\n")
cat("  output/SNA_analysis_results.RData\n")

threshold_results <- results_ex$threshold_results
save(
  results_ex, results_im, CONFIG, threshold_results,
  file = output_path("SNA_analysis_results.RData")
)
message("\nSaved: ", output_path("SNA_analysis_results.RData"))
