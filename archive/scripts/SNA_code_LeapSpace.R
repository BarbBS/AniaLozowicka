# 1.
library(igraph)
library(backbone)

# 2. Simulated data

set.seed(123)

n <- 60
# Symulujemy macierz o rozk³adzie zbli¿onym do handlu (du¿o ma³ych wartoœci, kilka du¿ych)
W <- matrix(rpois(n*n, lambda = 3), n, n)
diag(W) <- 0  # brak handlu z samym sob¹


# 3. Backbone: LANS Filter 
# ------------------------------------------------------------------------------
# The backbone_from_weighted() function automatically extracts the network.
# Since 'W' is a matrix, bb_lans_matrix is returned directly as a binary matrix.
bb_lans_matrix <- backbone_from_weighted(W, model = "lans", alpha = 0.05)

# Convert directly to an igraph object
g_lans <- graph_from_adjacency_matrix(bb_lans_matrix, mode = "directed")

# ------------------------------------------------------------------------------
# 4. Backbone: Disparity Filter
# ------------------------------------------------------------------------------
bb_disp_matrix <- backbone_from_weighted(W, model = "disparity", alpha = 0.05)

# Convert directly to an igraph object
g_disp <- graph_from_adjacency_matrix(bb_disp_matrix, mode = "directed")
############################################
# 5. Strength (pe³na sieæ)
############################################
g_full <- graph_from_adjacency_matrix(W, mode="directed", weighted=TRUE)
strengths <- strength(g_full, mode="all")

############################################
# 6. Analiza rozk³adu strength
############################################
png("strength_hist.png", width=800, height=600)
hist(strengths, breaks=20, col="steelblue", main="Rozk³ad si³y (strength)", xlab="strength")
dev.off()

############################################
quantiles <- quantile(strengths, c(0.05, 0.10, 0.15))
thresholds <- as.numeric(quantiles)

results <- list()

for (thr in thresholds) {
  keep_nodes <- which(strengths >= thr)
  g_sub <- induced_subgraph(g_full, keep_nodes)
  
  results[[paste0("threshold_", thr)]] <- list(
    nodes = vcount(g_sub),
    edges = ecount(g_sub),
    giant_component = max(components(g_sub)$csize)
  )
}

# ------------------------------------------------------------------------------
# 8. Wykres: porównanie backbone LANS i Disparity
# ------------------------------------------------------------------------------
png("backbone_comparison.png", width = 1000, height = 500)
par(mfrow = c(1, 2))

# Updated variable name from g_nc to g_lans and corrected the plot title
plot(g_lans, main = "Backbone LANS", vertex.size = 5, vertex.label = NA)
plot(g_disp, main = "Backbone Disparity", vertex.size = 5, vertex.label = NA)

dev.off()

# ------------------------------------------------------------------------------
# 9. Finalny eksport wyników
# ------------------------------------------------------------------------------
# Updated variable name from g_nc to g_lans in the export function
save(g_lans, g_disp, strengths, results, file = "analysis_results.RData")