#clean the environment
rm()

#----Libraries----
library(igraph)
library(ggplot2)
library(ggraph)
library(dplyr)
library(stringr)
library(ggrepel) #labels in ggplot2
library(patchwork) # For combining plots

#----Data download ----
getwd()
setwd("C:/Users/user/OneDrive/Dokumenty/R/SocialNetworks/AniaLozowicka")
list.files()
data_ex <- read.csv("MerchandiseTrade_Export.csv", sep = ";", dec = ".") 
data_im <- read.csv("MerchandiseTrade_Import.csv", sep = ";", dec = ".")

head(data_ex)
names(data_ex) <- c("Source", "Target", "Value")
head(data_im)

is.numeric(data_im$Value)
# removing potential loops
data_ex_no_loops <- data_ex[data_ex$Source != data_ex$Target, ]
data_im_no_loops <- data_im[data_im$Source != data_im$Target, ]
data_im <- data_im_no_loops

#----Network ----
g_ex <- graph_from_data_frame(data_ex, directed = TRUE) 

E(g_ex)$weight <- E(g_ex)$Value / 1000000  # scaling

#----Plot simple -----
png("network_ex.png", width = 600, height = 600)

plot(
  g_ex,
  layout = layout_in_circle(g_ex),     # Algorytm rozmieszczenia wierzcho³ków
  #vertex.size = 30,                # Rozmiar wierzcho³ków
  vertex.color = "lightblue",      # Kolor wierzcho³ków
  vertex.label.color = "black",    # Kolor etykiet wierzcho³ków
  #vertex.label.cex = 0.9,          # Rozmiar czcionki etykiet wierzcho³ków
  
  edge.width = log(E(g_ex)$weight),        # Gruboœæ krawêdzi na podstawie wagi
  edge.arrow.size = 0,                # Rozmiar strza³ek
  edge.label = NA
  #edge.label.color = "darkred",    # Kolor etykiet krawêdzi
  #edge.label.cex = 0.8             # Rozmiar czcionki etykiet krawêdzi
)

dev.off()

#---- Node attributes----
V(g_ex)
region <- c("E", "E", "E", "A", "A", "A", "A", "E", "E", "E", "E","E", "E","E", "E","E", "E","A",
            "E", "E", "A", "E", "E", "E", "E","E", "E","E", "E", "A", "E", "E","E", "E","E", "E","E")
g_ex <- set_vertex_attr(g_ex, "region", value = region)
# Set vertex color by region
V(g_ex)$color <- ifelse(V(g_ex)$region == "E", "skyblue", "gold")
V(g_ex)$strength <- strength(g_ex, mode = "out", weights = E(g_ex)$Value)

#----Edge attribute ----
## inversion of the weights
epsilon <- 1e-6
g_ex <- g_ex %>%
  set_edge_attr("distance", value = 1 / (E(g_ex)$weight + epsilon))
E(g_ex)$distance

#----Plot simple 1 -----
plot(
  g_ex,
  layout = layout_in_circle(g_ex),           # node layout
  vertex.size = sqrt(strength(g_ex, mode = "out")),                # node size
  #vertex.color = "lightblue",               # node color
  vertex.label.color = "black",              # node label color
  #vertex.label.cex = 0.9,                   # size of font
  
  edge.width = log(E(g_ex)$weight),          # edge width
  edge.arrow.size = 0,                       # arrow size
  edge.label = NA
  #edge.label.color = "darkred",             # color of edge labels
  #edge.label.cex = 0.8                      # size of font of edge labels
)

#---- Node list -----
epsilon <- 0.000001 
# Oblicz dystans jako odwrotnoœæ wartoœci handlu
E(g_ex)$distance <- 1 / (E(g_ex)$weight + epsilon)

##----Export----
nodes_list_ex <- as_data_frame(g_ex, what = "vertices")
head(nodes_list_ex) 
nodes_list_ex <- nodes_list_ex %>%
  #mutate(degree = degree(g_ex)) %>%
  mutate(eigenv = eigen_centrality(g_ex, weights = E(g_ex)$weight)$vector) %>%
  mutate(between = betweenness(g_ex, weights = E(g_ex)$distance)) %>% 
  #mutate(closen = closeness(g_ex, weights = E(g_ex)$weight, mode = "out")) %>%
  mutate(closen = closeness(g_ex, weights = E(g_ex)$distance, mode = "out"))
head(nodes_list_ex)

#cor(nodes_list$closen, nodes_list$closen_inv)

#sanodes_list#saving a file with centrality measures
write.csv2(nodes_list_ex, "nodes_ex.csv")

##----Import----
g_im <- graph_from_data_frame(data_im, directed = TRUE) 
g_im <- set_vertex_attr(g_im, "region", value = region)
V(g_im)$color <- ifelse(V(g_im)$region == "E", "skyblue", "gold")
V(g_im)$strength <- strength(g_im, mode = "out", weights = E(g_im)$Value)

E(g_im)$weight <- E(g_im)$Value / 1000000  # scaling
g_im <- g_im %>%
  set_edge_attr("distance", value = 1 / (E(g_im)$weight + epsilon))
E(g_im)$distance 

nodes_list_im <- as_data_frame(g_im, what = "vertices")
nodes_list_im <- nodes_list_im %>%
  #mutate(degree = degree(g_im)) %>%
  mutate(eigenv = eigen_centrality(g_im, weights = E(g_im)$weight)$vector) %>%
  mutate(between = betweenness(g_im, weights = E(g_im)$distance)) %>% 
  #mutate(between = betweenness(g_im, weights = E(g_im)$weight)) %>% 
  #mutate(closen = closeness(g_im, weights = E(g_im)$weight, mode = "out")) %>%
  mutate(closen = closeness(g_im, weights = E(g_im)$distance, mode = "out"))
head(nodes_list_im, 20)
write.csv2(nodes_list_im, "nodes_im.csv")

# Check for exact equality (content and order - should be OK)
identical(nodes_list_ex$name, nodes_list_im$name)
setdiff(nodes_list_ex$name, nodes_list_im$name)


#---- Plots advanced ----
## for export 
# exact same layout every time you run this script.
set.seed(123) 

# --- 1. Clean Names on BOTH graphs FIRST ---
V(g_ex)$name <- stringr::str_trim(V(g_ex)$name)
V(g_im)$name <- stringr::str_trim(V(g_im)$name)

# --- 2. Create ONE Authoritative Layout ---
# Calculate the layout ONCE and save it
layout_ex_named <- layout_with_fr(g_ex)
# Assign the now-CLEAN names to the layout
rownames(layout_ex_named) <- V(g_ex)$name

net_ex <- ggraph(g_ex, layout = layout_ex_named) +
    geom_edge_link(aes(width = weight), alpha = 0.05, color = "grey", show.legend = FALSE) + 
    geom_node_point(aes(size = strength, fill = region), shape = 21, color = "black", show.legend = FALSE) + 
    geom_node_text(aes(label = name), repel = TRUE) +
    scale_edge_width_continuous(range = c(0.5, 6), name = "Edge Weight") +
    scale_size_continuous(range = c(2, 8), name = "Node Strength") +
    scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
    scale_color_identity(guide = "none") +
    theme_graph() 

ggsave(filename = "network_export1.png", plot = net_ex,
       width = 8,        # Width in inches
       height = 6,       # Height in inches
       dpi = 300)         # High resolution for printing)  

# --- 1. Clean Names on BOTH graphs FIRST ---
names_im <- V(g_im)$name
# Match the g_im name order to the g_ex layout coordinates
# Use the SAME layout_ex_named object from step 2
saved_layout_ordered <- layout_ex_named[names_im, ]

# --- 5. Diagnostic Check (CRITICAL) ---
# Run this line and see what it prints.
# If it prints TRUE, you still have a name mismatch.
print(paste("Any NAs in layout:", any(is.na(saved_layout_ordered))))


net_im <- ggraph(g_im, layout = saved_layout_ordered) +
  geom_edge_link(aes(width = weight), alpha = 0.05, color = "grey", 
                 show.legend = FALSE) + 
  geom_node_point(aes(size = strength, fill = region), shape = 21, 
                  color = "black", show.legend = FALSE) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(0.5, 6), name = "Edge Weight") +
  scale_size_continuous(range = c(2, 8), name = "Node Strength") +
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  scale_color_identity(guide = "none") +
  theme_graph() 
ggsave(filename = "network_import1.png", plot = net_im,
       width = 8,        # Width in inches
       height = 6,       # Height in inches
       dpi = 300)         # High resolution for printing)  


#----Additional Plots----#####
# --- Option 1: "Top 5" Bar Charts "who is most important"

top_strength_ex <- nodes_list_ex %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = strength, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = strength, y = reorder(name, strength))) +
  # Use 'geom_col' for bar charts
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title for clarity on the slide
  labs(
    title = "Export Strength",  #Top 5: Export Strength
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# Display the first plot
# print(top_strength_ex)

# --- Plot 2: Top 5 by Import Strength (New) ---
# We assume 'nodes_list_im' is your data frame for import

top_strength_im <- nodes_list_im %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = strength, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = strength, y = reorder(name, strength))) +
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title
  labs(
    title = "Import Strength", #Top 5: Import Strength
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )
# --- Plot 3: Top 5 by Export Betweenness (New) ---

top_between_ex <- nodes_list_ex %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = between, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = between, y = reorder(name, between))) +
  # Use 'geom_col' for bar charts
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title for clarity on the slide
  labs(
    title = "Export Betweenness",  #Top 5: Export Betweeneess
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# Display the first plot
# print(top_between_ex)

# --- Plot 4: Top 5 by Import Betweenness (New) ---
# We assume 'nodes_list_im' is your data frame for import
top_between_im <- nodes_list_im %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = between, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = between, y = reorder(name, between))) +
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title
  labs(
    title = "Import Betweenness", #Top 5: Import Strength
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# --- Plot 5: Top 5 by Export Closeness  (New) ---

top_closen_ex <- nodes_list_ex %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = closen, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = closen, y = reorder(name, closen))) +
  # Use 'geom_col' for bar charts
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title for clarity on the slide
  labs(
    title = "Export Closeness",  #Top 5: Export Betweeneess
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# Display the first plot
# print(top_between_ex)

# --- Plot 6: Top 5 by Import Closeness (New) ---
# We assume 'nodes_list_im' is your data frame for import
top_closen_im <- nodes_list_im %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = closen, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = closen, y = reorder(name, closen))) +
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title
  labs(
    title = "Import Closenness", #Top 5: Import Strength
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# --- Plot 7: Top 5 by Export Eigenv  (New) ---

top_eigen_ex <- nodes_list_ex %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = eigenv, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = eigenv, y = reorder(name, eigenv))) +
  # Use 'geom_col' for bar charts
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title for clarity on the slide
  labs(
    title = "Export Eigenvalue",  #Top 5: Export Betweeneess
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# Display the first plot
# print(top_between_ex)

# --- Plot 8: Top 5 by Import Eigen (New) ---
# We assume 'nodes_list_im' is your data frame for import
top_eigen_im <- nodes_list_im %>%
  # Select the 5 rows with the highest 'strength'
  slice_max(order_by = eigenv, n = 5) %>%
  
  # Create the plot
  ggplot(aes(x = eigenv, y = reorder(name, eigenv))) +
  geom_col(aes(fill = region), color = "black") +
  
  # Set colors to match your network plots
  scale_fill_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  
  # Add a simple title
  labs(
    title = "Import Eigenvalue", #Top 5: Import Strength
    x = "", 
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Hides x-axis numbers (labels)
    axis.ticks.x = element_blank(), # Hides x-axis tick marks
    axis.title.x = element_blank()  # Formally hides the x-axis title
  )

# --- Combine and Save ---

# Combine the two plots side-by-side
# 'patchwork' makes this easy with '+'
combined_plot <- top_strength_ex + top_between_ex + top_eigen_ex + 
              top_strength_im + top_between_im + top_eigen_im
                  #top_closen_ex + top_closen_im 

# Display the combined plot
print(combined_plot)

# Save the combined plot to a high-resolution PNG file
ggsave(filename = "top5_strength_between_combined.png", 
       plot = combined_plot,
       width = 15,  # Width in inches (adjust as needed for your slide)
       height = 5,  # Height in inches
       dpi = 300)


# --- Option 2: "Role Map" Scatter Plot ---
# This plot identifies the "role" of each country in the network.

# --- Export Role Map (Strength vs. Betweenness) ---

# Calculate medians to draw quadrant lines
median_strength <- median(nodes_list_ex$strength)
median_between <- median(nodes_list_ex$between)

role_map_ex <- ggplot(nodes_list_ex, aes(x = strength, y = between)) +
  
  # Add median lines to create four quadrants
  geom_hline(yintercept = median_between, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = median_strength, linetype = "dashed", color = "grey50") +
  
  # Add points for each country
  geom_point(aes(color = region, size = strength), alpha = 0.7) +
  
  # Add labels that do not overlap (this is the key!)
  geom_text_repel(aes(label = name), size = 3.5, max.overlaps = 15) +
  
  # Set colors
  scale_color_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  # Hide the 'size' legend as it's redundant with the x-axis
  scale_size(guide = "none") +
  
  # Add titles
  labs(
    title = "Export Role Map",
    x = "Strength (Trade Volume / Activity)",
    y = "Betweenness (Brokerage / Control)"
  ) +
  
  # Add text annotations for the 4 quadrants
  annotate("text", x = Inf, y = Inf, label = "Hubs", 
           vjust = 1.5, hjust = 1.1, size = 5, color = "grey30") +
  annotate("text", x = -Inf, y = Inf, label = "Brokers", 
           vjust = 1.5, hjust = -0.1, size = 5, color = "grey30") +
  annotate("text", x = -Inf, y = -Inf, label = "Periphery", 
           vjust = -0.5, hjust = -0.1, size = 5, color = "grey30") +
  annotate("text", x = Inf, y = -Inf, label = "Local Players", 
           vjust = -0.5, hjust = 1.1, size = 5, color = "grey30") +
  
  theme_minimal(base_size = 14)

# Display the plot
print(role_map_ex)

median_strength <- meann(nodes_list_ex$strength)
median_between <- mean(nodes_list_ex$between)
role_map_ex <- ggplot(nodes_list_ex, aes(x = strength, y = between)) +
  
  # Add median lines (ggplot automatically maps these to the log scale)
  geom_hline(yintercept = median_between, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = median_strength, linetype = "dashed", color = "grey50") +
  
  # Add points
  geom_point(aes(color = region, size = strength), alpha = 0.7) +
  
  # Add labels
  geom_text_repel(aes(label = name), size = 3.5, max.overlaps = 15) +
  
  # Set colors
  scale_color_manual(values = c("E" = "skyblue", "A" = "gold"), name = "Region") +
  scale_size(guide = "none") +
  
  # --- NEW: Logarithmize the axes ---
  scale_x_log10() + 
  scale_y_log10() +
  # ----------------------------------

# Add titles
labs(
  title = "Export Role Map",
  x = "Strength (Log Scale)",
  y = "Betweenness (Log Scale)"
) +
  
  # Add text annotations
  # Note: Inf and -Inf still work correctly to target the plot edges on log scales
  annotate("text", x = Inf, y = Inf, label = "Hubs", 
           vjust = 1.5, hjust = 1.1, size = 5, color = "grey30") +
  annotate("text", x = -Inf, y = Inf, label = "Brokers", 
           vjust = 1.5, hjust = -0.1, size = 5, color = "grey30") +
  annotate("text", x = -Inf, y = -Inf, label = "Periphery", 
           vjust = -0.5, hjust = -0.1, size = 5, color = "grey30") +
  annotate("text", x = Inf, y = -Inf, label = "Local Players", 
           vjust = -0.5, hjust = 1.1, size = 5, color = "grey30") +
  
  theme_minimal(base_size = 14)

##############################################################################
#---- Edge list SKIP----  
is_weighted(g_ex)
E(g_ex)
hist(E(g_ex)$weight)
# Get all edges with their source and target node attributes
edge_list <- as_data_frame(g_ex, what = "edges")
edge_list$source_region <- V(g_ex)$region[match(edge_list$from, V(g_ex)$name)]
edge_list$target_region <- V(g_ex)$region[match(edge_list$to, V(g_ex)$name)]

# Count the types of ties
intra_europe_ties <- sum(edge_list$source_region == "E" & edge_list$target_region == "E")
intra_asia_ties <- sum(edge_list$source_region == "A" & edge_list$target_region == "A")
inter_regional_ties <- sum(edge_list$source_region != edge_list$target_region)

print(paste("Intra-Europe Connections:", intra_europe_ties))
print(paste("Intra-Asia Connections:", intra_asia_ties))
print(paste("Inter-Regional Connections:", inter_regional_ties))


#---- Bipartite network example FOR LATER----
  
# Sample data
  df_trade <- data.frame(
    Exporter = c("Germany", "China", "Poland", "China"),
    Importer = c("Poland", "Germany", "USA", "USA"),
    Value = c(100, 250, 50, 300)
  )
  
  # Create the graph directly from the edge list
  # igraph is smart enough to infer the nodes
  g_bipartite <- graph_from_data_frame(df_trade, directed = TRUE)
  
  # The key step: Define the 'type' attribute for the bipartite mapping
  # Get all node names
  node_names <- V(g_bipartite)$name
  
  # A node's 'type' is TRUE if its name appears in the Importer column, FALSE otherwise
  V(g_bipartite)$type <- node_names %in% df_trade$Importer
  
  # Check the types (FALSE = Exporter set, TRUE = Importer set)
  print(V(g_bipartite)$type)
  #> [1] FALSE FALSE FALSE  TRUE  TRUE
  