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
library(countrycode)

#----Data download ----
#getwd()
#setwd("C:/Users/user/OneDrive/Dokumenty/Publikacje/AniaLozowicka")
#list.files()
#data_ex <- read.csv("MerchandiseTrade_Export.csv", sep = ";", dec = ".") 
#data_im <- read.csv("MerchandiseTrade_Import.csv", sep = ";", dec = ".")
data_ex <- read.csv("eksport_agregowany.csv", sep = ",", dec = ".") 
data_im <- read.csv("import_agregowany.csv", sep = ",", dec = ".")

head(data_ex)
names(data_ex) <- c("Source", "Target", "Value")
head(data_im)
names(data_im) <- c("Source", "Target", "Value")

is.numeric(data_ex$Value)
is.numeric(data_im$Value)

# removing potential loops
data_ex_no_loops <- data_ex[data_ex$Source != data_ex$Target, ]
data_im_no_loops <- data_im[data_im$Source != data_im$Target, ]
data_ex <- data_ex_no_loops
data_im <- data_im_no_loops

#----Network for export ----
g_ex <- graph_from_data_frame(data_ex, directed = TRUE) 

E(g_ex)$weight <- E(g_ex)$Value / 1000000  # scaling


#----Plot simple -----
png("network_ex_new.png", width = 600, height = 600)

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

##---- Node attributes----
V(g_ex)
country_names <- V(g_ex)$name
sorted_names <- sort(country_names)
print(sorted_names)
dput(sorted_names)

country_nodes <- V(g_ex)$name

# Automate mapping with specific overrides for broken encodings and weird abbreviations
V(g_ex)$continent <- countrycode(
  sourcevar = country_nodes,
  origin = "country.name",
  destination = "continent",
  custom_match = c(
    # Fixing encoding errors
    "CA´te d'Ivoire" = "Africa",
    "CuraA§ao" = "Americas",
    "Saint BarthA©lemy" = "Americas",
    "TA1rkiye" = "Asia",
    # Fixing specific abbreviations and aggregates
    "Br. Indian Ocean Terr." = "Asia",
    "Br. Virgin Isds" = "Americas",
    "FS Micronesia" = "Oceania",
    "Other Asia, nes" = "Asia",
    "N. Mariana Isds" = "Oceania",
    "Wallis and Futuna Isds" = "Oceania"
  )
)

# Identify any nodes that failed to map (returns NA)
# Why? This is a crucial quality assurance step to ensure our network has no missing metadata.
unmatched_countries <- country_nodes[is.na(V(g_ex)$continent)]
print(unmatched_countries)

# Load necessary libraries
library(igraph)
library(countrycode)

continent_colors <- c(
  "Europe"     = "skyblue",
  "Asia"       = "gold",
  "Americas"   = "palegreen3",
  "Africa"     = "tomato",
  "Oceania"    = "orchid",
  "Antarctica" = "lightgray"
)

# Extract nodes from the graph
country_nodes <- V(g_ex)$name

# Map countries to continents with an updated custom_match dictionary
# handling all known edge cases, encoding errors, and missing territories.
V(g_ex)$continent <- countrycode(
  sourcevar = country_nodes,
  origin = "country.name",
  destination = "continent",
  custom_match = c(
    # Fixing encoding errors
    "CA´te d'Ivoire" = "Africa",
    "CuraA§ao" = "Americas",
    "Saint BarthA©lemy" = "Americas",
    "TA1rkiye" = "Asia",
    # Fixing specific abbreviations and aggregates
    "Br. Indian Ocean Terr." = "Asia",
    "Br. Virgin Isds" = "Americas",
    "FS Micronesia" = "Oceania",
    "Other Asia, nes" = "Asia",
    "N. Mariana Isds" = "Oceania",
    "Wallis and Futuna Isds" = "Oceania",
    # Fixing newly identified unmatched territories
    "Cocos Isds" = "Oceania",                  # Cocos (Keeling) Islands
    "Fr. South Antarctic Terr." = "Antarctica",  # French Southern and Antarctic Lands
    "Bonaire" = "Americas"                     # Caribbean island (Netherlands)
  )
)

epsilon <- 1e-6

V(g_ex)$color <- unname(continent_colors[V(g_ex)$continent])

V(g_ex)$strength <- strength(g_ex, mode = "out", weights = E(g_ex)$Value)

E(g_ex)$distance <- 1 / (E(g_ex)$weight + epsilon)
hist(E(g_ex)$distance)
export_threshold <- quantile(V(g_ex)$strength, probs = 0.1, na.rm = TRUE)
#export_threshold <- 1000

weak_nodes <- V(g_ex)[V(g_ex)$strength < export_threshold]
g_ex_filtered <- delete_vertices(g_ex, weak_nodes)
print(vcount(g_ex_filtered))

nodes_list_ex <- igraph::as_data_frame(g_ex_filtered, what = "vertices")
head(nodes_list_ex) 
nodes_list_ex <- nodes_list_ex %>%
  mutate(
  eigenv = eigen_centrality(g_ex_filtered, weights = E(g_ex_filtered)$weight)$vector,
  between = betweenness(g_ex_filtered, weights = E(g_ex_filtered)$distance),
  #mutate(closen = closeness(g_ex, weights = E(g_ex)$weight, mode = "out")) %>%
  closen = closeness(g_ex_filtered, weights = E(g_ex_filtered)$distance, mode = "out")
  )
head(nodes_list_ex)

#cor(nodes_list$closen, nodes_list$closen_inv)

write.csv2(nodes_list_ex, "nodes_ex1.csv")

#----Network for import ----

g_im <- graph_from_data_frame(data_im, directed = TRUE) 
E(g_im)$weight <- E(g_im)$Value / 1000000  # scaling
country_nodes <- V(g_im)$name

# Map countries to continents with an updated custom_match dictionary
# handling all known edge cases, encoding errors, and missing territories.
V(g_im)$continent <- countrycode(
  sourcevar = country_nodes,
  origin = "country.name",
  destination = "continent",
  custom_match = c(
    # Fixing encoding errors
    "CA´te d'Ivoire" = "Africa",
    "CuraA§ao" = "Americas",
    "Saint BarthA©lemy" = "Americas",
    "TA1rkiye" = "Asia",
    # Fixing specific abbreviations and aggregates
    "Br. Indian Ocean Terr." = "Asia",
    "Br. Virgin Isds" = "Americas",
    "FS Micronesia" = "Oceania",
    "Other Asia, nes" = "Asia",
    "N. Mariana Isds" = "Oceania",
    "Wallis and Futuna Isds" = "Oceania",
    # Fixing newly identified unmatched territories
    "Cocos Isds" = "Oceania",                  # Cocos (Keeling) Islands
    "Fr. South Antarctic Terr." = "Antarctica",  # French Southern and Antarctic Lands
    "Bonaire" = "Americas"                     # Caribbean island (Netherlands)
  )
)

V(g_im)$color <- unname(continent_colors[V(g_im)$continent])

V(g_im)$strength <- strength(g_im, mode = "out", weights = E(g_im)$Value)

E(g_im)$distance <- 1 / (E(g_im)$weight + epsilon)
hist(E(g_im)$distance)
import_threshold <- quantile(V(g_im)$strength, probs = 0.1, na.rm = TRUE)
#import_threshold <- 1000

weak_nodes <- V(g_im)[V(g_im)$strength < import_threshold]
g_im_filtered <- delete_vertices(g_im, weak_nodes)
print(vcount(g_im_filtered))

nodes_list_im <- igraph::as_data_frame(g_im_filtered, what = "vertices")
head(nodes_list_im) 
nodes_list_im <- nodes_list_im %>%
  mutate(
    eigenv = eigen_centrality(g_im_filtered, weights = E(g_im_filtered)$weight)$vector,
    between = betweenness(g_im_filtered, weights = E(g_im_filtered)$distance),
    #mutate(closen = closeness(g_im, weights = E(g_im)$weight, mode = "out")) %>%
    closen = closeness(g_im_filtered, weights = E(g_im_filtered)$distance, mode = "out")
  )
head(nodes_list_im)

write.csv2(nodes_list_im, "nodes_im1.csv")

# Check for exact equality (content and order - should be OK)
identical(nodes_list_ex$name, nodes_list_im$name)
setdiff(nodes_list_ex$name, nodes_list_im$name)
##! problem

#---- Plots advanced ----

# exact same layout every time you run this script.
set.seed(123) 

iso_codes <- countrycode(
  sourcevar = V(g_ex_filtered)$name,
  origin = "country.name",
  destination = "iso3c",
  custom_match = c(
    "CA´te d'Ivoire" = "CIV",
    "CuraA§ao" = "CUW",
    "Saint BarthA©lemy" = "BLM",
    "TA1rkiye" = "TUR",
    "Br. Indian Ocean Terr." = "IOT",
    "Br. Virgin Isds" = "VGB",
    "FS Micronesia" = "FSM",
    "Other Asia, nes" = NA,       
    "N. Mariana Isds" = "MNP",
    "Wallis and Futuna Isds" = "WLF",
    "Cocos Isds" = "CCK",
    "Fr. South Antarctic Terr." = "ATF",
    "Bonaire" = "BES"
  )
)
g_ex_filtered <- set_vertex_attr(g_ex_filtered, name = "iso3", value = iso_codes)
g_ex_filtered <- set_vertex_attr(g_ex_filtered, name = "label", value = iso_codes)

iso_codes_im <- countrycode(
  sourcevar = V(g_im_filtered)$name,
  origin = "country.name",
  destination = "iso3c",
  custom_match = c(
    "CA´te d'Ivoire" = "CIV",
    "CuraA§ao" = "CUW",
    "Saint BarthA©lemy" = "BLM",
    "TA1rkiye" = "TUR",
    "Br. Indian Ocean Terr." = "IOT",
    "Br. Virgin Isds" = "VGB",
    "FS Micronesia" = "FSM",
    "Other Asia, nes" = NA,       
    "N. Mariana Isds" = "MNP",
    "Wallis and Futuna Isds" = "WLF",
    "Cocos Isds" = "CCK",
    "Fr. South Antarctic Terr." = "ATF",
    "Bonaire" = "BES"
  )
)
g_im_filtered <- set_vertex_attr(g_im_filtered, name = "iso3", value = iso_codes)
g_im_filtered <- set_vertex_attr(g_im_filtered, name = "label", value = iso_codes)


# --- 2. Create ONE Authoritative Layout ---
# Calculate the layout ONCE and save it
layout_ex_named <- layout_with_fr(g_ex_filtered)
# Assign the now-CLEAN names to the layout
rownames(layout_ex_named) <- V(g_ex_filtered)$name

net_ex <- ggraph(g_ex_filtered, layout = "kk") +
  # Edges: Kept the same, light grey with variable width based on export weight
  geom_edge_link(aes(width = weight, alpha = weight), color = "grey", show.legend = FALSE) + 
  
  # Nodes: Map fill to 'continent'. Removed show.legend = FALSE so the color key appears
  geom_node_point(aes(size = strength, fill = continent), shape = 21, color = "black") + 
  
  # Labels: Map to 'label' (ISO-3 codes) instead of 'name' for a cleaner look
  geom_node_text(aes(label = label), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  
  # Scales: Update the fill scale to use the new global palette
  scale_edge_width_continuous(range = c(0.2, 4), name = "Edge Weight") +
  scale_size_continuous(range = c(1.5, 6), name = "Node Strength") +
  scale_fill_manual(values = continent_colors, name = "Continent", na.value = "white") +
  
  # Clean background theme
  theme_graph() 


ggsave(filename = "network_export2.png", plot = net_ex,
       width = 14,        # Width in inches 8
       height = 10,       # Height in inches 6
       dpi = 300)         # High resolution for printing)  
# Print the plot
print(net_ex)

##---- export plot second approach ----
edge_threshold <- quantile(E(g_ex_filtered)$weight, probs = 0.80, na.rm = TRUE)
g_plot <- delete_edges(g_ex_filtered, E(g_ex_filtered)[weight < edge_threshold])
g_plot <- delete_vertices(g_plot, degree(g_plot) == 0)
net_ex <- ggraph(g_plot, layout = "fr") +
  geom_edge_link(aes(width = log(weight), alpha = log(weight)), color = "grey", show.legend = FALSE) + 
  
  geom_node_point(aes(size = strength, fill = continent), shape = 21, color = "black") + 
  
  geom_node_text(aes(label = label), repel = TRUE, size = 3, max.overlaps = Inf) +
  
  scale_edge_width_continuous(range = c(0.1, 1.5)) +
  scale_edge_alpha_continuous(range = c(0.05, 0.3)) + 
  
  scale_size_continuous(range = c(2, 8), name = "Node Strength") +
  scale_fill_manual(values = continent_colors, name = "Continent", na.value = "white") +
  
  theme_graph() 

# save
ggsave(
  filename = "network_pruned.png", 
  plot = net_ex,           
  width = 14,              
  height = 10,             
  dpi = 300,               
  bg = "white"             
)
#keep the layout from export
saved_layout_ordered <- layout_ex_named[V(g_im_filtered)$iso3, ]

has_coordinate <- !is.na(saved_layout_ordered[, 1])

if (any(!has_coordinate)) {
  # Print which countries are causing the mismatch
  missing_countries <- V(g_im)$name[!has_coordinate]
  print(paste("Removing countries missing from export layout:", paste(missing_countries, collapse=", ")))
  g_im_filtered <- delete_vertices(g_im, V(g_im)[!has_coordinate])
  saved_layout_ordered <- saved_layout_ordered[has_coordinate, ]
} else {
  g_im_filtered <- g_im
  print("Perfect match! No missing coordinates.")
}
#!! do dokoñczenia

net_im <- ggraph(g_im_filtered, layout = "circle") +
  geom_edge_link(aes(width = weight), alpha = 0.05, color = "grey", show.legend = FALSE) + 
  geom_node_point(aes(size = strength, fill = continent), shape = 21, 
                  color = "black", show.legend = FALSE) + 
  geom_node_text(aes(label = label), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  scale_edge_width_continuous(range = c(0.5, 6), name = "Edge Weight") +
  scale_size_continuous(range = c(2, 8), name = "Node Strength") +
  scale_fill_manual(values = continent_colors, name = "Continent", na.value = "white") +
  scale_color_identity(guide = "none") +
  theme_graph() 
ggsave(filename = "network_import2.png", plot = net_im,
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
  