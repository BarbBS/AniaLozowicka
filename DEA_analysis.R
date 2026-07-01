# =====================================
# Author: Anna £ozowicka
# Purpose: DEA analysis - exports (part 1) & imports (part 2) - centrality measures based on full network from SNA
# =====================================

# =====================================
# Packages
# =====================================

library(writexl)
library(readxl)
library(deaR)
library(DJL)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(giscoR)


# =====================================
# SNA exports centrality measures -> DEA outputs (ex)
# =====================================

# Wczytanie pliku
centralities_ex <- read.csv2("output/csv_results/centralities/centralities_ex.csv")

# Sprawdzenie czy dane s¹ wczytane jako liczbowe
head(centralities_ex)
names(centralities_ex)
str(centralities_ex)

# Wydzielenie z pliku podzbioru krajów z centrality measures tylko dla full network
centralities_ex_full <- subset(
  centralities_ex,
  variant == "1. Full network"
)

dim(centralities_ex_full)
head(centralities_ex_full)

#sprawdzenie czy kolumna iso3=label
table(centralities_ex_full$iso3 == centralities_ex_full$label)

#wartoœci siê zgadzaj¹ dla 225 z 226 oberwacji dla iso3. Szukam tej jednej obserwacji
sum(is.na(centralities_ex_full$iso3))
centralities_ex_full[is.na(centralities_ex_full$iso3), ]
sum(is.na(centralities_ex_full$label))
#ta jedna obserwacja to Other Asia (przyp.Taiwan- w DEA brak danych dla Taiwan, wiêc póŸniej ta pozycja wypadnie)

#sprawdzenie czy dany kraj wystêpuje tylko raz
sum(duplicated(centralities_ex_full$iso3))

#sprawdzenie kompletnoœci danych dla centrality measures (czy s¹ braki danych)
colSums(is.na(centralities_ex_full))

#Wydzielenie tylko outputs do DEA (iso3 i label na razie zostaj¹)
dea_outputs_ex <- centralities_ex_full[
  , c("name", "continent", "iso3", "label", "strength", "eigenv", "closen")
]
head(dea_outputs_ex)

#Eksport DEA outputs do pliku
write_xlsx(
  dea_outputs_ex,
  "DEA_results/DEA_outputs_ex.xlsx"
)

# =====================================
# SNA imports centrality measures -> DEA outputs (im)
# =====================================

# Wczytanie pliku
centralities_im <- read.csv2("output/csv_results/centralities/centralities_im.csv")

# Sprawdzenie czy dane s¹ wczytane jako liczbowe
head(centralities_im)
names(centralities_im)
str(centralities_im)

# Wydzielenie z pliku podzbioru krajów z centrality measures tylko dla full network
centralities_im_full <- subset(
  centralities_im,
  variant == "1. Full network"
)

dim(centralities_im_full)
head(centralities_im_full)

#sprawdzenie czy kolumna iso3=label
table(centralities_im_full$iso3 == centralities_im_full$label)

#wartoœci siê zgadzaj¹ dla 225 z 226 obserwacji dla iso3. Szukam tej jednej obserwacji
sum(is.na(centralities_im_full$iso3))
centralities_im_full[is.na(centralities_im_full$iso3), ]
sum(is.na(centralities_im_full$label))
#ta jedna obserwacja to Other Asia (przyp.Taiwan- w DEA brak danych dla Taiwan, wiêc póŸniej ta pozycja wypadnie)

#sprawdzenie czy dany kraj wystêpuje tylko raz
sum(duplicated(centralities_im_full$iso3))

#sprawdzenie kompletnoœci danych dla centrality measures (czy s¹ braki danych)
colSums(is.na(centralities_im_full))

#Wydzielenie tylko outputs do DEA (iso3 i label na razie zostaj¹)
dea_outputs_im <- centralities_im_full[
  , c("name", "continent", "iso3", "label", "strength", "eigenv", "closen")
]
head(dea_outputs_im)

#Eksport DEA outputs (im) do pliku
write_xlsx(
  dea_outputs_im,
  "DEA_results/DEA_outputs_im.xlsx"
)

# =====================================
# DEA dataset for exports efficiency analysis
# =====================================

#wczytanie pliku z inputs oraz pliku z outputs
inputs_complete <- read_excel("DEA_results/inputs_complete.xlsx")
names(inputs_complete)
dea_outputs_ex <- read_excel("DEA_results/DEA_outputs_ex.xlsx")
names(dea_outputs_ex)

# sprawdzenie ró¿nic w nazwach krajow
setdiff(inputs_complete$Country, dea_outputs_ex$name)

# spr które nazwy krajów z outpus s¹ podobne do fragmentów nazw z pliku inputs 
dea_outputs_ex$name[
  grepl(
    "Bosnia|Central African|Ivoire|Dominican|Iran|Netherlands|Korea|Dem. People|Moldova|Solomon|Syria|TA1rkiye|Tanzania|USA|Venezuela",
    dea_outputs_ex$name
  )
]

# tworzenie s³ownika, gdzie odmienne nazwy krajow z inputs dostaja nazwy z pliku outputs (to plik z SNA)
name_map <- c(
  "Bosnia and Herzegovina" = "Bosnia Herzegovina",
  "Central African Republic" = "Central African Rep.",
  "Cote d'Ivoire" = "CA´te d'Ivoire",
  "Dominican Republic" = "Dominican Rep.",
  "Iran (Islamic Republic of)" = "Iran",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "Republic of Korea" = "Rep. of Korea",
  "Republic of Moldova" = "Rep. of Moldova",
  "Solomon Islands" = "Solomon Isds",
  "Syrian Arab Republic" = "Syria",
  "Turkiye" = "TA1rkiye",
  "United Republic of Tanzania" = "United Rep. of Tanzania",
  "United States" = "USA",
  "Venezuela (Bolivarian Rep. of)" = "Venezuela"
)
name_map

# zmiana odmiennych nazw krajów z pliku inputs na nazwy z pliku outputs (pozostale nazwy bez zmian)
inputs_complete$Country <- ifelse(
  inputs_complete$Country %in% names(name_map),
  name_map[inputs_complete$Country],
  inputs_complete$Country
)
setdiff(inputs_complete$Country, dea_outputs_ex$name)

#sprawdzenie czy liczba krajow w inputs sie nie zmienila (172) i czy nie ma zdublowanych nazw
nrow(inputs_complete)
sum(duplicated(inputs_complete$Country))

#Dolaczenie do pliku outputs kolumn z nak³adami z pliku inputs (po nazwach krajow). Zostana tylko 172 kraje
DEA_dataset_ex <- merge(
  dea_outputs_ex,
  inputs_complete,
  by.x = "name",
  by.y = "Country",
  all = FALSE
)
dim(DEA_dataset_ex)
names(DEA_dataset_ex)

# sprawdzenie czy nie ma brakow danych
colSums(is.na(DEA_dataset_ex))

# Zapisanie dataset do pliku excela
write_xlsx(
  DEA_dataset_ex,
  "DEA_results/DEA_dataset_ex.xlsx"
)


# =====================================
# DEA dataset for imports efficiency analysis
# =====================================

#wczytanie pliku z inputs oraz pliku z outputs
inputs_complete <- read_excel("DEA_results/inputs_complete.xlsx")
names(inputs_complete)
dea_outputs_ex <- read_excel("DEA_results/DEA_outputs_im.xlsx")
names(dea_outputs_ex)

# sprawdzenie ró¿nic w nazwach krajow
setdiff(inputs_complete$Country, dea_outputs_im$name)

# spr które nazwy krajów z outpus s¹ podobne do fragmentów nazw z pliku inputs 
dea_outputs_im$name[
  grepl(
    "Bosnia|Central African|Ivoire|Dominican|Iran|Netherlands|Korea|Dem. People|Moldova|Solomon|Syria|TA1rkiye|Tanzania|USA|Venezuela",
    dea_outputs_im$name
  )
]

# tworzenie s³ownika, gdzie odmienne nazwy krajow z inputs dostaja nazwy z pliku outputs (to plik z SNA)
name_map <- c(
  "Bosnia and Herzegovina" = "Bosnia Herzegovina",
  "Central African Republic" = "Central African Rep.",
  "Cote d'Ivoire" = "CA´te d'Ivoire",
  "Dominican Republic" = "Dominican Rep.",
  "Iran (Islamic Republic of)" = "Iran",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "Republic of Korea" = "Rep. of Korea",
  "Republic of Moldova" = "Rep. of Moldova",
  "Solomon Islands" = "Solomon Isds",
  "Syrian Arab Republic" = "Syria",
  "Turkiye" = "TA1rkiye",
  "United Republic of Tanzania" = "United Rep. of Tanzania",
  "United States" = "USA",
  "Venezuela (Bolivarian Rep. of)" = "Venezuela"
)
name_map

# zmiana odmiennych nazw krajów z pliku inputs na nazwy z pliku outputs (pozostale nazwy bez zmian)
inputs_complete$Country <- ifelse(
  inputs_complete$Country %in% names(name_map),
  name_map[inputs_complete$Country],
  inputs_complete$Country
)
setdiff(inputs_complete$Country, dea_outputs_ex$name)

#sprawdzenie czy liczba krajow w inputs sie nie zmienila (172) i czy nie ma zdublowanych nazw
nrow(inputs_complete)
sum(duplicated(inputs_complete$Country))

#Dolaczenie do pliku outputs kolumn z nak³adami z pliku inputs (po nazwach krajow). Zostana tylko 172 kraje
DEA_dataset_im <- merge(
  dea_outputs_im,
  inputs_complete,
  by.x = "name",
  by.y = "Country",
  all = FALSE
)
dim(DEA_dataset_im)
names(DEA_dataset_im)

# sprawdzenie czy nie ma brakow danych
colSums(is.na(DEA_dataset_im))

# Zapisanie dataset do pliku excela
write_xlsx(
  DEA_dataset_im,
  "DEA_results/DEA_dataset_im.xlsx"
)


# =====================================
# Diagnostyka danych (w DEA_dataset_ex oraz DEA_dataset_im) pod DEA
# =====================================

summary(DEA_dataset_ex[, c("GDP", "FDI_stock", "Labour", "GHG", "Energy",
                           "strength", "eigenv", "closen")])

summary(DEA_dataset_im[, c("GDP", "FDI_stock", "Labour", "GHG", "Energy",
                           "strength", "eigenv", "closen")])

cor(
  DEA_dataset_ex[, c("strength", "eigenv", "closen")]
)

cor(
  DEA_dataset_im[, c("strength", "eigenv", "closen")]
)

cor(
  DEA_dataset_ex[, c("GDP","FDI_stock","Labour","GHG","Energy")]
)

sd(DEA_dataset_im$closen)
sd(DEA_dataset_ex$closen)

hist(DEA_dataset_im$closen)

quantile(
  DEA_dataset_im$closen,
  probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
)

# =====================================
# Analiza efektywnosci cz.1 - model SBM dla exports
# =====================================

packageVersion("deaR")
packageVersion("DJL")
apropos("sbm")
?model_sbmeff

# wczytanie dataset dla exports
DEA_dataset_ex <- read_excel(
  "DEA_results/DEA_dataset_ex.xlsx"
)

# utworzenie obiektu do analizy
args(read_data)

data_dea_ex <- make_deadata(
  datadea = DEA_dataset_ex,
  ni = 5,
  no = 3,
  dmus = 1,
  inputs = c(8, 9, 10, 11, 12),
  outputs = c(5, 6, 7)
)

sbm_ex <- model_sbmeff(
  data_dea_ex,
  orientation = "io",
  rts = "vrs"
)

eff_ex <- efficiencies(sbm_ex)
head(eff_ex)

results_ex <- data.frame(
  Country = DEA_dataset_ex$name,
  iso3 = DEA_dataset_ex$iso3,
  Efficiency = as.numeric(efficiencies(sbm_ex)),
  row.names = NULL
)

head(results_ex)

#zapisanie rankingu z analizy efektywnoœci dla exports
write_xlsx(
  results_ex,
  "DEA_results/SBM_exports_results.xlsx"
)

#zapisanie wszystkich informacji do pliku
names(sbm_ex)
apropos("slack")
apropos("target")
apropos("lambda")
apropos("peer")

slacks_ex <- slacks(sbm_ex)
str(slacks_ex)

targets_ex <- targets(sbm_ex)
str(targets_ex)

lambdas_ex <- lambdas(sbm_ex)
str(lambdas_ex)


# =========================
# Efficiency scores

Efficiency_ex <- data.frame(
  Country = DEA_dataset_ex$name,
  iso3 = DEA_dataset_ex$iso3,
  Efficiency = as.numeric(efficiencies(sbm_ex))
)

# =========================
# Input slacks

Input_slacks_ex <- data.frame(
  Country = rownames(slacks_ex$slack_input),
  iso3 = DEA_dataset_ex$iso3,
  slacks_ex$slack_input,
  row.names = NULL
)

# =========================
# Output slacks

Output_slacks_ex <- data.frame(
  Country = rownames(slacks_ex$slack_output),
  iso3 = DEA_dataset_ex$iso3,
  slacks_ex$slack_output,
  row.names = NULL
)

# =========================
# Target inputs

Target_inputs_ex <- data.frame(
  Country = rownames(targets_ex$target_input),
  iso3 = DEA_dataset_ex$iso3,
  targets_ex$target_input,
  row.names = NULL
)

# =========================
# Target outputs

Target_outputs_ex <- data.frame(
  Country = rownames(targets_ex$target_output),
  iso3 = DEA_dataset_ex$iso3,
  targets_ex$target_output,
  row.names = NULL
)

# =========================
# Lambdas

Lambdas_ex <- data.frame(
  Country = rownames(lambdas_ex),
  lambdas_ex,
  row.names = NULL
)

# =========================
# Export do Excel

write_xlsx(
  list(
    Efficiency_ex     = Efficiency_ex,
    Input_slacks_ex   = Input_slacks_ex,
    Output_slacks_ex  = Output_slacks_ex,
    Target_inputs_ex  = Target_inputs_ex,
    Target_outputs_ex = Target_outputs_ex,
    Lambdas_ex        = Lambdas_ex
  ),
  "DEA_results/SBM_exports_full_results.xlsx"
)

#zapisanie modelu w formacie RDS
saveRDS(
  sbm_ex,
  "DEA_results/SBM_exports_model.rds"
)


# =====================================
# Analiza efektywnosci cz.2 - model SBM dla imports
# =====================================

# wczytanie dataset dla exports
DEA_dataset_im <- read_excel(
  "DEA_results/DEA_dataset_im.xlsx"
)

# utworzenie obiektu do analizy
#args(read_data)

data_dea_im <- make_deadata(
  datadea = DEA_dataset_im,
  ni = 5,
  no = 3,
  dmus = 1,
  inputs = c(8, 9, 10, 11, 12),
  outputs = c(5, 6, 7)
)

sbm_im <- model_sbmeff(
  data_dea_im,
  orientation = "io",
  rts = "vrs"
)

eff_im <- efficiencies(sbm_im)
head(eff_im)

results_im <- data.frame(
  Country = DEA_dataset_im$name,
  iso3 = DEA_dataset_im$iso3,
  Efficiency = as.numeric(efficiencies(sbm_im)),
  row.names = NULL
)

head(results_im)

#zapisanie rankingu z analizy efektywnoœci dla exports
write_xlsx(
  results_im,
  "DEA_results/SBM_imports_results.xlsx"
)

#zapisanie wszystkich informacji do pliku
names(sbm_im)
apropos("slack")
apropos("target")
apropos("lambda")
apropos("peer")

slacks_im <- slacks(sbm_im)
str(slacks_im)

targets_im <- targets(sbm_im)
str(targets_im)

lambdas_im <- lambdas(sbm_im)
str(lambdas_im)


# =========================
# Efficiency scores

Efficiency_im <- data.frame(
  Country = DEA_dataset_im$name,
  iso3 = DEA_dataset_im$iso3,
  Efficiency = as.numeric(efficiencies(sbm_im))
)

# =========================
# Input slacks

Input_slacks_im <- data.frame(
  Country = rownames(slacks_im$slack_input),
  iso3 = DEA_dataset_im$iso3,
  slacks_im$slack_input,
  row.names = NULL
)

# =========================
# Output slacks

Output_slacks_im <- data.frame(
  Country = rownames(slacks_im$slack_output),
  iso3 = DEA_dataset_im$iso3,
  slacks_im$slack_output,
  row.names = NULL
)

# =========================
# Target inputs

Target_inputs_im <- data.frame(
  Country = rownames(targets_im$target_input),
  iso3 = DEA_dataset_im$iso3,
  targets_im$target_input,
  row.names = NULL
)

# =========================
# Target outputs

Target_outputs_im <- data.frame(
  Country = rownames(targets_im$target_output),
  iso3 = DEA_dataset_im$iso3,
  targets_im$target_output,
  row.names = NULL
)

# =========================
# Lambdas

Lambdas_im <- data.frame(
  Country = rownames(lambdas_im),
  lambdas_im,
  row.names = NULL
)

# =========================
# Export do Excel

write_xlsx(
  list(
    Efficiency_im     = Efficiency_im,
    Input_slacks_im   = Input_slacks_im,
    Output_slacks_im  = Output_slacks_im,
    Target_inputs_im  = Target_inputs_im,
    Target_outputs_im = Target_outputs_im,
    Lambdas_im        = Lambdas_im
  ),
  "DEA_results/SBM_imports_full_results.xlsx"
)

#zapisanie modelu w formacie RDS
saveRDS(
  sbm_im,
  "DEA_results/SBM_imports_model.rds"
)


# =====================================
# Prezentacja wyników - wykresy, mapy
# =====================================

#kolory kontynentów takiej, jak z SNA
continent_colors <- c(
  "Europe"   = "skyblue",
  "Asia"     = "gold",
  "Africa"   = "tomato",
  "Americas" = "palegreen3",
  "Oceania"  = "orchid"
)

# =========================
# Dane do map, histogramow i scatterplot
# =========================

plot_exports <- data.frame(
  Country = Efficiency_ex$Country,
  iso3 = Efficiency_ex$iso3,
  continent = DEA_dataset_ex$continent,
  color = continent_colors[DEA_dataset_ex$continent],
  Efficiency = Efficiency_ex$Efficiency
)

plot_imports <- data.frame(
  Country = Efficiency_im$Country,
  iso3 = Efficiency_im$iso3,
  continent = DEA_dataset_im$continent,
  color = continent_colors[DEA_dataset_im$continent],
  Efficiency = Efficiency_im$Efficiency
)

write_xlsx(
  list(
    Exports = plot_exports,
    Imports = plot_imports
  ),
  "DEA_results/DEA_plot_data_maps_histograms.xlsx"
)

# =========================
# Dane do scatterplot i histogramow
# =========================

plot_scatter <- merge(
  plot_exports[, c("Country", "iso3", "continent", "color", "Efficiency")],
  plot_imports[, c("iso3", "Efficiency")],
  by = "iso3",
  suffixes = c("_exports", "_imports")
)

write_xlsx(
  plot_scatter,
  "DEA_results/DEA_plot_data_scatter.xlsx"
)

head(plot_exports)
head(plot_imports)
head(plot_scatter)

dim(plot_exports)
dim(plot_imports)
dim(plot_scatter)

#Exports efficiency statistics
summary(plot_exports$Efficiency)
mean(plot_exports$Efficiency)
median(plot_exports$Efficiency)
#sum(plot_exports$Efficiency == 1) - jeden kraj ma efekt.b.bliska 1 (w xsl tego nie widac)
sum(round(plot_exports$Efficiency, 6) == 1)
100 * sum(round(plot_exports$Efficiency, 6) == 1) / nrow(plot_exports)

#Imports efficiency statistics
summary(plot_imports$Efficiency)
mean(plot_imports$Efficiency)
median(plot_imports$Efficiency)
sum(round(plot_imports$Efficiency, 6) == 1)
100 * sum(round(plot_imports$Efficiency, 6) == 1) / nrow(plot_imports)

# =========================
# Histogram - exports efficiency

hist_exports_plot <-
  ggplot(plot_exports, aes(x = Efficiency)) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "steelblue",
    linewidth = 0.3
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    #title = "SBM Efficiency Scores - Exports",
    x = "Efficiency score",
    y = "Number of countries"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    
    plot.margin = margin(5, 5, 5, 5)
  )

# zapis do PDF
ggsave(
  filename = "DEA_results/SBM_exports_histogram.pdf",
  plot = hist_exports_plot,
  width = 10,
  height = 4.5,
  units = "in"
)


# =========================
# Histogram - imports efficiency

hist_imports_plot <-
  ggplot(plot_imports, aes(x = Efficiency)) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "steelblue",
    linewidth = 0.3
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    #title = "SBM Efficiency Scores - Imports",
    x = "Efficiency score",
    y = "Number of countries"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    
    plot.margin = margin(5, 5, 5, 5)
  )

# zapis do PDF
ggsave(
  filename = "DEA_results/SBM_imports_histogram.pdf",
  plot = hist_imports_plot,
  width = 10,
  height = 4.5,
  units = "in"
)


# =========================
# Maps - 1.Exports efficiency and 2. Imports efficiency

# sprawdzenie kodowania nazw krajow (nasze vs. wbudowane w mapy swiata w pakiecie)
world_gisco <- gisco_get_countries(
  resolution = "20",
  epsg = "4326"
)

names(world_gisco)

setdiff(plot_exports$iso3, world_gisco$ISO3_CODE)
setdiff(plot_imports$iso3, world_gisco$ISO3_CODE)

# =========================
# Map 1 - exports efficiency 

map_exports_gisco <- world_gisco %>%
  left_join(
    plot_exports,
    by = c("ISO3_CODE" = "iso3")
  )

sum(!is.na(map_exports_gisco$Efficiency))
table(is.na(map_exports_gisco$Efficiency))

world_gisco[world_gisco$ISO3_CODE == "UKR", c("CNTR_NAME", "ISO3_CODE")]
world_gisco[world_gisco$ISO3_CODE == "RUS", c("CNTR_NAME", "ISO3_CODE")]

map_exports_gisco$Efficiency_round <- round(map_exports_gisco$Efficiency, 6)

map_exports_gisco$eff_class <- cut(
  map_exports_gisco$Efficiency_round,
  breaks = c(-Inf, 0.25, 0.50, 0.75, 1, Inf),
  labels = c("< 0.25", "0.25–0.50", "0.50–0.75", "0.75–<1.00", "1.00"),
  right = FALSE
)

table(map_exports_gisco$eff_class, useNA = "ifany")

map_exports_plot <-
  ggplot(map_exports_gisco) +
  geom_sf(aes(fill = eff_class),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "< 0.25"     = "#d73027",
      "0.25–0.50"  = "#fc8d59",
      "0.50–0.75"  = "#fee08b",
      "0.75–<1.00" = "#91cf60",
      "1.00"       = "#006837"
    ),
    na.value = "grey95",
    name = "Efficiency scores"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-58, 85),
    expand = FALSE
  ) +
  #labs(title = "SBM Efficiency - Exports") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 9.5, face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.key.width = unit(0.35, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, -25),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 2)
  )

ggsave(
  filename = "DEA_results/SBM_exports_map.pdf",
  plot = map_exports_plot,
  width = 12,
  height = 5,
  units = "in"
)

# =========================
# Map 2 - imports efficiency 

map_imports_gisco <- world_gisco %>%
  left_join(
    plot_imports,
    by = c("ISO3_CODE" = "iso3")
  )

sum(!is.na(map_imports_gisco$Efficiency))
table(is.na(map_imports_gisco$Efficiency))

world_gisco[world_gisco$ISO3_CODE == "UKR", c("CNTR_NAME", "ISO3_CODE")]
world_gisco[world_gisco$ISO3_CODE == "RUS", c("CNTR_NAME", "ISO3_CODE")]

map_imports_gisco$Efficiency_round <- round(map_imports_gisco$Efficiency, 6)

map_imports_gisco$eff_class <- cut(
  map_imports_gisco$Efficiency_round,
  breaks = c(-Inf, 0.25, 0.50, 0.75, 1, Inf),
  labels = c("< 0.25", "0.25–0.50", "0.50–0.75", "0.75–<1.00", "1.00"),
  right = FALSE
)

table(map_imports_gisco$eff_class, useNA = "ifany")

map_imports_plot <-
  ggplot(map_imports_gisco) +
  geom_sf(aes(fill = eff_class),
          color = "white",
          linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "< 0.25"     = "#d73027",
      "0.25–0.50"  = "#fc8d59",
      "0.50–0.75"  = "#fee08b",
      "0.75–<1.00" = "#91cf60",
      "1.00"       = "#006837"
    ),
    na.value = "grey95",
    name = "Efficiency scores"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-58, 85),
    expand = FALSE
  ) +
  #(title = "SBM Efficiency - Imports") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 9.5, face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.key.width = unit(0.35, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, -25),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 2)
  )

ggsave(
  filename = "DEA_results/SBM_imports_map.pdf",
  plot = map_imports_plot,
  width = 12,
  height = 5,
  units = "in"
)


