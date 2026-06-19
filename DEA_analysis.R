# =====================================
# Author: Anna £ozowicka
# Purpose: DEA analysis
# =====================================

# =====================================
# Packages
# =====================================

library(writexl)
library(readxl)

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