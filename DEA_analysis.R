# =====================================
# Author: Anna £ozowicka
# Purpose: DEA analysis
# =====================================

# =====================================
# Packages
# =====================================

library(writexl)


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