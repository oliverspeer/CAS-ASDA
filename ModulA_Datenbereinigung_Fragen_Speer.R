# aut: Oliver Speer
# speeroli@students.zhaw.ch
# https://github.com/oliverspeer/CAS-ASDA/blob/main/ModulA_Datenbereinigung_Fragen_Speer.R
#
# Septermber 2024, Winterthur
--------------------------------------------------------------------------------

#  tidyverse library aktivieren
library(tidyverse)

# Teil1: Aufbereitung und Bereinigung des Datensatzes---------------------------


ugz <- read_csv("ugz_luftqualitaetsmessung_seit-2012.csv", col_names = FALSE)
problems(ugz)

# nach der Warnmeldung zeigt sich  mit problems(ugz), 
# dass Zeile 2595 des csv 59 Spalten anstelle 30 erwarteter hat. Beim Einlesen
# wurden die Enträge dieser zusätzlichen csv-Spalten in die die letzte Zelle, 
# Zeile 2595 Spalte 30 des tibble "ugz" eingefügt.
# Bei einer Eingabe/Verarbeitung ging vermulitch ein Komma zwischen letztem Wert 
# der Zeile für 2020-01-31 und dem Datum 2020-02-01 der darauf folgenden Zeile verloren.

# csv file einlesen
ugz <- read_csv("ugz_luftqualitaetsmessung_seit-2012.csv", col_names = FALSE) |> 
  
  
  
  # fehlerhafte Zeile herausfiltern
  filter(str_detect(X30, "\\d{2}.\\d{2}\\d{4}-\\d{2}-\\d{2}")) |>
  
  
  # fehlerhafte Zelle zerlegen und in neue Zeile einfügen
  separate(X30, into = paste0("X", 1:30), sep = ",") |>
  
  # (\(csv) { separate(colnames(csv)[ncol(csv)], into = paste0("X", 1:ncol(csv)), sep = ",") })() |>  
  # warum funktioniert das nicht?
  

  # in neuer Zeile fehlerhaften Datumseintrag korrigieren
  mutate(X1 = str_extract(X1, "\\d{4}-\\d{2}-\\d{2}")) |>
  
  # korrigierte Zeile mit dem csv zusammenführen
  bind_rows(read_csv("ugz_luftqualitaetsmessung_seit-2012.csv", 
                     col_names = FALSE)) |> 
  
 # fehlerhafte Zelle des Original-csv korrigieren
  mutate(X30 = ifelse(
    str_detect(X30, "\\d{2}.\\d{2}\\d{4}-\\d{2}-\\d{2}"),
    str_extract(X30, "\\d{2}.\\d{2}"),
    X30)) |> 
  
  # originale Reihenfolge wiederherstellen
  (\(csv) { slice(csv, 2:nrow(csv), 1) })() |> 


  # 1. Spalte umbennen
  rename(Datum = X1) |> 
  
  # entnehmen der Standort-Bezeichnungen aus der zweiten Zeile mit regex, 
  # mit "-" als Trennzeichen mit den 
  # Umweltvariablen verbinden und als Spaltennamen setzen
  (\(csv) {  rename_with(csv, 
                         ~paste(
                           str_match(csv[2, -1], "(?<=Zch_)[A-Z][a-z\\u00fc]+$")[,1], 
                           csv[4,-1], 
                           sep = "-"
                                ), 
                         .cols = -1
                         )
            }
  )() |> 


# dann können die Zeilen 1 bis 6 gelöscht werden
  slice(-c(1:6)) |> 
  
  # tranformieren in long format mit "standort" and "analyt" als Spalten
  pivot_longer(cols = -Datum,
               names_to = c("standort", "analyt"),
               names_sep = "-") |>

  
  # transformieren in wide format mit "Datum", "standort" 
  # und den verschiedenen Umwelt-Variablen als Spalten
  pivot_wider(names_from = analyt,
              values_from = value) |> 
 
  # kleinschreibung über alles
  rename_with(tolower) |>
  mutate(standort = tolower(standort)) |>
  
  # ">" durch "gt" ersetzen 
  rename_with(~str_replace(., ">", "gt")) |> 
  
  # "strasse" in allen Standort-Angabe entfernen
  mutate(standort = str_replace(standort, "strasse", "")) |> 
  
  # Formate wie Datum, Character und Numeric anpassen wo zutreffend, 
  # einheitlich NAs setzen
  mutate(datum = as.Date(datum),
         
         standort = as.factor(standort),
         
         across(-c(datum, standort), ~ifelse(. == "NaN", NA, .)),
         
         across(-c(datum, standort), as.numeric)
         
          ) |>
         

  
  # nach Datum sortieren
  arrange(datum) |> 

# Spalten "jahr", "monat" und  "wochentag" hinzufügen, 
#"monat" und "wochentag" als Faktor definieren
# ugz <- ugz |>
  mutate(jahr = as.integer(year(datum)),
         monat = factor(month(datum, label = TRUE)),
         wochentag = factor(wday(datum, label = TRUE))) 

# das bereinigte Datenset als .rds file im Arbeitsverzeichnis speichern
  saveRDS(ugz,"ugz_luftqualitaetsmessung_speer.rds")
  

# Teil 2: Datenanalyse & Antworten zu Moodle-Fragen

# Frage 1: ---------------------------------------------------------------------
nrow(ugz)
glimpse(ugz)

# Frage 2-----------------------------------------------------------------------
ugz |> 
  filter(datum == "2014-06-22", standort == "stampfenbach") |> 
  select(pm10)

ugz |> 
  filter(datum == "2020-02-01", standort == "stampfenbach") |> 
  select(pm10)

ugz |> 
  filter(datum == "2012-03-23", standort == "stampfenbach") |> 
  select(pm10)

# Frage 6-----------------------------------------------------------------------
ugz |> 
  filter(!is.na(pm10)) |> 
  nrow()


# Frage 7 & Frage 8-------------------------------------------------------------
library(VIM)
a <- aggr(ugz, plot = FALSE)
plot(a, 
     labels = substr(colnames(ugz), 1, 5)
     )
par(mfrow = c(1,2))
matrixplot(ugz, 
           labels = substr(colnames(ugz), 1, 5)
           )
matrixplot(ugz, 
           labels = substr(colnames(ugz), 1, 5),
           cex.axis = 0.5,
           sortby = "standort"
           )

par(mar = c(4,4,1,1), cex.axis = 0.5)
pbox(ugz, pos = 2, las = 2 )

ugz.s <- ugz |> 
  filter(standort == "stampfenbach")
matrixplot(ugz.s, 
           labels = substr(colnames(ugz), 1, 5),
           cex.axis = 0.5,
           sortby = "monat"
)

# Frage 9-----------------------------------------------------------------------

ugz.s_aug <- ugz |> 
  filter(standort == "stampfenbach", monat == "Aug") |> 
  dplyr::select(#datum, 
                so2, no2, co, no, pm10)

# Anzahl fehlender Werte in ugz.s_aug$pm10
sum(is.na(ugz.s_aug$pm10))

# imputiere die 7 fehlenden Werte in pm10 mit kNN (k=5, gower distance)
library(VIM)
ugz.s_aug.imp <- kNN(ugz.s_aug, 
                     k = 5, 
                     metric = "gower"
                     )

mean(ugz.s_aug.imp$pm10)
mean(ugz.s_aug$pm10, na.rm = TRUE)


# Frage 10----------------------------------------------------------------------

set.seed(245)
library(mice)
m.imputed <- mice(ugz.s_aug, m = 5 , method = "rf") # (1)

analyse <- with(m.imputed , lm(pm10 ~ 1)) # (2)
mean_imp <- sapply(analyse$analyses, coef) 
mean(mean_imp)  # (3)


# Frage 11----------------------------------------------------------------------
library(rrcov)
rob.est <- CovRobust(ugz.s_aug.imp[,1:5])
sort(rob.est@mah, decreasing = TRUE)
ugz.s_aug.imp[,1:5][order(rob.est@mah, decreasing = TRUE)[1],]

# Frage 12----------------------------------------------------------------------

library(MASS)
library(rrcov)

ugz.s_aug.m <- as.matrix(sapply(ugz.s_aug.imp[,1:5], as.numeric))
pca.rob <- PcaHubert(ugz.s_aug.m, k = 1, scale = TRUE)

outliers <- pca.rob@flag == FALSE

plot(pca.rob@scores, pca.rob@od)
points(pca.rob@scores[outliers], pca.rob@od[outliers], col = "red", pch = 19)
# Ausreisser aus Zeile 33 aus Aufgabe 11 übernehmen und grafisch darstellen
points(pca.rob@scores[33], pca.rob@od[33], col = "blue", pch = 19)

ugz.s_aug.m[which.max(pca.rob@od),]
ugz.s_aug.m[which.max(abs(pca.rob@scores)),]

# Frage 13----------------------------------------------------------------------

# Boxplot der NO und NO2 Werte pro Wochentag am Standort Rosengarten
p.box <- ugz |> 
  filter(standort == 'rosengarten') |>
  select(wochentag, no2, no) |>
  na.omit() |>
  mutate('nox' = no + no2) |> 
  pivot_longer(cols = -wochentag, names_to = 'Stickoxide', values_to = 'Werte') |>
  ggplot(aes(x = wochentag, y = Werte, fill = Stickoxide)) +
  geom_boxplot() +
  geom_violin(alpha = 0.5) +
  labs(title = 'NO\u2082 & NO am Rosengarten',
       x = 'Wochentag',
       y = 'Stickoxid-Konzentration [\u00B5g/m\u00B3]') +
  theme_minimal()
  
p.box

# Statistische Zahlen um den Boxplot besser zu erklären
stats <- ugz |> 
  filter(standort == 'rosengarten') |>
  select(wochentag, no2, no) |>
  na.omit() |>
  mutate('nox' = no + no2) |>
  pivot_longer(cols = -wochentag, names_to = 'Stickoxide', values_to = 'Werte') |>
  group_by(wochentag, Stickoxide) |>
  summarise(mean = round( mean(Werte, na.rm = TRUE), 1 ),
            median = round( median(Werte, na.rm = TRUE), 1 ),
            sd = sd(Werte, na.rm = TRUE),
            min = min(Werte, na.rm = TRUE),
            max = max(Werte, na.rm = TRUE),
            n = n()) |>
  ungroup() |>
  arrange(Stickoxide, wochentag)
  

stats


# Frage 14----------------------------------------------------------------------

p.box1.1 <- ugz |> 
  filter(month(datum) == 1 & day(datum) == 1) |> 
  filter(!is.na(pm10)) |>
  ggplot(aes(x = standort, y = pm10)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'PM10 am 1. Januar',
       x = 'Standort',
       y = 'PM10-Konzentration [\u00B5g/m\u00B3]')

p.box1.1

p.box8.1 <- ugz |> 
  filter(month(datum) == 8 & day(datum) == 1) |> 
  filter(!is.na(pm10)) |>
  ggplot(aes(x = standort, y = pm10)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'PM10 am 1. August',
       x = 'Standort',
       y = 'PM10-Konzentration [\u00B5g/m\u00B3]')

p.box8.1

p.box.rest <- ugz |>
  filter(!(month(datum) %in% c(1, 8) & day(datum) == 1)) |> 
  filter(!is.na(pm10)) |>
  ggplot(aes(x = standort, y = pm10)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'PM10 ausser am 1. Januar und 1. August',
       x = 'Standort',
       y = 'PM10-Konzentration [\u00B5g/m\u00B3]')

p.box.rest

p.box9.1 <- ugz |> 
  filter(month(datum) == 9 & day(datum) == 1) |> 
  filter(!is.na(pm10)) |>
  ggplot(aes(x = standort, y = pm10)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'PM10 am 1. September',
       x = 'Standort',
       y = 'PM10-Konzentration [\u00B5g/m\u00B3]')

p.box9.1

p.box2.1 <- ugz |> 
  filter(month(datum) == 2 & day(datum) == 1) |> 
  filter(!is.na(pm10)) |>
  ggplot(aes(x = standort, y = pm10)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = 'PM10 am 1. Februar',
       x = 'Standort',
       y = 'PM10-Konzentration [\u00B5g/m\u00B3]')

p.box2.1
