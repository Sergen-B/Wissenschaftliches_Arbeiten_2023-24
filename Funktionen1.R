# Aufruf des modifizierten Datensatzes:
source("titanic-WA.R")

# Unnoetigen Zusatz loeschen:
rm(titanic1)
rm(age_medians)
rm(determine_side)
rm(extract_deck)

# Datensatz in neue Variable zum verkuerzen des Namen:
titanic = titanic_data_modifiziert
rm(titanic_data_modifiziert)

# Aufruf von "Funktionen2.R" fuer innere Funktionen:
source("Funktionen2.R")

###############################################################################
# 2a) (i) - Deskriptive Stat. (metrisch):
# deskriptive_Statistiken Funktion
# 
# Diese Funktion berechnet und gibt deskriptive Statistiken für metrische
# Variablen aus.
#
# Argumente:
#   data: Ein Vektor von metrischen Daten, für die deskriptive Statistiken 
#         berechnet werden sollen.
#
# Rueckgabewerte:
#   Die Funktion gibt die folgenden deskriptiven Statistiken aus:
#     - Mittelwert (Mean)
#     - Median
#     - Minimum
#     - Maximum
#     - Standardabweichung
#
# Hinweise:
#   - Die Funktion ueberprueft, ob die Daten metrisch sind, indem sie 
#     sicherstelltl, dass sie numerisch sind. Andernfalls wird ein Fehler 
#     ausgegeben.
#
# Beispiel:
#   # Beispielaufruf der Funktion mit einem Vektor von metrischen Daten
#   data <- c(10, 20, 30, 40, 50)
#   deskriptive_Statistiken(data).
#

deskriptive_Statistiken <- function(data) {
  # Ueberpruefen, ob die Daten metrisch sind
  if (!is.numeric(data)) {
    stop("Die Daten müssen metrisch sein.")
  }
  
  # Berechnung der deskriptiven Statistiken
  mean_value <- mean(data)
  median_value <- median(data)
  min_value <- min(data)
  max_value <- max(data)
  sd_value <- sd(data)
  
  # Ausgabe der berechneten Statistiken
  cat("Mean (Mittelwert): ", mean_value, "\n")
  cat("Median: ", median_value, "\n")
  cat("Minimum: ", min_value, "\n")
  cat("Maximum: ", max_value, "\n")
  cat("Standardabweichung: ", sd_value, "\n")
}

# Beispielaufruf der Funktion mit einem Vektor von metrischen Daten
data <- c(10, 20, 30, 40, 50)
deskriptive_Statistiken(data)

###############################################################################
# 2a) (ii) - Deskriptive Stat. (kategorial):
deskriptive_Statistiken_kateg = function (daten)
{
  # Überprüfen, ob daten ein DataFrame ist 
  if (!is.data.frame(daten))
  {
    stop(  "Die übergebenen Daten sind kein DataFrame.")
  }
  # Überprüfen, ob der DataFrame mindestens eine kategoriale Variable (Faktor) enthält
  if  (!any(sapply(daten, is.factor)))
  {
    stop("Der DataFrame enthält keine kategoriale Variable.")
  }
  
  # Selektieren aller kategorialen Variablen
  kategoriale_variablen <- daten[, sapply(daten, is.factor)]
  
  # Zusammenführen aller kategorialen Variablen in eine einzige Vektor
  gesamte_kategorien <- unlist(kategoriale_variablen, use.names = FALSE)
  
  # Berechnen der Gesamthäufigkeiten und relativen Häufigkeiten
  abs_ha_gesamt <- table(gesamte_kategorien, useNA = "always")
  re_ha_gesamt <- prop.table(abs_ha_gesamt)
  
  # Erstellen eines DataFrames für die gesamten Statistiken
  gesamte_statistiken <- data.frame(
    Kategorie = names(abs_ha_gesamt),
    Abs_ha = as.integer(abs_ha_gesamt),
    Re_ha = re_ha_gesamt
  )
  
  return(gesamte_statistiken)
}
# als beispiel können wir untere df betrachen 
beispiel_daten <- factor(c("Apfel","Apfel","Apfel","Apfel","Apfel", "Banane", "Banane","Banane","Apfel", NA, "Banane","Orange","Orange", NA, "Orange"))
data <- as.data.frame(beispiel_daten)
deskriptive_Statistiken_kateg(data)

###############################################################################
# 2a) (iii) - Deskrpitve bivariate Stat. (kategorial):
calculate_bivariate_stats <- function(var1, var2) {
  contingency_table <- table(var1, var2)
  
  # Chi-square test
  chi_sq_test <- chisq.test(contingency_table)
  
  # Phi coefficient (for 2x2 tables)
  if(nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
    phi_coefficient <- sqrt(chi_sq_test$statistic / sum(contingency_table))
  } else {
    phi_coefficient <- NA
  }
  
  # Cramér's V
  n <- sum(contingency_table)
  min_dim <- min(nrow(contingency_table), ncol(contingency_table))
  cramers_v <- sqrt(chi_sq_test$statistic / (n * (min_dim - 1)))
  
  # Output
  cat("Contingency Table:\n")
  print(contingency_table)
  cat("\n")
  cat("Chi-square test:\n")
  print(chi_sq_test)
  cat("\n")
  cat("Phi Coefficient (for 2x2 tables):\n")
  print(phi_coefficient)
  cat("\n")
  cat("Cramér's V:\n")
  print(cramers_v)
}

# Beispielaufruf
# var1 <- c("A", "A", "B", "B", "B")
# var2 <- c("X", "X", "Y", "Y", "Z")
# calculate_bivariate_stats(var1, var2)

###############################################################################
# 2a) (iv) - Deskriptive bivariate Stat. (metrisch - dichotom):
ist_binär <- function(variable) {
  length(unique(na.omit(variable))) == 2
}

# Funktion, um zu prüfen, ob zwei Datensätze die gleiche Anzahl an Beobachtungen haben
gleiche_länge <- function(datensatz1, datensatz2) {
  length(datensatz1) == length(datensatz2)
}

# Funktion zur Berechnung der Punkt-biserialen Korrelation
punkt_biseriale_korrelation <- function(kontinuierlich, binär, korrektur = TRUE) {
  # Prüfungen der Eingabedaten
  if (!is.numeric(kontinuierlich)) {
    stop("Die kontinuierliche Variable sollte numerisch sein.")
  }
  if (!ist_binär(binär)) {
    stop("Die binäre Variable sollte genau zwei eindeutige Werte haben.")
  }
  if (!gleiche_länge(kontinuierlich, binär)) {
    stop("Die Variablen sollten die gleiche Anzahl an Beobachtungen haben.")
  }
  
  # Fehlende Werte ausschließen
  vollständig <- complete.cases(kontinuierlich, binär)
  kontinuierlich <- kontinuierlich[vollständig]
  binär <- binär[vollständig]
  
  # Umwandlung der binären Variable in numerisch für die Berechnung
  binär_numerisch <- as.numeric(as.factor(binär)) - 1
  
  # verdecken der binären Variablen
  verst0 <- binär_numerisch == 0
  verst1 <- binär_numerisch == 1
  
  # Anzahl der Beobachtungen für jede Gruppe berechnen
  anzahl0 <- sum(verst0)
  anzahl1 <- sum(verst1)
  gesamtanzahl <- anzahl0 + anzahl1
  
  # Mittelwerte und Standardabweichungen berechnen
  mittelwert1 <- mean(kontinuierlich[verst0], na.rm = TRUE)
  mittelwert2 <- mean(kontinuierlich[verst1], na.rm = TRUE)
  gesamt_mittelwert <- mean(kontinuierlich)
  gesamt_sd <- sd(kontinuierlich)
  
  # Punkt-biseriale Korrelation berechnen
  punkt_korr <- (mittelwert2 - mittelwert1) / gesamt_sd * sqrt((anzahl0 * anzahl1) / gesamtanzahl^2)
  
  return(punkt_korr)
}

# Beispielaufruf der Funktion
beispiel_daten <- data.frame(KontinuierlicheVariable = rnorm(100), 
                             BinäreVariable = factor(sample(c("Kategorie1", "Kategorie2"), 100, replace = TRUE)))
korrelation <- punkt_biseriale_korrelation(beispiel_daten$KontinuierlicheVariable, beispiel_daten$BinäreVariable)
print(korrelation)

###############################################################################
# 2a) (v) - Visualisierung (kategorial):

# visualize_data  - implementiert eine Funktion, die die uebergebenen
#                   kategoriellen Variablen als Saeulendiagramm visualisiert.
#
# Eingabe:
#   - dataList: eine benannte Liste mit drei oder vier Elementen. Alle
#               Listenelemente muessen dabei nicht-numerische Vektoren sein,
#               welche die Auspraegungen der entsprechenden kategoriellen
#               Variable enthalten. 
#
# Ausgabe:
# Eine Grafik, die drei oder vier Plots bzw. Saeulendiagramme enthaelt, je nach
# Laenge des "dataList"-Parameters. Jeder Plot visualisiert dabei jeweils die
# Auspraegungen, bzw. die dazugehoerigen absoluten Haeufigkeiten, einer
# kategoriellen Variable, die als Listenelement im "dataList"-Parameter
# enthalten ist, mit einem passenden Saeulendiagramm. Der Titel eines
# Saeulendiagramms entspricht dabei dem Namen des Listenelement, der den
# dazugehoerigen Parametervektor der kategoriellen Variable enthaelt.

visualize_data = function(dataList){
  stopifnot(length(dataList) %in% 3:4, !all(sapply(dataList, is.numeric)))
  
  par(mfrow = c(2, 2)) # Einstellung des Fensters
  
  for(i in seq(1, length(dataList))){
    tab = correctTab(dataList[[i]])
    plotMain = names(dataList)[[i]]
    
    barplot(as.numeric(tab), names.arg = names(tab), main = plotMain,
            ylim = c(0, max(tab) + 100), col = rainbow(length(tab)),
            cex.names = 0.7)
    
    customBars(as.numeric(tab))
  }
  
  par(mfrow = c(1, 1)) # Zuruecksetzen der Einstellung
}

# Beispielaufruf der Funktion mit vier ausgewaehlten kategoriellen Variabelen
# des Datensatzes aus "titantic-WA.R":
argList = list("Ueberleben der Passagiere" = titanic$Survived,
               "Geschlecht der Passagiere" = titanic$Sex,
               "Zustiegshafen" = titanic$Embarked,
               "Deck der Passagiere" = titanic$Deck)

visualize_data(argList)
