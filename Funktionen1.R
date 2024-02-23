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
# Funktion zur Berechnung und Ausgabe von deskriptiven Statistiken für
# metrische Variablen

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

###############################################################################
# 2a) (v) - Visualisierung (kategorial):
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

# Die Bezeichung bzw. der Name eines Listenelements entspricht dem Titel des
# zugehoerigen Plots, der durch visualize_data() erzeugt wird:
argList = list("Ueberleben der Passagiere" = titanic$Survived,
               "Geschlecht der Passagiere" = titanic$Sex,
               "Zustiegshafen" = titanic$Embarked,
               "Deck der Passagiere" = titanic$Deck)

visualize_data(argList)
