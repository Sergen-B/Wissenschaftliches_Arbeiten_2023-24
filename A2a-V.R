# Aufruf des modifizierten Datensatzes:
source("titanicData.R")

# Unnoetigen Zusatz loeschen:
rm(titanic1)
rm(age_medians)
rm(determine_side)
rm(extract_deck)

# Datensatz in neue Variable zum verkuerzen des Namen:
titanic = titanic_data_modifiziert
rm(titanic_data_modifiziert)

###############################################################################

# Funktion zum Visualisieren:

visualize_data = function(){
  par(mfrow = c(2, 2)) # Einstellung des Fensters
  
  # Survived - Ueberleben der Passagiere:
  tab = as.numeric(table(titanic$Survived))
  
  barplot(tab, names.arg = c("Tote", "Ueberlebende"),
          ylim = c(0, max(tab) + 100), main = "Ueberleben der Passagiere",
          col = rainbow(length(tab)))
  
  # Absolute Zahl je Auspraegung:
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
  
  # Sex - Geschlecht der Passagiere:
  tab = as.numeric(table(titanic$Sex))
  
  barplot(tab, names.arg = c("weiblich", "maennlich"),
          ylim = c(0, max(tab) + 100), main = "Geschlecht der Passagiere",
          col = rainbow(length(tab)))
  
  # Absolute Zahl je Auspraegung:
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
  
  # Embarked - Zustiegshafen:
  tab = as.numeric(table(titanic$Embarked)[-1])
  tab = c(tab, nrow(titanic) - sum(tab))
  
  barplot(tab, ylim = c(0, max(tab) + 100), main = "Zustiegshafen",
          names = c("Cherbourg", "Queenstown", "Southampton", "Unbekannt"),
          col = rainbow(length(tab)))
  
  # Absolute Zahl je Auspraegung:
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
  
  # Deck der Passagiere:
  tab = as.numeric(table(titanic$Deck)[-8])
  tab = c(tab, nrow(titanic) - sum(tab))
  
  barplot(tab, ylim = c(0, max(tab) + 100), main = "Deck der Passagiere",
          names.arg = c("A", "B", "C", "D", "E", "F", "G", "T", "Unbekannt"),
          col = rainbow(length(tab)))
  
  # Absolute Zahl je Auspraegung:
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
  
  par(mfrow = c(1, 1)) # Zuruecksetzen der Einstellung
}

visualize_data()

