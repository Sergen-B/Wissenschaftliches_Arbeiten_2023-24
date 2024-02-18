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

###############################
# Noch unfertig:

# Kategoriale Variablen:
# Survived - Sex - Embarked (zwei fehlende Elemente) -
# Anrede (ungeeignet, zu viele Merkmale) - Side - Deck (viele NAs)

# Visualisieren der kategorialen Variablen:
visualize_data = function(){
  par(mfrow = c(2, 2)) # Einstellung des Fensters
  
  # Saeulendiagramme:
  # Survived:
  tab1 = as.numeric(table(titanic$Survived))
  
  barplot(tab1, names.arg = c("Tote", "Überlebende"),
          ylim = c(0, max(tab1) + 100), main = "Überleben der Passagiere")
  text(0.7, y = tab1[1] + 50, labels = as.character(tab1[1]), cex = 1.5)
  text(1.9, y = tab1[2] + 40, labels = as.character(tab1[2]), cex = 1.5)
  
  # ...
  
  
  # Kreisdiagramme:
  ## Eines muss entfernt werden.
  
  pie(table(titanic$Survived), radius = 1) # Survived
  pie(table(titanic$Sex), radius = 1) # Sex
  pie(table(titanic$Embarked), radius = 1) # Embarked
  pie(table(titanic$Side)) # Side
  # pie(table(titanic$Deck)[-8]) # Deck
  
  par(mfrow = c(1, 1)) # Zuruecksetzen der Einstellung
}

visualize_data()


###################################
table(titanic$Survived)
table(titanic$Sex)
table(titanic$Embarked)
table(titanic$Anrede)
table(titanic$Side)
table(titanic$Deck)[-8]

hist(as.numeric(titanic$Survived))
barplot(as.numeric(table(titanic$Survived)), names.arg = c(0, 1))