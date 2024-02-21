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
# 2a) (i) - Deskriptive Stat. (metrisch):

###############################################################################
# 2a) (ii) - Deskriptive Stat. (kategorial):

###############################################################################
# 2a) (iii) - Deskrpitve bivariate Stat. (kategorial):

###############################################################################
# 2a) (iv) - Deskriptive bivariate Stat. (metrisch - dichotom):

###############################################################################
# 2a) (v) - Visualisierung (kategorial):
visualize_data = function(vars = c("Survived", "Sex", "Embarked", "Deck")){
  stopifnot(is.character(vars), length(vars) > 2, length(vars) < 5)
  
  par(mfrow = c(2, 2)) # Einstellung des Fensters
  
  # Innere Funktion fuer Beschriftung der Saeulen:
  customBars = function(tab){
    for(i in seq(1, length(tab))){
      text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
    }
  }
  
  if("Survived" %in% vars){
    tab = as.numeric(table(titanic$Survived))
    
    barplot(tab, names.arg = c("Tote", "Ueberlebende"),
            ylim = c(0, max(tab) + 100), main = "Ueberleben der Passagiere",
            col = rainbow(length(tab)))
    
    customBars(tab)
  }
  
  if("Sex" %in% vars){
    tab = as.numeric(table(titanic$Sex))
    
    barplot(tab, names.arg = c("weiblich", "maennlich"),
            ylim = c(0, max(tab) + 100), main = "Geschlecht der Passagiere",
            col = rainbow(length(tab)))
    
    customBars(tab)
  }
  
  if("Embarked" %in% vars){
    tab = as.numeric(table(titanic$Embarked)[-1])
    tab = c(tab, nrow(titanic) - sum(tab))
    
    barplot(tab, ylim = c(0, max(tab) + 100), main = "Zustiegshafen",
            names = c("Cherbourg", "Queenstown", "Southampton", "Unbekannt"),
            col = rainbow(length(tab)))
    
    customBars(tab)
  }
  
  if("Anrede" %in% vars){
    tab = as.numeric(table(titanic$Anrede))
    
    barplot(tab, ylim = c(0, max(tab) + 100), main = "Anrede der Passagiere",
            names = names(table(titanic$Anrede)), col = rainbow(length(tab)),
            las = 2)
    
    customBars(tab)
  }
  
  if("Side" %in% vars){
    tab = as.numeric(table(titanic$Side))
    tab = c(tab, nrow(titanic) - sum(tab))
    
    barplot(tab, ylim = c(0, max(tab) + 100), main = "Seite der Passagiere",
            names.arg = c("Backbord", "Steuerbord", "Unbekannt"),
            col = rainbow(length(tab)))
    
    customBars(tab)
  }
  
  if("Deck" %in% vars){
    tab = as.numeric(table(titanic$Deck)[-8])
    tab = c(tab, nrow(titanic) - sum(tab))
    
    barplot(tab, ylim = c(0, max(tab) + 100), main = "Deck der Passagiere",
            names.arg = c("A", "B", "C", "D", "E", "F", "G", "T", "Unbekannt"),
            col = rainbow(length(tab)))
    
    customBars(tab)
  }
  
  par(mfrow = c(1, 1)) # Zuruecksetzen der Einstellung
}

visualize_data()
