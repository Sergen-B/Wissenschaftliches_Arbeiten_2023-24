# Aufgabe 2b) - Innere Funktionen:

# Funktion fuer Beschriftung von Saeulendiagrammen:
customBars = function(tab){
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
}

# Funktion zur Ausgabe einer angepassten Tabelle des Parametervektors:
correctTab = function(data){
  tab = table(data)
  
  # Filterung der Variablennamen: "NA" oder "" -> "Unbekannt"
  if(any(names(tab) %in% c("NA", ""))){
    pos = which(names(tab) %in% c("NA", ""))
    sub = tab[pos]
    names(sub) = "Unbekannt"
    
    tab = c(tab[-pos], sub)
  }
  
  # Anpassung bestimmter Variablennamen fuer bessere Visualisierung:
  if(any(names(tab) %in% c("0", "1"))){
    names(tab) = c("Tote", "Ueberlebende")
  }
  if(any(names(tab) %in% c("female", "male"))){
    names(tab) = c("weiblich", "maennlich")
  }
  if(all(names(tab) %in% c("C", "Q", "S", "Unbekannt"))){
    names(tab) = c("Cherbourg", "Queenstown", "Southampton", "Unbekannt")
  }
  
  # Ergaenzung potentiell fehlender Werte:
  if(sum(tab) < length(data)){
    sub = length(data) - sum(tab)
    names(sub) = "Unbekannt"
    
    tab = c(tab, sub)
  }
  
  return(tab)
}
