# Aufgabe 2b) - Innere Funktionen:

# Funktion fuer Beschriftung von Saeulendiagrammen:
# customBars  - implementiert eine Hilfsfunktion, die zu jeder Saeulen eines
#               Saeulendiagramms gewuenschte Zahlen hinzufuegt.
#
# Eingabe:
#   - tab:  ein numerischer Vektor.
#           Der Parameter "tab" enthaelt die Zahlen mit denen die einzelnen
#           Saeulen beschriftet werden sollen, weshalb die Laenge des Vektors
#           der Anzahl an Saeulen des Saeulendiagramms entsprechen sollte. Die
#           Zahlen muessen dabei schon in gewuenschter Reiherfolge sortiert
#           sein.
#
# Ausgabe:
# Keine direkte Ausgabe vorhanden. Es wird lediglich der gewuenschte "Text",
# hier in Form der Zahlen des "tab"-Parameters, beim bereits bestehendes
# Saeulendiagramm ergaenzt.

customBars = function(tab){
  stopifnot(is.numeric(tab))
  
  for(i in seq(1, length(tab))){
    text(0.7 + (i - 1) * 1.2, y = tab[i] + 50, labels = tab[i], cex = 1.5)
  }
}

# Funktion zur Ausgabe einer angepassten Tabelle des Parametervektors:
# correctTab  - implementiert eine Funktion, die eine Haeufigkeitstabelle des
#               uebergebenen Parametervektors einer kategoriellen Variable
#               erstellt und ausgibt.
#
# Eingabe:
#   - data: ein nicht-numerischer Vektor.
#           Der "data"-Parameter enthaelt die beobachteten Auspraegungen
#           einer kategoriellen Variable.
#
# Ausgabe:
# Ein benannter Vektor, der auch als Haeufigkeitstabelle der kategoriellen
# Variable mit den absoluten Haeufigkeiten der Auspraegungen gelesen werden
# kann. Der "Name" des jeweiligen Elements entspricht der Auspraegung.
# Fehlenden Werten wird der "Name" "Unbekannt" uebergeben.

correctTab = function(data){
  stopifnot(!is.numeric(data))
  
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
