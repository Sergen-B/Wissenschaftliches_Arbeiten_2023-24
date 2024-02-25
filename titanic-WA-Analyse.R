# Aufgabe 4)

# Aufruf des Skripts "Funktionen1.R" fuer Funktionen:
source("Funktionen1.R")

# Analyse der Ticketpreise:
deskriptive_Statistiken(titanic$Fare)

# Die vorliegende Standardabweichung laesst sich so interpretieren, dass die
# gezahlten Ticketpreise durchschnittlich ca. 50 [Waehrung] vom Mittelwert
# entfernt liegen. Mit Beruecksichtigung des Maximum, Minimums und Mittelwerts
# laesst sich daraus interpretieren, dass die meisten Passagiere eher geringe
# Ticketpreise bezahlt haben und verhaeltnismaeßig wenige Passagiere hohe
# Ticketpreise bezahlt haben. Diese Interpretation stuetzt, dass der vorliegende
# Median ebenfalls sehr nah am Minimum bzw. sehr weit entfernt vom Maximum ist.

# Analyse des Verhaeltnisses Ticketpreise - Geschlechter:
punkt_biseriale_korrelation(titanic$Fare, titanic$Sex)

# Die punktbiseriale Korrelation ergibt einen recht kleinen Wert bzw. einen Wert,
# der verhaeltnismaeßig nahe bei 0 liegt, was bedeutet, dass zwischen den Werten
# nur ein recht kleiner Zusammenhang vorliegt. Im Sachverhalt kann dies so
# interpretiert werden, dass der generelle Ticketpreis nicht eindeutig vom
# Geschlecht eines Passagieres abhaengt, was bedeutet, dass beispielsweise 
# maennliche Passagiere nicht mehr bezahlt haben als weibliche Passagiere oder
# auch andersherum.


# Analyse des Verhaeltnisses Alter - Ueberlebende:
# Deskriptive bivariate Statistiken (metrisch - dichotom):

print("Deskriptive bivariate Statistiken (metrisch - dichotom):")
result <- punkt_biseriale_korrelation_und_stats(titanic, "Age", "Survived")

# Die Analyse zeigt eine Korrelation zwischen dem Alter der Passagiere und 
# ihrem ueberlebensstatus. Aeltere Passagiere hatten tendenziell niedrigere 
# Überlebenschancen als juengere. Dies legt daran, dass das Alter eine wichtige 
# rolle spielte, moeglicherweise wurden juengere Passagiere bevorzugt oder waren
# physisch besser in der Lage, den Herausforderungen der Katastrophe zu 
# begegnen.
print(result)
