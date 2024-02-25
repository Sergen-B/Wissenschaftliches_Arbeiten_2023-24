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
res_fare_sex = punkt_biseriale_korrelation_und_stats(titanic, "Fare", "Sex")
print(res_fare_sex)

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
print(result)

# Die Analyse zeigt eine Korrelation zwischen dem Alter der Passagiere und 
# ihrem Ueberlebensstatus. Aeltere Passagiere hatten tendenziell niedrigere 
# Ueberlebenschancen als juengere. Dies legt daran, dass das Alter eine wichtige 
# Rolle spielte, moeglicherweise wurden juengere Passagiere bevorzugt oder waren
# physisch besser in der Lage, den Herausforderungen der Katastrophe zu 
# begegnen.

# Analyse der überlebenden Passagiere
deskriptive_Statistiken_kateg(data.frame(titanic$Survived))
# Der Großteil der Passagiere hat nicht überlebt 
# Deskriptive Statistiken fuer kategorielle Variablen wie Geschlecht, Passagierklasse und Einsteigehafen zeigen die Haeufigkeit jeder Kategorie sowie deren relative Verteilung im Datensatz
# Informationen ermoeglichen es Muster und Trends im Bezug auf die Variablen zu indentifizieren, die vielleicht mit dem Ueberlebensstatus der Passagiere in Verbindung stehen


# Analyse vom Ueberleben, Geschlecht und Zustiegshafen:
visualize_data(list("Ueberleben" = titanic$Survived, "Geschlecht" = titanic$Sex,
                    "Zustiegshafen" = titanic$Embarked))
# Liefert einen ersten Blick in die Vertder kategoriellen Variablen im Datensatz
# Es können weiter Analysen und statistische Tests durhcgeführt werden, um Zusammenhänge zwischen den Variablen und dem Ueberleben der Passagiere zu untersuchen
# Die Balkendiagramme zeigen, dass die Anzahl der Toten überwiegt, genau so wie die Anzahl der mänlichen Passagiere
# Zudem wird klar deutlich, dass die meisten Passagiere in Southampton einsteigen



# Analyse vom Verhaeltnis Ueberleben - Geschlecht:
calculate_bivariate_stats(titanic$Sex, titanic$Survived)

# An der Kontingenztafel sieht man, dass viel mehr maennliche Passagiere
# gestorben sind als weibliche. Andererseits gibt es aber auch viel mehr
# maennliche Passagiere als weibliche, weshalb man das Verhaeltnis betrachten
# sollte. Aber auch dann bleibt die Beobachtung richtig, da ca. 81 % der
# maennlichen Passagiere gestorben sind, waehrend es nur ca. 26 % tote weibliche
# Passagiere sind. Der p-Wert beider Variablen beim chisq-Test ist sehr klein,
# was hier bedeuten kann, dass es wahrscheinlich ist, dass die Variablen
# voneinander abhaengen, was der vorangegangenen Beobachtung gewissermaßen
# entsprechen wuerde. Da hier eine 2x2-Kontingenztafel vorliegt, entspricht der
# der Phi-Koeffizient Cramers V und beide sagen das gleiche aus. Da beide Werte
# hier verhaeltnismaeßig groß sind, laesst sich auch mithilfe dieser beiden
# Werte aussagen, dass eine gewisse Abhaengigkeit besteht, die auch nicht
# wirklich klein ist. Somit wird hier auf vier verschiedenen Wegen bzw. mit vier
# Statistiken gezeigt, dass das Ueberleben abhaengig vom Geschlecht ist, da
# maennliche Passagiere verhaeltnismaeßig oefter gestorben sind als weibliche
# Passagiere.

