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
# auch andersherum. Dies ist mit der Beruecksichtigung des Kontextes auch
# logisch, da einige Familien an Bord waren und damit auch einige Kinder zu den
# Passagieren zaehlten, denen es eher nicht moeglich ist, ein Ticket zu erwerben.
# Zudem ist die sehr geringe Korrelation insofern auch interessant, da dies
# impliziert, dass zur damaligen Zeit nicht großartig zwischen dem maennlichen
# und weiblichen Geschlecht unterschieden wurde, wenn es um die finanziellen
# Preisaspekt der Tickets ging, was einen kleinen Einblick in die damaligen
# sozialen bzw. gesellschaftlichen Sichtweisen ermoeglicht.


# Analyse des Verhaeltnisses Alter - Ueberlebende:
result <- punkt_biseriale_korrelation_und_stats(titanic, "Age", "Survived")
print(result)

# Die Analyse der Daten legt nahe, dass es eine gewisse korrelation zwischen 
# dem Alter der passagiere und ihrem ueberlebensstatus gibt. Obwohl aeltere 
# Passagiere tendenziell niedrigere Ueberlebenschancen hatten als juengere, ist 
# die Korrelation relativ schwach. Dies deutet darauf hin, dass das Alter allein
# moeglicherweise nicht ausreicht, um die ueberlebenschancen vollstaendig zu 
# erklaeren. Andere Faktoren wie Geschlecht, soziooekonomischer Status oder die 
# positionierung auf dem Schiff koennten ebenfalls eine Rolle gespielt haben.
# Daher ist es möglich, dass die Daten keine eindeutige Unterstützung dafür
# bieten, dass das Alter einen signifikanten Einfluss auf die 
# Ueberlebenschancen hatte.


# Analyse der ueberlebenden Passagiere:
deskriptive_Statistiken_kateg(data.frame(titanic$Survived))

# Deskriptive Statistiken fuer kategorielle Variablen wie Geschlecht,
# Passagierklasse und Einsteigehafen zeigen die Haeufigkeit jeder Kategorie
# sowie deren relative Verteilung im Datensatz Informationen ermoeglichen es
# Muster und Trends im Bezug auf die Variablen zu indentifizieren, die
# vielleicht mit dem Ueberlebensstatus der Passagiere in Verbindung stehen.
# Hier wird ersichtlich, dass der Großteil der Passagiere nicht ueberlebt.
# Zusaetzlich gibt es scheinbar nur zwei verschiedene Auspraegungen von der
# Variable Ueberlebensstatus, naemlich "ueberlebt" und "tot".


# Analyse vom Ueberleben, Geschlecht und Zustiegshafen:
visualize_data(list("Ueberleben" = titanic$Survived, "Geschlecht" = titanic$Sex,
                    "Zustiegshafen" = titanic$Embarked))

# Liefert einen ersten Blick in die Vertder kategoriellen Variablen im Datensatz
# Die Balkendiagramme zeigen, dass die Anzahl der Toten úeberwiegt, genauso wie
# die Anzahl der maennlichen Passagiere. Zudem wird klar deutlich, dass die
# meisten Passagiere in Southampton eingestiegen sind.
# Es können weiter Analysen und statistische Tests durchgeführt werden, um
# Zusammenhänge zwischen den Variablen und dem Ueberleben der Passagiere zu
# untersuchen.


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

###################################################################################
# Aufgabe 5)

# Analyse vom Verhaeltnis Ueberleben - Side :
calculate_bivariate_stats(titanic$Side, titanic$Survived)

# In der Kontingenztafel sehen wir, dass 38,59% (44 von 114) der Passagiere auf der Backbordseite nicht überlebt haben,
# während es auf der Steuerbordseite 26% (24 von 90)sind. Das scheint auf den ersten Blick ein Unterschied 
# zu sein. Allerdings zeigen der Phi-Koeffizient und Cramér's V, beide bei 0.115, dass dieser Zusammenhang eher
# schwach ist. Diese Werte sind ziemlich niedrig und deuten darauf hin, dass die Seite des Schiffes (Backbord vs.
# Steuerbord) möglicherweise keinen starken Einfluss auf die Überlebenschancen hatte.

# Ein wichtiger Punkt, den wir hier beachten sollten, ist die große Anzahl an fehlenden Werten (NA) in unseren 
# Daten – insgesamt 687 von 891 Beobachtungen. Dies könnte die Ergebnisse beeinflussen und die Beziehung zwischen
# der Schiffsseite und den Überlebenschancen verzerren. Es ist möglich, dass diese fehlenden Daten wichtige
# Informationen enthalten, die uns helfen könnten, ein klareres Bild zu erhalten. Daher sollten wir bei der
# Interpretation unserer Ergebnisse vorsichtig sein und diese Limitation im Hinterkopf behalten.


