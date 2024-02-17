titanic1 <- titanic.csv
summary(titanic1$Age)
library(stringr)
# Anrede extrahieren
titanic1$Anrede <- str_extract(titanic1$Name, "Mr\\.|Mrs\\.|Miss\\.|Ms\\.|Mlle\\.|Mme\\.|Master\\.|Dr\\.|Rev\\.|Col\\.|Major\\.|Capt\\.|Countess\\.|Lady\\.|Sir\\.|Don\\.|Dona\\.|Jonkheer\\.")
titanic1$Anrede<- gsub("Mlle|Ms", "Miss", titanic$Anrede)
table(titanic1$Anrede)
###### as.factor########
titanic1$Survived <- factor(titanic1$Survived)
titanic1$Sex <- factor(titanic1$Sex)
titanic1$Embarked <- factor(titanic1$Embarked)
titanic1$Pclass<- factor(titanic1$Pclass)

##################################
library(dplyr)
# Berechnung des Medians für jede Anrede-Kategorie
age_medians <- titanic1 %>% 
  group_by(Anrede) %>%
  summarize(MedianAge = median(Age, na.rm = TRUE))

# Imputation der fehlenden Werte in 'Age'
titanic1 <- titanic1 %>%
  left_join(age_medians, by = "Anrede") %>%
  mutate(Age = ifelse(is.na(Age), MedianAge, Age)) %>%
  select(-MedianAge)
#########################################################################################

# Funktionen zur Bestimmung von Backbord/Steuerbord und Deck
determine_side <- function(cabin) {
  if (is.na(cabin) || cabin == "") {
    return(NA)
  }
  num_part <- as.numeric(gsub("[^0-9]", "", cabin))
  if (!is.na(num_part) && num_part %% 2 != 0) {
    return("Steuerbord")
  } else {
    return("Backbord")
  }
}
extract_deck <- function(cabin) {
  if (is.na(cabin) || cabin == "") {
    return("NA")
  }
  return(substring(cabin, 1, 1))
}
# Transformationen anwenden
titanic_data_modifiziert <- titanic1 %>%
  mutate(Side = sapply(Cabin, determine_side),
         Deck = sapply(Cabin, extract_deck)) %>%
  select(-Cabin)
# Subset des titanic_data_modifiziert Datensatzes ohne bestimmte Spalten
titanic_data_modifiziert <- subset(titanic_data_modifiziert, select = -c(Pclass, PassengerId, Name, Ticket))

                                   
