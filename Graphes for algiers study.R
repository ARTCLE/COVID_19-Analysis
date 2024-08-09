library(ggplot2)
library(lubridate)
library(dplyr)

# Assurez-vous que votre data frame contient les colonnes suivantes: Date, AT, MXT, MNT, HM, RF, WS, AQI_predict, PM2.5_predict
#######1 Clean the data from NA's 
#Calculer le nombre de NA dans les deux variables de pollution de l'air
na_AQI_predict <- sum(is.na(ALGER_data$AQI_predict))
na_PM25_predict <- sum(is.na(ALGER_data$PM2.5_predict))

# Afficher les résultats
cat("Nombre de NA dans AQI_predict:", na_AQI_predict, "\n")
cat("Nombre de NA dans PM2.5_predict:", na_PM25_predict, "\n")

# Supprimer les lignes avec des NA dans les deux variables
ALGER_data <- ALGER_data %>%
  filter(!is.na(AQI_predict) & !is.na(PM2.5_predict))




# Ajouter des colonnes pour l'année
ALGER_data <- ALGER_data %>%
  mutate(Year = year(Date))

# AT, MXT, MNT
ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = MXT, color = "MXT")) +
  geom_line(aes(y = MNT, color = "MNT")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Températures (AT, MXT, MNT) par Année",
       x = "Date", y = "Température (°C)",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = HM, color = "HM")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Humidité (HM) par Année",
       x = "Date", y = "Humidité (%)",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = RF, color = "RF")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Précipitations (RF) par Année",
       x = "Date", y = "Précipitations (mm)",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = WS, color = "WS")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Vitesse du Vent (WS) par Année",
       x = "Date", y = "Vitesse du Vent (m/s)",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = AQI_predict, color = "AQI_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "AQI_predict par Année",
       x = "Date", y = "AQI_predict",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = PM2.5_predict, color = "PM2.5_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "PM2.5_predict par Année",
       x = "Date", y = "PM2.5_predict",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




