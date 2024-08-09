library(ggplot2)
library(lubridate)
library(dplyr)

# Filtrer les données pour inclure uniquement la période de mars 2020 à février 2021
ALGER_data_filtered <- ALGER_data %>%
  filter(Date >= ymd("2020-03-01") & Date <= ymd("2021-02-28"))

# Créer la séquence de dates pour la période filtrée
dates_filtered <- ALGER_data_filtered$Date

# Initialiser les variables
lockdown <- rep(0, length(dates_filtered))

# Définir les périodes de confinement
lockdown_periods <- list(
  c(ymd("2020-03-23"), ymd("2020-06-14")),
  c(ymd("2020-11-01"), ymd("2021-01-31"))
)

# Mettre à jour les valeurs de confinement
for (period in lockdown_periods) {
  start_date <- period[1]
  end_date <- period[2]
  lockdown[dates_filtered >= start_date & dates_filtered <= end_date] <- 1
}

# PM2.5 and lockdowns in Algiers 

# Normaliser les valeurs de PM2.5
ALGER_data_filtered <- ALGER_data_filtered %>%
  mutate(PM2.5_predict_normalized = (PM2.5_predict - min(PM2.5_predict, na.rm = TRUE)) / (max(PM2.5_predict, na.rm = TRUE) - min(PM2.5_predict, na.rm = TRUE)),
         Lockdown = lockdown)

# Créer un graphique avec PM2.5 normalisé et les périodes de confinement
ggplot(ALGER_data_filtered, aes(x = Date)) +
  geom_line(aes(y = PM2.5_predict_normalized, color = "PM2.5 Predict (Normalized)"), size = 1) +
  geom_line(aes(y = Lockdown, color = "Lockdown"), size = 1) +
  scale_y_continuous(name = "Normalized Values", sec.axis = sec_axis(~ ., name = "Lockdown (0 or 1)")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("PM2.5 Predict (Normalized)" = "darkred", "Lockdown" = "red")) +
  labs(title = "Normalized PM2.5 and Lockdown Periods in Algiers (Mar 2020 - Feb 2021)",
       x = "Date", color = "Legend") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = c(0.9, 0.5), # Position de la légende à droite du graphique
    legend.justification = "center",
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14)
  ) +
  annotate("rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-06-14"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = ymd("2020-11-01"), xmax = ymd("2021-01-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")

############################################################################################
# AQI and lockdowns in Algiers 
library(ggplot2)
library(lubridate)
library(dplyr)

# Filtrer les données pour inclure uniquement la période de mars 2020 à février 2021
ALGER_data_filtered <- ALGER_data %>%
  filter(Date >= ymd("2020-03-01") & Date <= ymd("2021-02-28"))

# Créer la séquence de dates pour la période filtrée
dates_filtered <- ALGER_data_filtered$Date

# Initialiser les variables
lockdown <- rep(0, length(dates_filtered))

# Définir les périodes de confinement
lockdown_periods <- list(
  c(ymd("2020-03-23"), ymd("2020-06-14")),
  c(ymd("2020-11-01"), ymd("2021-01-31"))
)

# Mettre à jour les valeurs de confinement
for (period in lockdown_periods) {
  start_date <- period[1]
  end_date <- period[2]
  lockdown[dates_filtered >= start_date & dates_filtered <= end_date] <- 1
}

# Normaliser les valeurs d'AQI
ALGER_data_filtered <- ALGER_data_filtered %>%
  mutate(
    AQI_predict_normalized = (AQI_predict - min(AQI_predict, na.rm = TRUE)) / (max(AQI_predict, na.rm = TRUE) - min(AQI_predict, na.rm = TRUE)),
    Lockdown = lockdown
  )

# Créer un graphique avec AQI normalisé et les périodes de confinement
ggplot(ALGER_data_filtered, aes(x = Date)) +
  geom_line(aes(y = AQI_predict_normalized, color = "AQI Predict (Normalized)"), size = 1) +
  geom_line(aes(y = Lockdown * max(AQI_predict_normalized, na.rm = TRUE), color = "Lockdown"), size = 1) +
  scale_y_continuous(name = "Normalized AQI", sec.axis = sec_axis(~ ., name = "Lockdown (0 or 1)")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("AQI Predict (Normalized)" = "darkgreen", "Lockdown" = "red")) +
  labs(title = "Normalized AQI with Lockdown Periods in Algiers (Mar 2020 - Feb 2021)",
       x = "Date", color = "Legend") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom", # Position de la légende en bas
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.margin = margin(10, 10, 10, 10) # Ajouter de l'espace autour de la légende
  ) +
  annotate("rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-06-14"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = ymd("2020-11-01"), xmax = ymd("2021-01-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red")
