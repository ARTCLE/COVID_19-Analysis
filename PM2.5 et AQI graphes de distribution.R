library(ggplot2)
library(lubridate)
library(dplyr)

ALGER_data = na.omit(ALGER_data)
# Ajouter des colonnes pour l'année
ALGER_data <- ALGER_data %>%
  mutate(Year = year(Date))

# Graphique pour AQI_predict
ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = AQI_predict, color = "AQI_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Indice de qualité de l'air (AQI) à Alger par Année",
       x = "Date", y = "Indice de qualité de l'air",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("AQI_predict" = "darkgreen"))



# Graphique pour AQI_predict
ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = PM2.5_predict, color = "PM2.5_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "PM2.5 à Alger par Année",
       x = "Date", y = "PM2.5",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("PM2.5_predict" = "darkred"))

############## en anglais ############"
library(ggplot2)
library(lubridate)
library(dplyr)

ALGER_data = na.omit(ALGER_data)
# Add columns for the year
ALGER_data <- ALGER_data %>%
  mutate(Year = year(Date))

# Plot for AQI_predict
ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = AQI_predict, color = "AQI_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Air Quality Index (AQI) in Algiers by Year",
       x = "Date", y = "Air Quality Index",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("AQI_predict" = "darkgreen"))

# Plot for PM2.5_predict
ggplot(ALGER_data, aes(x = Date)) +
  geom_line(aes(y = PM2.5_predict, color = "PM2.5_predict")) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "PM2.5 in Algiers by Year",
       x = "Date", y = "PM2.5",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("PM2.5_predict" = "darkred"))