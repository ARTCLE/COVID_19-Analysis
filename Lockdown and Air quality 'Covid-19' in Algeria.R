################################################################################
# Load necessary libraries
library(ggplot2)
library(lubridate)
library(dplyr)

# Create the sequence of dates
dates <- seq(ymd("2020-03-01"), ymd("2022-03-31"), by = "day")

# Initialize variables
lockdown <- rep(0, length(dates))
waves <- rep(0, length(dates))

# Define lockdown periods
lockdown_periods <- list(
  c(ymd("2020-03-23"), ymd("2020-06-14")),
  c(ymd("2020-11-01"), ymd("2021-01-31"))
)

# Update lockdown values
for (period in lockdown_periods) {
  start_date <- period[1]
  end_date <- period[2]
  lockdown[dates >= start_date & dates <= end_date] <- 1
}

# Define COVID-19 wave periods
wave_periods <- list(
  c(ymd("2020-03-01"), ymd("2020-06-30")),
  c(ymd("2020-10-01"), ymd("2021-02-28")),
  c(ymd("2021-04-01"), ymd("2021-07-31")),
  c(ymd("2021-11-01"), ymd("2022-04-30"))
)

# Update wave values
for (i in seq_along(wave_periods)) {
  period <- wave_periods[[i]]
  start_date <- period[1]
  end_date <- period[2]
  waves[dates >= start_date & dates <= end_date] <- i
}

# Create a data frame
data <- data.frame(
  Date = dates,
  Lockdown = lockdown,
  Wave = as.factor(waves)
)

# Create a line plot with direct lines for lockdown periods and waves
ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Lockdown, color = "Lockdown"), size = 1) +
  geom_line(aes(y = as.numeric(Wave), color = "Wave"), linetype = "dashed", size = 1) +
  scale_y_continuous(name = "Lockdown (1 = Yes, 0 = No)", sec.axis = sec_axis(~., name = "COVID-19 Wave", breaks = 1:4, labels = paste("Wave", 1:4))) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Lockdown" = "red", "Wave" = "blue")) +
  labs(title = "Lockdown Periods and COVID-19 Waves in Algeria",
       x = "Date", color = "Legend") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = c(0.9, 0.5), # Position legend to the right of the plot
    legend.justification = "center",
    legend.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 14)
  ) +
  annotate("rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-06-14"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = ymd("2020-11-01"), xmax = ymd("2021-01-31"), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
  annotate("rect", xmin = ymd("2020-03-01"), xmax = ymd("2020-06-30"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = ymd("2020-10-01"), xmax = ymd("2021-02-28"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-07-31"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = ymd("2021-11-01"), xmax = ymd("2022-04-30"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "blue")
