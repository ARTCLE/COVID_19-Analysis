library(readr)
library(devtools)
library(estimateR)
library(EstimateGroupNetwork)

######################### Load datasets

####### 1 City Data
city_data <- read_csv("path_to_your_data/city_data.csv")
####### AQI_Predict
AQI_Predict <- read_csv("path_to_your_data/data_AQI.csv")
####### PM2.5_Predict
PM2.5_Predict <- read_csv("path_to_your_data/data_PM2.5.csv")

################### Effective Reproduction Number ########################

start_date <- as.Date("your_start_date")
incidence_data <- as.numeric(city_data$your_column_name)

shape_onset_to_report <- your_value_1
scale_onset_to_report <- your_value_2
onset_to_report <- list(name = "gamma", 
                        shape = shape_onset_to_report, 
                        scale = scale_onset_to_report)

shape_incubation <- your_value_3
scale_incubation <- your_value_4
incubation <- list(name = "gamma", shape = shape_incubation, scale = scale_incubation)

mean_serial_interval <- your_value_5
std_serial_interval <- your_value_6

estimation_window <- your_value_7

incidence_data <- incidence_data[!is.na(incidence_data)]
estimates <- estimate_Re_from_noisy_delayed_incidence(incidence_data,
                                                      smoothing_method = "LOESS",
                                                      deconvolution_method = "Richardson-Lucy delay distribution",
                                                      estimation_method = "EpiEstim sliding window",
                                                      delay = list(incubation, onset_to_report),
                                                      estimation_window = estimation_window,
                                                      mean_serial_interval = mean_serial_interval,
                                                      std_serial_interval = std_serial_interval,
                                                      output_Re_only = FALSE,
                                                      ref_date = start_date,
                                                      time_step = "day")
tail(estimates)

View(estimates)

estimates$Re_estimate
