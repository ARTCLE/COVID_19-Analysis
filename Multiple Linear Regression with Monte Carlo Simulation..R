
x1 <- data_A$Var1  # independent variable 1 AQI_predict
x2 <- data_A$Var2  # independent variable 2 PM2.5_predict
y <- data_A$Var3   # dependent variable  Re_estimate

# run the Original model
model <- lm(y ~ x1 + x2)
summary_original <- summary(model)

# the Monte Carlo simulation
n_sims <- 10000
coefficients <- matrix(0, n_sims, 3)  # to store the coefficients
rsquared <- numeric(n_sims)  # to store the R-squared values

for (i in 1:n_sims) {
  # Confidence interval
  random_R <- quantile(data_A$Var3, c(0.025, 0.975)) 
  
  # Use the randomly drawn R value in the interval to fit the regression model
  y_sim <- runif(1, min = random_R[1], max = random_R[2]) * x1 + runif(1, min = random_R[1], max = random_R[2]) * x2 + rnorm(length(y))  # Modify according to your model
  model_sim <- lm(y_sim ~ x1 + x2)
  
  # Store the coefficients
  coefficients[i, ] <- coef(model_sim)
  
  # Store the R-squared value
  rsquared[i] <- summary(model_sim)$r.squared
}

# Calculate summary statistics for coefficients and R-squared values
summary_coefficients <- apply(coefficients, 2, function(x) c(Mean = mean(x), SD = sd(x), Q25 = quantile(x, 0.25), Median = median(x), Q75 = quantile(x, 0.75)))
summary_rsquared <- c(Mean = mean(rsquared), SD = sd(rsquared), Q25 = quantile(rsquared, 0.25), Median = median(rsquared), Q75 = quantile(rsquared, 0.75))

# Create a combined summary
combined_summary <- list(
  Original_Model_Summary = summary_original,
  Monte_Carlo_Coefficients = summary_coefficients,
  Monte_Carlo_Rsquared = summary_rsquared
)

# Print the combined summary
print(combined_summary)

########################################################
############# check the model ##########################
# Calculate residuals
residuals <- residuals(model_sim)

# Create a histogram of residuals
hist(residuals, main = "Distribution of Residuals from Simple Linear Model with Monte Carlo Simulation", xlab = "Residuals", col = "skyblue", border = "black")

# Plot residuals versus predicted values to assess homoscedasticity
plot(model$fitted.values, residuals, main = "Homoscedasticity of Residuals from Simple Linear Model with Monte Carlo Simulation", xlab = "Predicted Values", ylab = "Residuals", col = "steelblue")
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 for reference
