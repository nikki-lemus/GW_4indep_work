# Set seed for reproducibility
set.seed(123)

# 1. Load the previously generated data
data <- read.csv("generated_data.csv")

# 2. Expand the dataset by adding a quadratic term for predictor1
predictor1_quad <- data$predictor1^2
data$predictor1_quad <- predictor1_quad

# Generate new quadratic response for predictor1
response_new <- 3 + 1.5 * data$predictor1 - 0.5 * data$predictor2 +
  2 * data$predictor3 - 0.05 * data$predictor1^2 + rnorm(nrow(data))

# Replace the response in the data frame with the new quadratic response
data$response <- response_new

# 3. Fit the previously tested models (linear models)
linear_model1 <- glm(response ~ predictor1, data = data)
linear_model2 <- glm(response ~ predictor1 + predictor2, data = data)
linear_model3 <- glm(response ~ predictor1 + predictor2 + predictor3, data = data)

# 4. Now, fit models with the quadratic term for predictor1
quad_model1 <- glm(response ~ predictor1 + predictor1_quad, data = data)
quad_model2 <- glm(response ~ predictor1 + predictor2 + predictor1_quad, data = data)
quad_model3 <- glm(response ~ predictor1 + predictor2 + predictor3 + predictor1_quad, data = data)

# 5. Compare all models (linear and quadratic) based on AIC
aic_values <- AIC(linear_model1, linear_model2, linear_model3, quad_model1, quad_model2, quad_model3)

# Export AIC values to a text file
write.table(aic_values, file = "AIC_values_extdata.txt", sep = "\t", col.names = NA, quote = FALSE)

# 6. Select the best model based on AIC
best_model_index <- which.min(aic_values$AIC)
all_models <- list(linear_model1, linear_model2, linear_model3, quad_model1, quad_model2, quad_model3)
best_model <- all_models[[best_model_index]]
best_model_summary <- summary(best_model)

# Export the summary of the best model to a text file
capture.output(best_model_summary, file = "best_model_summary_extdata.txt")

# 7. Save correlation matrix for the expanded data
cor_matrix_extdata <- cor(data[, c("predictor1", "predictor2", "predictor3", "predictor1_quad")])

# Save correlation plot
png("correlation_plot_extdata.png")
pairs(data[, c("predictor1", "predictor2", "predictor3", "predictor1_quad")],
      main = "Correlation Plot (Extended Data)")
dev.off()

# 8. Plot the response curves for the best model (linear or quadratic)
fitted_values <- fitted(best_model)

# Save response curve plot
png("response_curve_plot_extdata.png")
plot(fitted_values, data$response, main = "Response vs Fitted Values (Extended Data)",
     xlab = "Fitted Values", ylab = "Response", pch = 16, col = "blue")
abline(lm(data$response ~ fitted_values), col = "red", lwd = 2)
dev.off()

# 9. Export the updated dataset as CSV
write.csv(data, "generated_extdata.csv", row.names = FALSE)

