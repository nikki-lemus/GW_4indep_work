# 2. Correlation among predictors
cor_matrix <- cor(data[, c("predictor1", "predictor2", "predictor3")])

# Save correlation matrix as a plot
png("correlation_plot.png")
pairs(data[, c("predictor1", "predictor2", "predictor3")], main = "Correlation Plot")
dev.off()

# 3. Fit GLMs with different combinations of predictors
model1 <- glm(response ~ predictor1, data = data)
model2 <- glm(response ~ predictor1 + predictor2, data = data)
model3 <- glm(response ~ predictor1 + predictor2 + predictor3, data = data)

# 4. Compare models based on AIC
aic_values <- AIC(model1, model2, model3)

# Export AIC values to a text file
write.table(aic_values, file = "AIC_values.txt", sep = "\t", col.names = NA, quote = FALSE)

# 5. Select the best model based on AIC
best_model_index <- which.min(aic_values$AIC)
best_model <- list(model1, model2, model3)[[best_model_index]]
best_model_summary <- summary(best_model)

# Export the summary of the best model to a text file
capture.output(best_model_summary, file = "best_model_summary.txt")

# 6. Plot the response curves for the best model
fitted_values <- fitted(best_model)

# Save response curve plot
png("response_curve_plot.png")
plot(fitted_values, data$response, main = "Response vs Fitted Values",
     xlab = "Fitted Values", ylab = "Response", pch = 16, col = "blue")
abline(lm(data$response ~ fitted_values), col = "red", lwd = 2)
dev.off()
