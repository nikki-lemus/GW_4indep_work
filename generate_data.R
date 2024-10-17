# Set seed for reporduciiblity
set.seed(123)

# 1. Data generation
n <- 500  # Number of observations
predictor1 <- rnorm(n, mean = 50, sd = 10)
predictor2 <- rnorm(n, mean = 30, sd = 5)
predictor3 <- rnorm(n, mean = 80, sd = 20)
response <- 3 + 1.5 * predictor1 - 0.5 * predictor2 + 2 * predictor3 + rnorm(n)

# Create a data frame
data <- data.frame(response, predictor1, predictor2, predictor3)

# Export the genearted data as CSV
write.csv(data, "generated_data.csv", row.names = FALSE)
