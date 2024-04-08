rm(list=ls(all=TRUE))

set.seed(42)  # Set seed for reproducibility

# Generate n values from 5 to 1000
n_values <- seq(5, 1000, 20)

# Initialize a vector to store the fraction of intervals that include the true mean
fractions_t <- numeric(length(n_values))

# Loop over different sample sizes
for (i in seq_along(n_values)) {
  n <- n_values[i]
  
  included_mu_count <- 0
  
  # Number of simulation runs
  num_samples <- 1000
  
  for (j in 1:num_samples) {
    # Generate n random samples from a normal distribution
    samples <- rnorm(n, mean = 0, sd = 1)
    
    # Calculate the sample mean and standard error of the mean
    xbar <- mean(samples)
    
    # Calculate the margin of error
    margin <- qt(0.975, df= n - 1) * sd(samples) / sqrt(n)
    
    # Calculate the confidence interval
    lower_bound <- xbar - margin
    upper_bound <- xbar + margin
    
    # Check if the true mean is within the interval
    if (lower_bound <= 0 && 0 <= upper_bound) {
      included_mu_count <- included_mu_count + 1
    }
  }
  
  # Calculate the fraction of intervals that include the true mean
  fractions_t[i] <- included_mu_count / num_samples
}

# Plot the results
plot(n_values, fractions_t, type = 'o', col = 'blue', xlab = 'n', ylab = 'Fraction of Intervals that include μ', main = '95% Confidence Intervals')
abline(h = 0.95, col = 'red', lty = 2, label = '95% Confidence Level')
legend('bottomright', legend = '95% Confidence Level', col = 'red', lty = 2)

# Create a histogram of the generated data
hist(samples, probability = TRUE, main = "Normal Distribution overlay on Histogram ", xlab = "Sample data")

# Overlay a standard normal distribution curve on the histogram
curve(dnorm(x, mean = 0, sd = 1), col = "blue", lwd = 2, add = TRUE)

