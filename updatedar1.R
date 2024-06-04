library(skd)
library(energy)
library(mvtnorm)
library(sn)
library(ggplot2)
library(utils)  # For the progress bar

# Function to generate AR(1) process data with specified coefficient using arima.sim
generate_ar1_process <- function(n, phi, d) {
  ar1 <- function(n, phi) {
    arima.sim(n = n, list(ar = phi))
  }
  return(t(replicate(d, ar1(n, phi))))
}

# Parameters
sample_size <- 250
replications <- 200
dimensions <- c(205, 405, 603, 803, 1003, 1203, 1401, 1601, 1801, 2001)
phi_values <- c(0.1)

# Function to perform energy test
perform_energy_test <- function(data1, data2, R = 1000) {
  etest_result <- eqdist.etest(rbind(data1, data2), sizes= c(nrow(data1),nrow(data2)), R = R)
  return(etest_result$p.value)
}

# Function to perform SKD test
perform_skd_test <- function(data1, data2) {
  stest_result <- equal.test(data1, data2, nboot = 1000)
  return(stest_result$p.value)
}

# Running the tests
results <- list()

for (d in dimensions) {
  for (phi in phi_values) {
    power_skd <- numeric(replications)
    power_1000 <- numeric(replications)
    cat("Dimension:", d, "Phi:", phi, "\n")
    
    # Initialize progress bar
    pb <- txtProgressBar(min = 0, max = replications, style = 3)
    
    for (i in 1:replications) {
      # Generate AR(1) process data with different coefficients
      data1 <- generate_ar1_process(sample_size, phi, d)
      data2 <- generate_ar1_process(sample_size, phi + 0.1, d)
      
      # Perform the energy tests
      p_value_skd <- perform_skd_test(data1, data2)
      p_value_1000 <- perform_energy_test(data1, data2, R = 1000)
      power_skd[i] <- p_value_skd < 0.05
      power_1000[i] <- p_value_1000 < 0.05
      
      # Update progress bar
      setTxtProgressBar(pb, i)
    }
    
    # Close progress bar
    close(pb)
    
    results[[paste("Dim", d, "Phi", phi, "SKD")]] <- mean(power_skd)
    results[[paste("Dim", d, "Phi", phi, "R1000")]] <- mean(power_1000)
  }
}

# Convert results to a data frame for plotting
results_df <- data.frame(
  Dimension = rep(dimensions, each = length(phi_values) * 2),
  Phi = rep(rep(phi_values, each = 2), times = length(dimensions)),
  Test = rep(c("SKD", "R1000"), times = length(dimensions) * length(phi_values)),
  Power = unlist(results)
)

# Plotting the results
ggplot(results_df, aes(x = Dimension, y = Power, color = Test, linetype = as.factor(Phi), group = interaction(Test, Phi))) +
  geom_line() +
  geom_point() +
  labs(title = "Test Power for AR(1) Processes Across Different Dimensions",
       x = "Dimension",
       y = "Power (Proportion of Correctly Rejected H0)",
       color = "Test",
       linetype = "Phi") +
  theme_minimal()
