library(skd)
library(energy)
library(mvtnorm)
library(sn)
library(ggplot2)
library(utils)  # For the progress bar

plotRes <- function(results_df){
  ggplot(results_df, aes(x = Dimension, y = Power, color = R, group = R)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = c("SKD" = "blue", "ET" = "red")) +
    labs(title = "Energy Test Power Across Different Dimensions",
         x = "Dimension",
         y = "Power (Proportion of Correctly Rejected H0)",
         color = "Method") +
    theme_minimal()
}

testar <- function(phi1, phi2){
# Function to generate AR(1) process data with specified coefficient using arima.sim
generate_ar1_process <- function(n, phi, d) {
  ar1 <- function(n, phi) {
    arima.sim(n = d, list(ar = phi))
  }
  return(t(replicate(n, ar1(n, phi))))
}

# Parameters
sample_size <- 200
replications <- 200
dimensions <- c(205, 405, 603, 803, 1003, 1203, 1401, 1601)
phi_values <- c(phi1)

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
    power_skd <- numeric(replications)
    power_1000 <- numeric(replications)
    cat("Dimension:", d, "\n")
    
    # Initialize progress bar
    pb <- txtProgressBar(min = 0, max = replications, style = 3)
    
    for (i in 1:replications) {
      # Generate AR(1) process data with different coefficients
      data1 <- generate_ar1_process(sample_size, phi1, d)
      data2 <- generate_ar1_process(sample_size, phi2, d)
      
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
    
    results[[paste("Dim", d, "SKD")]] <- mean(power_skd)
    results[[paste("Dim", d, "ET")]] <- mean(power_1000)
}

# Convert results to a data frame for plotting
results_df <- data.frame(
  Dimension = rep(dimensions, each = 2),
  R = rep(c("SKD", "ET"), each = 1, times = length(dimensions)),
  Power = unlist(results)
)


return(results_df)
}

arres <- testar(0.1,0.2)
plotRes(arres)

arres0 <- testar(0.1,0.1)
plotRes(arres0)
arres2 <- testar(0.1,0.3)
plotRes(arres2)

