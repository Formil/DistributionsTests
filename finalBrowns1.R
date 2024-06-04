library(e1071)
library(skd)
library(energy)
library(ggplot2)

# Function to generate Brownian motion data
generate_brownian_motion <- function(n, d) {
  return(t(sapply(1:d, function(x) rwiener(n))))
}

# Function to generate Brownian bridge data
generate_brownian_bridge <- function(n, d) {
  return(t(sapply(1:d, function(x) rbridge(n))))
}

# Parameters
sample_size <- 250
replications <- 200
dimensions <- c(205, 405, 603, 803, 1003, 1203, 1401, 1601, 1801, 2001)

# Function to perform energy test
perform_energy_test <- function(data1, data2, R = 1000) {
  etest_result <- eqdist.etest(rbind(data1, data2), sizes = c(nrow(data1), nrow(data2)), R = R)
  return(etest_result$p.value)
}

# Function to perform SKD test
perform_skd_test <- function(data1, data2) {
  stest_result <- equal.test(data1, data2, nboot = 1000)
  return(stest_result$p.value)
}

results <- list()

for (d in dimensions) {
  power_skd <- numeric(replications)
  power_1000 <- numeric(replications)
  print(paste("Dimension:", d))
  pb <- txtProgressBar(min = 0, max = replications, style = 3)
  
  for (i in 1:replications) {
    # Generate data
    data1 <- generate_brownian_motion(d, sample_size)
    data2 <- generate_brownian_bridge(d, sample_size)
    
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
  results[[paste("Dim", d, "R1000")]] <- mean(power_1000)
}

results_df <- data.frame(
  Dimension = rep(dimensions, each = 2),
  R = rep(c("SKD", "R1000"), times = length(dimensions)),
  Power = unlist(results)
)

ggplot(results_df, aes(x = Dimension, y = Power, color = R, group = R)) +
  geom_line() +
  geom_point() +
  labs(title = "Test Power Across Different Dimensions",
       x = "Dimension",
       y = "Power (Proportion of Correctly Rejected H0)",
       color = "R Value") +
  theme_minimal()

