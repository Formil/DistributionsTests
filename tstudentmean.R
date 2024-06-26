library(skd) 
library(energy)
library(mvtnorm)
library(sn)
library(ggplot2)
library(mvnfast)
# Function to generate t-distribution data

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

testTs <- function(m1,m2){
  generate_t_distribution <- function(n, df=3, d, m) {
    #mu <- matrix(0, nrow = 1, ncol = d)  # Mean vector (1 component, d dimensions)
    mu <- rep(m,d)
    sigma <- diag(d)  # Covariance matrix (identity matrix for no covariance)
    w= c(1)
    samples <- rmvt(n, mu, sigma, df, w)
    return(samples)
  } 
  
  
  # Parameters
  sample_size <- 200
  replications <- 200
  dimensions <- c(205, 405, 603, 803, 1003, 1203, 1401, 1601)
  #dimensions <- c(1203, 1401, 1601)
  
  # Function to perform energy test
  perform_energy_test <- function(data1, data2, R = 1000) {
    etest_result <- eqdist.etest(rbind(data1, data2), sizes= c(nrow(data1),nrow(data2)),R = R)
    return(etest_result$p.value)
  }
  
  perform_skd_test <- function(data1, data2){
    stest_result <- equal.test(data1, data2, nboot = 1000)
    return(stest_result$p.value)
  }
  
  results <- list()
  
  for (d in dimensions) {
    power_skd <- numeric(replications)
    power_1000 <- numeric(replications)
    print("dim")
    print(d)
    pb <- txtProgressBar(min = 0, max = replications, style = 3)
    
    for (i in 1:replications) {
      # Generate data
      data1 <- generate_t_distribution(sample_size, df = 3, d,m1)
      data2 <- generate_t_distribution(sample_size, df = 3, d,m2)
      
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
  
  results_df <- data.frame(
    Dimension = rep(dimensions, each = 2),
    R = rep(c("SKD", "ET"), each = 1, times = length(dimensions)),
    Power = unlist(results)
  )
  
  return(results_df)
}

rest121 = testTs(0,0)
rest122 = testTs(0,0.01)
rest123 = testTs(0,0.02)
rest124 = testTs(0,0.05)


plotRes(rest121)
plotRes(rest122)
plotRes(rest123)
plotRes(rest124)
