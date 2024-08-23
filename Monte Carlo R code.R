# Set seed for reproducibility
set.seed(123)

# Define parameters
num_cases <- 5  # Number of simulation cases
num_means <- 5  # Number of net contamination means to simulate
num_iterations <- 50  # Number of Monte Carlo simulations per mean
mean_start <- 0.3  # Starting value for net contamination mean
mean_end <- 1.2  # Ending value for net contamination mean
mean_increment <- (mean_end - mean_start) / (num_means - 1)
DCGL <- 1  # Defined threshold for DCGL

# Placeholder for alpha level for critical value calculation
alpha <- 0.05

# Function to calculate critical value for the Sign test
calculate_critical_value_sign <- function(sample_size, alpha) {
  return(qbinom(alpha, sample_size, 0.5, lower.tail = FALSE))
}

# Function to calculate the test statistic for the Sign test
calculate_test_statistic_sign <- function(samples, dcgl) {
  return(sum(samples > dcgl))
}

# Function to perform the WRS test
perform_WRS_test <- function(sample_size, alpha) {
  # Generate two sets of random samples
  samples_survey <- rnorm(sample_size / 2, mean = DCGL + 0.1)  # Adjusted for gross contamination
  samples_reference <- rnorm(sample_size / 2, mean = 0.1)  # Background contamination
  
  # Perform the WRS test
  test_result <- wilcox.test(samples_survey, samples_reference, alternative = "greater")
  
  # Determine if the test statistic is significant
  return(test_result$p.value > alpha)
}

# Initialize a list to store the results
results <- list()

# Main simulation loop
for (n in 1:num_cases) {
  # Calculate sample size derived from your specific conditions
  sample_size_n <- 5
  
  # Critical value for the Sign test
  critical_value_n_sign <- calculate_critical_value_sign(sample_size_n, alpha)
  
  # Loop over contamination means
  for (mean_x in seq(mean_start, mean_end, by = mean_increment)) {
    # Initialize counter for pass events for Sign and WRS tests
    c_pass_events_sign <- 0
    c_pass_events_WRS <- 0
    
    for (i in 1:num_iterations) {
      # Generate random samples for this iteration
      samples <- rnorm(sample_size_n, mean = mean_x, sd = 1)
      
      # Calculate test statistic for Sign test
      test_statistic_sign <- calculate_test_statistic_sign(samples, DCGL)
      
      # Check if the test statistic exceeds the critical value for the Sign test
      if (test_statistic_sign > critical_value_n_sign) {
        c_pass_events_sign <- c_pass_events_sign + 1
      }
      
      # Perform WRS test and check if the null hypothesis is rejected
      if (perform_WRS_test(sample_size_n, alpha)) {
        c_pass_events_WRS <- c_pass_events_WRS + 1
      }
    }
    
    # Calculate the power at the current mean_x for Sign and WRS tests
    power_x_sign <- c_pass_events_sign / num_iterations
    power_x_WRS <- c_pass_events_WRS / num_iterations
    
    # Save the results in a list
    results[[paste("Case", n, "Mean", mean_x)]] <- list(
      Sign_Test_Power = power_x_sign,
      WRS_Test_Power = power_x_WRS
    )
  }
}

# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("openxlsx")) install.packages("openxlsx")
library(ggplot2)
library(knitr)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert the nested results list into a data frame
results_df <- do.call(rbind, lapply(seq_along(results), function(n) {
  data.frame(
    Case = rep(n, num_means),
    Mean = seq(mean_start, mean_end, by = mean_increment),
    Sign_Test_Power = sapply(results[n], function(x) x$Sign_Test_Power),
    WRS_Test_Power = sapply(results[n], function(x) x$WRS_Test_Power)
  )
}))

# Print the first few rows of the results
print(head(results_df))

# Save the full results to a CSV file
write.csv(results_df, "Results.csv", row.names = FALSE)
read.csv("Monte_Carlo_Simulation_Results.csv")
# Plotting with ggplot2
ggplot(data = results_df, aes(x = Mean)) + 
  geom_line(aes(y = Sign_Test_Power, colour = "Sign Test Power")) + 
  geom_line(aes(y = WRS_Test_Power, colour = "WRS Test Power")) + 
  labs(title = "Power vs. Net Contamination Mean",
       x = "Net Contamination Mean",
       y = "Power") +
  theme_minimal() +
  scale_colour_manual("", 
                      breaks = c("Sign Test Power", "WRS Test Power"),
                      values = c("blue", "red"))

# If you want to create a separate plot for each case, you can use facet_wrap
ggplot(data = results_df, aes(x = Mean)) + 
  geom_line(aes(y = Sign_Test_Power, colour = "Sign Test Power")) + 
  geom_line(aes(y = WRS_Test_Power, colour = "WRS Test Power")) + 
  facet_wrap(~Case, scales = 'free_y') +
  labs(title = "Power vs. Net Contamination Mean",
       x = "Net Contamination Mean",
       y = "Power") +
  theme_minimal() +
  scale_colour_manual("", 
                      breaks = c("Sign Test Power", "WRS Test Power"),
                      values = c("blue", "red"))
# Write the results data frame to an Excel file
write.xlsx(results_df, file = "Results.xlsx")
read.csv(Results.xlsx)
read.csv(results_df)

