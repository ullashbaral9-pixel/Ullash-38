# 1. Function to calculate Standard Deviation
calc_sd <- function(data) {
  return(sd(data, na.rm = TRUE))
}

# 2. Function to calculate Mean
calc_mean <- function(data) 
    n <- length(data)
  if (n == 0) return(0)
  
  total_sum <- sum(data, na.rm = TRUE)
  mean <- total_sum / n
  return(mean)
}

# 3. Function to calculate Coefficient of Variation (CV as ratio)
# Calls calc_sd and calc_mean internally, stores results, and calculates CV
calc_cv <- function(sd_data, mean_data) {
  calculated_sd <- calc_sd(sd_data)
  calculated_mean <- calc_mean(mean_data)
  
  cv <- calculated_sd / calculated_mean
  return(cv)
}

# 4. Function to convert CV to percentage and return it
calc_cv_percent <- function(sd_data, mean_data) {
  cv_ratio <- calc_cv(sd_data, mean_data)
  cv_percentage <- cv_ratio * 100
  return(cv_percentage)
}

# --- Example Usage / Taking User Input ---

# Example vector of numerical data
sample_data <- c(12.5, 15.2, 18.9, 23.3, 25.8)

# Calculate CV percentage
final_cv_percent <- calc_cv_percent(sample_data, sample_data)

# Print the result
cat("Coefficient of Variation (%):", round(final_cv_percent, 2), "%\n")

calc_mean <- function(data) {
    return(mean(data, na.rm = TRUE))
}

user_input_data <- c(12.5, 15.2, 18.9, 23.3, 25.8)

calculated_mean <- calc_mean(user_input_data)

cat("Formula Mean:", calc_mean(user_input_data), "\n")

calc_sd <-function(data) {
    return(sd(data, na.rm = TRUE))
} 
calculated_sd <-calc_sd(user_input_data)

cat("standard Deviation:", calculated_sd, "\n")

calc_cv <- function(data) {
    calculated_sd <- calc_sd(data)
    calculated_mean <- calc_mean(data)
    cv_ratio <- calculated_sd / calculated_mean
     return(cv_ratio)
}

user_data <- c(12.5, 15.2, 18.9, 23.3, 25.8)

cv_result <- calc_cv(user_data)

cat("Coefficient of Variation (Ratio):", cv_result, "\n")

