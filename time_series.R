data <- read.csv("snotel_swe_data.csv")

library(dplyr)
library(tseries)

# Filter out rows with NA values
na_data <- data %>% filter(is.na(snow_water_equivalent))

# Get unique station ids
na_station_ids <- na_data %>% distinct(station_id) %>% pull(station_id)

data <- data %>% filter(!station_id %in% na_station_ids)

#Step 1: Check for stationarity using ADF test and stationarize it using 
#differencing if it's not stationary.

# Loop over each site and check stationarity of SWE data
for (site_id in unique(data$station_id)) {
  stationary = TRUE
  # Subset the data for the current site
  site_data <- data %>% filter(station_id == site_id)
  # Run the ADF test to check for stationarity
  adf_result <- adf.test(site_data$snow_water_equivalent)
  # If data is not stationary, stationarize it using differencing
  if (adf_result$p.value < 0.05) {
    # Perform differencing to stationarize the data
    temp <- diff(site_data$snow_water_equivalent, differences = 1)
    site_data <- site_data[-1,] # remove first row since differencing causes NAs
    site_data$snow_water_equivalent <- temp
    
    # Update the data object with the stationarized data
    data <- data %>% filter(station_id != site_id) %>% bind_rows(site_data)
  }
}

data

data_copy <- data

#Step 2: Build Arima model for each site and evaluate accuracy
library(forecast)

#We only have time to use the first 50 stations
#This filters down to those stations
#data <- data %>% filter(station_id %in% station_id[1:182135])

accuracies = list()

# Function to train and test a seasonal ARIMA model on a given dataset
train_test_arima <- function(site_data, order, seasonal_order, h = 60) {
  # Split the data into training and testing sets
  n <- nrow(site_data)
  train <- site_data[1:(n-h),]
  test <- site_data[(n-h+1):n,]
  
  # Fit a seasonal ARIMA model on the training data
  model <- Arima(train$snow_water_equivalent, order = order, seasonal = seasonal_order)
  
  # Make predictions on the test data
  forecast <- forecast(model, h = h)
  
  # Return the test set and the predictions
  return(list(test = test$snow_water_equivalent, forecast = forecast$mean))
}
# Loop over unique station ids
for (site_id in unique(data$station_id)) {
  # Subset the data for the current site
  site_data <- data %>% filter(station_id == site_id)
  # Set the order and seasonal order for the ARIMA model
  order <- c(1,1,1)
  seasonal_order <- c(1,1,1)
  
  # Set the window size for rolling window cross-validation
  window_size <- 365
  
  # Initialize a list to store the test and forecast values
  test_forecast <- list()
  
  # Loop over each window of data and train/test an ARIMA model
  for (i in seq(window_size, nrow(site_data), by = window_size)) {
    site_data_window <- site_data[1:i,]
    
    # Train and test the ARIMA model on the current window of data
    results <- train_test_arima(site_data_window, order = order, seasonal_order = seasonal_order)
    
    # Add the test and forecast values to the list
    test_forecast$test <- c(test_forecast$test, results$test)
    test_forecast$forecast <- c(test_forecast$forecast, results$forecast)
  }
  
  # Calculate the accuracy measures for the model
  accuracy_metrics <- accuracy(test_forecast$forecast, test_forecast$test)
  
  # Print the accuracy measures
  cat("Site ID:", site_id, "\n")
  
  cat("RMSE:", accuracy_metrics[2], "\n")
  cat("MAE:", accuracy_metrics[3], "\n\n")
  accuracies = append(accuracies, accuracy_metrics[2])
  print(length(accuracies))
}

mean(unlist(accuracies))
hist(unlist(accuracies))
boxplot(unlist(accuracies))

#Step 3: Build exponential smoothing model for each site and evaluate accuracy

# Function to train and test an exponential smoothing model on a given dataset
train_test_ets <- function(site_data, h = 60) {
  # Split the data into training and testing sets
  n <- nrow(site_data)
  train <- site_data[1:(n-h), ]
  test <- site_data[(n-h+1):n, ]
  
  # Fit an exponential smoothing model on the training data
  fit <- ets(train$snow_water_equivalent)
  
  # Make predictions on the test data
  forecast <- forecast(fit, h = h)
  
  # Return the test set and the predictions
  return(list(test = test$snow_water_equivalent, forecast = forecast$mean))
}

# Loop over unique station ids
accuracies_exp = list()
for (site_id in unique(data$station_id)) {
  # Subset the data for the current site
  site_data <- data %>% filter(station_id == site_id)
  
  # Set the window size for rolling window cross-validation
  window_size <- 365
  
  # Initialize a list to store the test and forecast values
  test_forecast <- list()
  
  # Loop over each window of data and train/test an exponential smoothing model
  for (i in seq(window_size, nrow(site_data), by = window_size)) {
    site_data_window <- site_data[1:i, ]
    
    # Train and test the exponential smoothing model on the current window of data
    results <- train_test_ets(site_data_window)
    
    # Add the test and forecast values to the list
    test_forecast$test <- c(test_forecast$test, results$test)
    test_forecast$forecast <- c(test_forecast$forecast, results$forecast)
  }
  
  # Calculate the accuracy measures for the model
  accuracy_metrics <- accuracy(test_forecast$forecast, test_forecast$test)
  
  # Print the accuracy measures
  cat("Site ID:", site_id, "\n")
  cat("RMSE:", accuracy_metrics[2], "\n")
  cat("MAE:", accuracy_metrics[3], "\n\n")
  accuracies_exp = append(accuracies_exp, accuracy_metrics[2])
}

# Calculate and print the mean RMSE across all sites
cat("Mean RMSE:", mean(unlist(accuracies_exp)), "\n")

# Visualize the distribution of RMSE values
hist(unlist(accuracies_exp))
boxplot(unlist(accuracies_exp)


