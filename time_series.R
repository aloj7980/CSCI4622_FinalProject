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
  print(site_id)
  # Subset the data for the current site
  site_data <- data %>% filter(site_id == site_id) %>% select(date, snow_water_equivalent)
  
  site_data

  # Run the ADF test to check for stationarity
  adf_result <- adf.test(site_data$snow_water_equivalent)
  # If data is not stationary, stationarize it using differencing
  if (adf_result$p.value > 0.05) {
    # Perform differencing to stationarize the data
    site_data$snow_water_equivalent_diff <- diff(site_data$snow_water_equivalent, differences = 1)
    site_data <- site_data[-1,] # remove first row since differencing causes NAs
    
    # Update the data object with the stationarized data
    data <- data %>% filter(site_id != site_id) %>% bind_rows(site_data)
    print("s")
  }
}

# View the updated data object
data
