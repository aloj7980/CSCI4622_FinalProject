# time_series.R - this script will crunch through a lengthy .csv
# 		of SNOTEL station data. For each contained station, it will run
# 		an ADF test to verify stationarity. Then, if a station is found 
# 		not to be stationary it will be made so by differencing.

library(dplyr)
library(tseries)
library(data.table)

# Read in the data
data <- fread("snotel_swe_data.csv")

# Filter out rows with NA values
na_data <- data %>% filter(is.na(snow_water_equivalent))

# Get unique station_ids
na_station_ids <- na_data %>% distinct(station_id) %>% pull(station_id)

data <- data %>% filter(!station_id %in% na_station_ids)

# Loop over each site and check stationarity of SWE data
for (site_id in unique(data$station_id)) {
  print(sprintf("Processing: %i", site_id))

  # Subset the data for the current site
  site_data <- data %>% filter(site_id == site_id) %>% select(date, snow_water_equivalent)

  # Run the ADF test to check for stationarity
  adf_result <- adf.test(site_data$snow_water_equivalent)
  
  # If data not stationary...
  if (adf_result$p.value > 0.05) {
    print(sprintf("Site %i: Data was not stationary. Adjusting.", site_id))
    
    # Perform differencing to stationarize the data
    site_data$snow_water_equivalent_diff <- diff(site_data$snow_water_equivalent, differences = 1)
    site_data <- site_data[-1,] # remove first row since differencing causes NAs
    
    # Update the data object with the stationary data
    data <- data %>% filter(site_id != site_id) %>% bind_rows(site_data)

  }
}

# Produce the data
fwrite(data, "processed_data.csv")
