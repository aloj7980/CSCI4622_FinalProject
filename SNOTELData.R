
# Load the necessary packages
library(dplyr)
library(snotelr)

# Get metadata for all SNOTEL stations
site_meta_data <- snotel_info()

# Filter the metadata to include only stations with data from 1/1/2013 to 12/31/2022 and with the measurement description "SNOW WATER EQUIVALENT"
data_start_date <- "2013-01-01"
data_end_date <- "2022-12-31"
filtered_meta_data <- site_meta_data %>%
  filter(data_start_date >= start & end >= data_end_date)

# Create a vector of station IDs from the filtered metadata
station_ids <- filtered_meta_data$site_id
station_ids <- station_ids[-grep("1315", station_ids)]

# Create an empty data frame to store the snow water equivalent data
swe_data <- data.frame()

# Loop over each station and fetch the snow water equivalent data
for (station_id in station_ids) {
  # Download all available data for this station
  station_data <- snotel_download(site_id = station_id, internal = TRUE)
  
  
  
  # Filter the data to include only snow water equivalent and data from the specified start and end dates
  swe <- subset(station_data, date >= data_start_date & date <= data_end_date, select = c("date", "snow_water_equivalent"))
  
  # Add a column to the data frame with the station ID
  swe$station_id <- station_id
  
  # Append the data to the overall data frame
  swe_data <- rbind(swe_data, swe)
}

# Export the data frame to a CSV file
swe_data
length(unique(swe_data$station_id))
write.csv(swe_data, file = "snotel_swe_data.csv", row.names = FALSE)
