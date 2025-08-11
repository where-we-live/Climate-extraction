library(ncdf4)
library(dplyr)
library(lubridate)

# Open the netCDF file
nsat <- nc_open("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc")

# Extract variables
lon <- ncvar_get(nsat, "lon")
lat <- ncvar_get(nsat, "lat")
time <- ncvar_get(nsat, "day") 

# Convert time to date format
dates <- as.Date(time, origin = "1900-01-01")

# Define the longitude and latitude range for the subset
lon_rng <- c(-117.04, -116.33)
lat_rng <- c(46.54, 47.13)

# Find indices for the bounding box
lon_ind <- which(lon >= lon_rng[1] & lon <= lon_rng[2])
lat_ind <- which(lat >= lat_rng[1] & lat <= lat_rng[2])

# Extract temperature data for the defined range
temp_data <- ncvar_get(nsat, "daily_minimum_temperature", 
                       start = c(min(lon_ind), min(lat_ind), 1), 
                       count = c(length(lon_ind), length(lat_ind), length(time)))

# Convert data to a dataframe
data_list <- list()
for (i in seq_along(lon_ind)) {
  for (j in seq_along(lat_ind)) {
    temp_df <- data.frame(
      lon = lon[lon_ind[i]], 
      lat = lat[lat_ind[j]], 
      date = dates,
      daily_minimum_temperature = temp_data[i, j, ]
    )
    data_list <- append(data_list, list(temp_df))
  }
}

# Combine all extracted data into a single data frame
nsat_full_df <- do.call(rbind, data_list)

View(nsat_full_df)



nc_close(nsat)



# Extract unique latitude and longitude pairs
unique_coords <- unique(nsat_full_df[, c("lon", "lat")])

# View the unique coordinates
print(unique_coords)

View(unique_coords)