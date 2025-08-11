

extract_temperature <- function(var_type, lon_rng, lat_rng) {
  
  library(ncdf4)
  library(dplyr)
  library(lubridate)
  
  nc_url <- paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", var_type, "_1979_CurrentYear_CONUS.nc#fillmismatch", sep="")
  
  # Open the netCDF file
  nc_data <- nc_open(nc_url)
  var_name <- nc_data$name[1]
  
  
  # Extract variables
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  time <- ncvar_get(nc_data, "day")
  
  # Convert time to date format
  dates <- as.Date(time, origin = "1900-01-01")
  
  # Find indices for the bounding box
  lon_ind <- which(lon >= lon_rng[1] & lon <= lon_rng[2])
  lat_ind <- which(lat >= lat_rng[1] & lat <= lat_rng[2])
  
  # Extract temperature data for the defined range
  temp_data <- ncvar_get(nc_data, var_name, 
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
        temperature = temp_data[i, j, ] 
      )
      data_list <- append(data_list, list(temp_df))
    }
  }
  
  # Combine all extracted data into a single data frame
  temp_full_df <- do.call(rbind, data_list)
  
  # Close netCDF file
  nc_close(nc_data)
  
  return(temp_full_df)
}