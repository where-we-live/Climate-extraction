extract_climate_data_county_aggretation <- function() {
  
  
  library(sf)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(exactextractr)
  
  
  df <- read.csv("./data/area_data/air_temperature_PNW.csv")
  
  library(dplyr)
  library(lubridate)
  
  # Example: Summarize daily data to monthly average
  monthly_summary <- df %>%
    mutate(year = year(date),
           month = month(date)) %>%
    group_by(lat, lon, year, month) %>%
    summarize(monthly_mean = mean(temperature, na.rm = TRUE), .groups = "drop")
  
  monthly_summary2 <- as.data.frame(monthly_summary)
  
  #prep for convert to raster 
  
  compute_cell_edges <- function(lat, lon, cell_km = 4) {
    dlat <- (cell_km / 2) / 111.32
    dlon <- (cell_km / 2) / (111.32 * cos(lat * pi / 180))
    
    return(data.frame(
      lon_min = lon - dlon,
      lon_max = lon + dlon,
      lat_min = lat - dlat,
      lat_max = lat + dlat
    ))
  }
  
  # Apply function to all rows
  cell_bounds <- mapply(compute_cell_edges, monthly_summary2$lat, monthly_summary2$lon, SIMPLIFY = FALSE)
  cell_bounds_df <- do.call(rbind, cell_bounds)
  
  # Combine original centroids with computed bounds
  df_with_bounds <- cbind(monthly_summary2, cell_bounds_df)
  df <- df_with_bounds
  
  library(data.table)
  library(terra)
  
  # Loop to create polygons for each row
  polys <- lapply(1:nrow(df), function(i) {
    e <- ext(df$lon_min[i], df$lon_max[i], df$lat_min[i], df$lat_max[i])
    p <- as.polygons(e)
    values(p) <- data.frame(value = df$monthly_mean[i])
    return(p)
  })
  
  # Combine all into a single SpatVector
  polys <- do.call(rbind, polys)
  crs(polys) <- "EPSG:4326"  # assign CRS
  
  
  

}
