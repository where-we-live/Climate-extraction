

#extract_
#location can be:
#PNW
#ID
#WA
#OR
#NID (northern Idaho)

#var_type can be:
#tmmx - maximum daily temperature
#tmmn - minimum daily temperature
#pr - precipitation amount
#vs - wind speed

#EXAMPLE: extract_climate_area_data("tmmx", "PNW")


extract_climate_area_data <- function(var_type, location, year) {
  
  if(location=="PNW") {
    locationrange <- c(-124.8, -116, 41.7, 49)
    #locationrange <- c(-123, -122, 42, 43)
      } else if(location=="NID") {
    locationrange <- c(-117.2, -114.0, 45.5,49.0)
  }
    
  library(ncdf4)
  library(dplyr)
  library(lubridate)
  
  #nc_url <- paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", var_type, "_1979_CurrentYear_CONUS.nc#fillmismatch", sep="")
  #nc_url <- paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/", var_type, "/", var_type, "_", year, ".nc#fillmismatch", sep="")
  nc_url <- paste("http://tds-proxy.nkn.uidaho.edu/thredds/dodsC/agg_terraclimate_", var_type, "1958_CurrentYear_GLOBE", ".nc#fillmismatch", sep="")
  
  
  # Open the netCDF file
  nc_data <- nc_open(nc_url)
  var_name <-  nc_data$var[[1]]$name
  
  
  # Extract variables
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  time <- ncvar_get(nc_data, "day")
  
  # Convert time to date format
  dates <- as.Date(time, origin = "1900-01-01")
  
  # Find indices for the bounding box
  lon_ind <- which(lon >= locationrange[1] & lon <= locationrange[2])
  lat_ind <- which(lat >= locationrange[3] & lat <= locationrange[4])
  
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
  df <- temp_full_df
  
  library(dplyr)
  library(lubridate)
  
  # Example: Summarize daily data to monthly average
  monthly_summary <- df %>%
    mutate(year = year(date),
           month = month(date)) %>%
    group_by(lat, lon, year, month) %>%
    summarize(monthly_mean = mean(temperature, na.rm = TRUE), .groups = "drop")
  
  monthly_summary2 <- as.data.frame(monthly_summary)
  
  # Close netCDF file
  nc_close(nc_data)
  
  
  write.csv(temp_full_df, paste("./data/area_data/", var_name, "_", location, ".csv", sep=""), row.names=FALSE)
  write.csv(monthly_summary2, paste("./data/area_data/", var_name, "_", location, "_summary.csv", sep=""), row.names=FALSE)
  
  

  library(sf)
  library(dplyr)
  library(exactextractr)
  
  # Step 1: Convert your centroids to polygons (4km x 4km ~ 0.036° x 0.036°)
  deg_per_km <- 1 / 111.32  # approximate for latitude
  half_cell_deg <- (4 / 2) * deg_per_km  # half-width in degrees
  
  df_cells <- monthly_summary2 %>%
    rowwise() %>%
    mutate(geometry = st_sfc(st_polygon(list(matrix(c(
      lon - half_cell_deg, lat - half_cell_deg,
      lon + half_cell_deg, lat - half_cell_deg,
      lon + half_cell_deg, lat + half_cell_deg,
      lon - half_cell_deg, lat + half_cell_deg,
      lon - half_cell_deg, lat - half_cell_deg
    ), ncol = 2, byrow = TRUE))), crs = 4326)) %>%
    ungroup()
  
  # Convert to sf object
  cell_polygons <- st_as_sf(df_cells)
  
  # Step 2: Load county boundaries
  counties <- st_read("./geodata/UScounties_conus.shp") %>%
    st_transform(4326)
  
  # Step 3: Use exactextractr’s vector-vector mode for fast overlap weighting
  # This returns weighted mean per county based on overlapping area
  
  intersections <- st_intersection(cell_polygons, counties)
  
  # Compute area of each intersection
  intersections <- intersections %>%
    mutate(
      intersection_area = st_area(geometry),
      cell_area = st_area(cell_polygons)[match(cell_id, cell_polygons$cell_id)],
      coverage_fraction = as.numeric(intersection_area / cell_area)
    )
  
  #now multiply coverage_fraction by climate value
  
  intersections <- intersections %>%
    left_join(climate_data, by = "cell_id") %>%
    group_by(county_id) %>%
    summarize(weighted_temp = sum(temp * coverage_fraction, na.rm = TRUE))
  
  
  
  
  
  
  
  
  results <- exactextractr::coverage_fraction(cell_polygons, counties)
  
  # Add value column to coverage table
  results_with_values <- mapply(function(frac, val) {
    frac$value <- val[frac$coverage_cell]
    frac
  }, results, split(cell_polygons$value, seq_len(nrow(cell_polygons))), SIMPLIFY = FALSE)
  
  # Step 4: Compute weighted means
  county_means <- lapply(results_with_values, function(tbl) {
    sum(tbl$value * tbl$coverage_fraction, na.rm = TRUE) / sum(tbl$coverage_fraction, na.rm = TRUE)
  })
  
  # Step 5: Combine results with county metadata
  county_avg_df <- data.frame(
    GEOID = counties$GEOID,
    NAME = counties$NAME,
    mean_value = unlist(county_means)
  )
  
  # Done!
  head(county_avg_df)
  
  
  
  
  
  
  
  
}