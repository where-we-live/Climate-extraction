extract_climaate_data_county_aggretation <- function() {
  
  
  library(sf)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(exactextractr)
  
  df <- monthly_summary2
  #df <- read.csv("./data/area_data/air_temperature_PNW.csv")
  # Step 1: Convert your temperature data to an sf object
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Function to convert 4km x 4km centroids to bounding box edges
  compute_cell_edges <- function(lat, lon, cell_km = 4) {
    # Degrees per km
    dlat <- (cell_km / 2) / 111.32
    dlon <- (cell_km / 2) / (111.32 * cos(lat * pi / 180))
    
    return(data.frame(
      lon_min = lon - dlon,
      lon_max = lon + dlon,
      lat_min = lat - dlat,
      lat_max = lat + dlat
    ))
  }
  
  # Apply to your data
  cell_bounds <- mapply(compute_cell_edges, df$lat, df$lon, SIMPLIFY = FALSE)
  cell_bounds_df <- do.call(rbind, cell_bounds)
  
  # Combine with original centroids
  df_with_bounds <- cbind(df, cell_bounds_df)
  
  
  #----raster weighted averaging
  
  library(sf)
  library(raster)
  library(dplyr)
  library(terra)
  
  # Example input
  # df <- data.frame(lon_min = ..., lon_max = ..., lat_min = ..., lat_max = ..., value = ...)
  
  # Step 1: Create polygons from bounding box coordinates
  df_sf <- df_with_bounds %>%
    rowwise() %>%
    mutate(geometry = st_sfc(st_polygon(list(matrix(
      c(lon_min, lat_min,
        lon_max, lat_min,
        lon_max, lat_max,
        lon_min, lat_max,
        lon_min, lat_min), 
      ncol = 2, byrow = TRUE))),
      crs = 4326)) %>%
    ungroup()
  
  # Convert to sf object
  grid_sf <- st_as_sf(df_sf)
  
  # Step 2: Create an empty raster template
  # Use extent from data
  r_template <- raster(extent(st_bbox(grid_sf)), 
                       resolution = c(mean(df_with_bounds$lon_max - df_with_bounds$lon_min), 
                                      mean(df_with_bounds$lat_max - df_with_bounds$lat_min)), 
                       crs = "+proj=longlat +datum=WGS84")
  
  # Ensure 'value' column exists, is numeric, and has no NA
  # grid_sf <- grid_sf %>%
  #   filter(!is.na(value)) %>%     # remove rows with NA
  #   mutate(value = as.numeric(value))  # ensure numeric
  # 
  
  # Step 3: Rasterize the value column
  r <- terra::rasterize(grid_sf, r_template, field = "monthly_mean", fun = mean)
  
  

  # Plot it
  plot(r, main = "Rasterized Grid Cell Values")
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Step 2: Load counties shapefile (U.S. Census or other)
  # Download once: https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_us_county_500k.zip
  counties <- st_read("./geodata/UScounties_conus.shp") %>%
    st_transform(4326)
  
  # Step 3: Spatial join: assign each point to a county
  df_with_county <- st_join(df_sf, counties %>% select(FIPS))
  
  # Step 4: Extract month and year
  df_with_county <- df_with_county %>%
    mutate(year = year(date),
           month = month(date))
  
  # Step 5: Group by county, year, and month to compute average
  monthly_avg <- df_with_county %>%
    st_drop_geometry() %>%
    group_by(GEOID, NAME, STATEFP, year, month) %>%
    summarize(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")
  
  # View result
  head(monthly_avg)
  
}