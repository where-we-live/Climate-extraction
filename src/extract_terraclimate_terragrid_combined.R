extract_terraclimate_terragrid_combined <- function(variables, start_year = 1979, counties_shapefile = "./geodata/UScounties_conus.shp") {
  library(ncdf4)
  library(terra)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(tidyr)
  
  counties_sf <- st_read(counties_shapefile, quiet = TRUE) %>%
    st_transform(4326)
  
  results_list <- list()
  
  for (var in variables) {
    message("Processing variable: ", var)
    
    lat.range <- c(24.396308, 49.384358)
    lon.range <- c(-124.848974, -66.93457)
    
    baseurlagg <- paste0(
      "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
      var, "_1958_CurrentYear_GLOBE.nc"
    )
    
    nc <- nc_open(baseurlagg)
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    lat.index <- which(lat >= min(lat.range) & lat <= max(lat.range))
    lon.index <- which(lon >= min(lon.range) & lon <= max(lon.range))
    
    start_month_index <- (start_year - 1958) * 12 + 1
    start <- c(lon.index[1], lat.index[1], start_month_index)
    count <- c(length(lon.index), length(lat.index), -1)
    
    data <- ncvar_get(nc, varid = var, start = start, count = count)
    nc_close(nc)
    
    lon_sub <- lon[lon.index]
    lat_sub <- lat[lat.index]
    
    if (lat_sub[1] > lat_sub[length(lat_sub)]) {
      lat_sub <- rev(lat_sub)
      data <- data[, rev(seq_len(dim(data)[2])), ]
    }
    
    nlon <- length(lon_sub)
    nlat <- length(lat_sub)
    ntime <- dim(data)[3]
    
    ext_conus <- ext(min(lon.range), max(lon.range), min(lat.range), max(lat.range))
    r <- rast(ncols = nlon, nrows = nlat, nlyrs = ntime, extent = ext_conus, crs = "EPSG:4326")
    values(r) <- as.vector(aperm(data[, rev(seq_len(dim(data)[2])), ], c(1, 2, 3)))
    
    county_weighted <- exact_extract(r, counties_sf, fun = function(values, coverage_fraction) {
      apply(values, 2, weighted.mean, w = coverage_fraction, na.rm = TRUE)
    }, progress = TRUE)
    
    county_weighted_df <- as.data.frame(t(county_weighted))
    county_weighted_df$FIPS <- counties_sf$FIPS
    names(county_weighted_df)[1:ntime] <- paste0("lyr", 1:ntime)
    
    county_long <- county_weighted_df %>%
      pivot_longer(
        cols = starts_with("lyr"),
        names_to = "layer",
        values_to = var
      ) %>%
      mutate(
        layer = as.integer(gsub("[^0-9]", "", layer)),
        year = start_year + (layer - 1) %/% 12,
        month = (layer - 1) %% 12 + 1
      ) %>%
      select(FIPS, year, month, all_of(var)) %>%
      arrange(FIPS, year, month)
    
    write.csv(county_long, paste0("./data/area_data_1979_present/", var, ".csv"), row.names = FALSE)
    results_list[[var]] <- county_long
  }
  
  final_df <- results_list[[1]]
  if (length(results_list) > 1) {
    for (i in 2:length(results_list)) {
      final_df <- left_join(final_df, results_list[[i]], by = c("FIPS", "year", "month"))
    }
  }
  
  return(final_df)
} 


# Define the variables you want to extract
climate_vars <- c("aet", "def", "PDSI", "pet", "ppt", "q", "soil", 
                  "srad", "swe", "tmax", "tmin", "vap", "vpd", "ws")

# Call the function
climate_data <- extract_terraclimate_terragrid_combined(climate_vars)
