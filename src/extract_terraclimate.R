extract_terraclimate <- function(var) {
  
lat.range = c(24.396308, 49.384358)
lon.range = c(-124.848974, -66.93457)


# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
#var="aet"

#install.packages("ncdf4")
library(ncdf4)

baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

#------

nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

lat.range <- sort(lat.range)                              #!sort user input values from low to high
lon.range <-sort(lon.range)
lat.index <- which(lat>=lat.range[1]&lat<=lat.range[2])    #! index values within specified range
lon.index <- which(lon>=lon.range[1]&lon<=lon.range[2])    
lat.n <- length(lat.index)                                #!value for count
lon.n <- length(lon.index)
start <- c(lon.index[1], lat.index[1], 1)
count <- c(lon.n, lat.n, -1)                            #! parameter change: 'NA' instead of '-1' to signify entire dimension



data <-ncvar_get(nc, varid = var,start = start, count)    #! argument change: 'variable' instead of 'varid'  # Output is now a matrix


library(reshape2)
library(dplyr)

# Assume you already have:
# - data     (3D array [lon, lat, time])
# - lon_sub  (vector of longitudes after subsetting)
# - lat_sub  (vector of latitudes after subsetting)

# Step 1: Flatten the 3D array
data_melt <- melt(data)
names(data_melt) <- c("lon_idx", "lat_idx", "time_idx")

lon_sub <- lon[lon.index]
lat_sub <- lat[lat.index]

# Step 2: Match the index numbers back to real lon/lat coordinates
data_melt$lon <- lon_sub[data_melt$lon_idx]
data_melt$lat <- lat_sub[data_melt$lat_idx]

# Step 3: Create year and month columns based on the time index
start_year <- 1994
data_melt$year <- start_year + (data_melt$time_idx - 1) %/% 12
data_melt$month <- (data_melt$time_idx - 1) %% 12 + 1

colnames(data_melt)[4] <- var

# Step 4: Clean up and arrange into final format
climate_df <- data_melt %>%
  select(lon, lat, year, month, all_of(var))

#write.csv(climate_df, paste("./data/area_data/", var, ".csv", sep=""), row.names=FALSE)
return(climate_df)
}
