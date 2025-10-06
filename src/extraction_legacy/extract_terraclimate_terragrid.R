
#aet,def,PDSI,pet,ppt,q,soil,srad,swe,tmax,tmin,vap,vpd,ws

extract_terraclimate_terragrid <- function(var) {

# --- Load Packages ---
library(ncdf4)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)

# --- Set Parameters ---
lat.range <- c(24.396308, 49.384358)   # South to North
lon.range <- c(-124.848974, -66.93457) # West to East
#var <- "aet"                           # Variable name
start_year <- 1994

# --- Open TerraClimate NetCDF ---
baseurlagg <- paste0(
  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
  var, "_1958_CurrentYear_GLOBE.nc"
)

nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

# --- Subset lat/lon ranges ---
lat.range <- sort(lat.range)
lon.range <- sort(lon.range)
lat.index <- which(lat >= lat.range[1] & lat <= lat.range[2])
lon.index <- which(lon >= lon.range[1] & lon <= lon.range[2])

# For 1994 onward
start_month_index <- (1994 - 1958) * 12 + 1  # = 433
start <- c(lon.index[1], lat.index[1], start_month_index)
count <- c(length(lon.index), length(lat.index), -1)


# --- Extract climate data ---
data <- ncvar_get(nc, varid = var, start = start, count = count)
nc_close(nc)

lon_sub <- lon[lon.index]
lat_sub <- lat[lat.index]

# --- Create terra SpatRaster ---
ext_conus <- ext(-124.848974, -66.93457, 24.396308, 49.384358)

# Fix lat orientation if needed
if (lat_sub[1] > lat_sub[length(lat_sub)]) {
  lat_sub <- rev(lat_sub)
  data <- data[, rev(seq_len(dim(data)[2])), ]
}

nlon <- length(lon_sub)
nlat <- length(lat_sub)
ntime <- dim(data)[3]
#time_layers <- dim(data)[3]


r <- rast(
  ncols = nlon,
  nrows = nlat,
  nlyrs = ntime,         # <--- HERE
  extent = ext_conus,
  crs = "EPSG:4326"
)

#values(r) <- as.vector(aperm(data, c(2, 1, 3)))
values(r) <- as.vector(aperm(data, c(1, 2, 3)))

# --- Load Counties ---
counties_sf <- st_read("./geodata/UScounties_conus.shp") %>%
  st_transform(4326)

# Correct weighted extraction
county_weighted <- exact_extract(r, counties_sf, fun = function(values, coverage_fraction) {
  apply(values, 2, weighted.mean, w = coverage_fraction, na.rm = TRUE)
}, progress = TRUE)



# Step 1: Transpose matrix so each row = county
county_weighted_df <- as.data.frame(t(county_weighted))  # Now 3109 rows × 372 columns

# Step 1.5: Identify counties with all NA values across all months
na_counties <- county_weighted_df %>%
  filter(rowSums(is.na(select(., starts_with("lyr")))) == time_layers)

cat("Number of counties with no data:", nrow(na_counties), "
")

# Optional: print their FIPS codes
if (nrow(na_counties) > 0) print(na_counties$FIPS)

# Step 2: Attach FIPS
county_weighted_df$FIPS <- counties_sf$FIPS

# Step 3: Rename month columns
names(county_weighted_df)[1:372] <- paste0("lyr", 1:372)

# Step 4: Reshape to long format
county_long <- county_weighted_df %>%
  pivot_longer(
    cols = starts_with("lyr"),
    names_to = "layer",
    values_to = "mean_value"
  ) %>%
  mutate(
    layer = as.integer(gsub("[^0-9]", "", layer)),  # e.g., "lyr241" → 241
    year = start_year + (layer - 1) %/% 12,
    month = (layer - 1) %% 12 + 1
  ) %>%
  select(FIPS, year, month, mean_value) %>%
  arrange(FIPS, year, month)

write.csv(county_long, paste("./data/area_data/", var, ".csv", sep=""), row.names=FALSE)
return(county_long)
}

