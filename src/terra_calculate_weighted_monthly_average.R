library(sf)
library(dplyr)
library(units)

terra_calculate_weighted_monthly_average <- function(climate_df, varname, counties_sf) {
  
  # Step 1: Create small square polygons (4km x 4km) around each lon/lat
  cell_halfsize_deg <- 2 / 111  # ~2 km in degrees (1 degree ~111 km at equator)
  
  climate_cells <- climate_df %>%
    mutate(
      lon_min = lon - cell_halfsize_deg,
      lon_max = lon + cell_halfsize_deg,
      lat_min = lat - cell_halfsize_deg,
      lat_max = lat + cell_halfsize_deg
    ) %>%
    rowwise() %>%
    mutate(geometry = list(
      st_polygon(list(matrix(c(
        lon_min, lat_min,
        lon_max, lat_min,
        lon_max, lat_max,
        lon_min, lat_max,
        lon_min, lat_min
      ), ncol = 2, byrow = TRUE)))
    )) %>%
    ungroup()
  
  climate_cells_sf <- st_as_sf(climate_cells, crs = 4326)
  
  # Step 2: Make sure counties and cells are in the same CRS
  
  counties_sf <- st_read("./geodata/UScounties_conus.shp") %>%
    st_transform(4326)
  
  
  # Step 3: Intersect pixels with counties
  intersection <- st_intersection(
    st_make_valid(counties_sf),
    st_make_valid(climate_cells_sf)
  )
  
  # Step 4: Calculate areas
  intersection <- intersection %>%
    mutate(
      intersect_area = st_area(geometry),
      total_area = (4 * 1000) * (4 * 1000) # 4km x 4km = 16,000,000 m²
    ) %>%
    mutate(
      weight = as.numeric(intersect_area) / total_area
    )
  
  # Step 5: Weighted value
  intersection <- intersection %>%
    mutate(
      weighted_value = !!sym(varname) * weight
    )
  
  # Step 6: Summarize by county, year, month
  county_monthly_avg <- intersection %>%
    group_by(GEOID, NAME, year, month) %>%
    summarize(
      weighted_avg = sum(weighted_value, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(county_monthly_avg)
}
