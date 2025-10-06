library(sf)
library(dplyr)
library(tigris)

lat.range = c(24.396308, 49.384358)
lon.range = c(-124.848974, -66.93457)

# 1.1 Load U.S. counties
counties_sf <- counties(cb = TRUE) %>%
  st_transform(4326)

# 1.2 Create a grid covering the US
create_grid_sf <- function(lon.range, lat.range, cellsize_km = 4) {
  
  # Convert to approximate degrees (very rough)
  cellsize_deg <- cellsize_km / 111
  
  # Create grid points
  lon_seq <- seq(lon.range[1], lon.range[2], by = cellsize_deg)
  lat_seq <- seq(lat.range[1], lat.range[2], by = cellsize_deg)
  
  grid_df <- expand.grid(lon = lon_seq, lat = lat_seq)
  
  # Build 4km square polygons around each point
  grid_sf <- grid_df %>%
    rowwise() %>%
    mutate(geometry = list(st_polygon(list(matrix(c(
      lon - cellsize_deg/2, lat - cellsize_deg/2,
      lon + cellsize_deg/2, lat - cellsize_deg/2,
      lon + cellsize_deg/2, lat + cellsize_deg/2,
      lon - cellsize_deg/2, lat + cellsize_deg/2,
      lon - cellsize_deg/2, lat - cellsize_deg/2
    ), ncol = 2, byrow = TRUE))))) %>%
    ungroup()
  
  st_as_sf(grid_sf, crs = 4326)
}

# 1.3 Create the grid for CONUS
grid_sf <- create_grid_sf(
  lon.range = lon.range,
  lat.range = lat.range
)

# 1.4 Assign each grid cell an ID
grid_sf$grid_id <- 1:nrow(grid_sf)

# 1.5 Intersect grid cells with counties
intersections <- st_intersection(
  st_make_valid(grid_sf),
  st_make_valid(counties_sf)
)

# 1.6 Calculate fraction of each grid cell that lies in each county
intersections <- intersections %>%
  mutate(
    intersection_area = st_area(geometry),
    full_area = units::set_units((4 * 1000)^2, m^2),  # 4km × 4km = 16 million m²
    overlap_fraction = as.numeric(intersection_area / full_area)
  )

# 1.7 Select clean columns
overlap_table <- intersections %>%
  st_set_geometry(NULL) %>%
  select(grid_id, GEOID, NAME, overlap_fraction)

# 1.8 Save if you want
# write.csv(overlap_table, "grid_county_overlap_table.csv", row.names = FALSE)

# ✅ At this point you have a clean overlap table: grid_id ➔ county_id + overlap_fraction
