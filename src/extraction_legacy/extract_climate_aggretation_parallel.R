library(sf)
library(data.table)
library(future.apply)

cell_polygons <- readRDS("./data/area_data/cell_polygons.rds")
counties <- st_read("./geodata/UScounties_conus.shp")



# Set up parallel backend (use available cores)
future::plan(multisession, workers = parallel::detectCores() - 1)

# Reproject for area accuracy
cell_polygons <- st_transform(cell_polygons, 5070)
counties <- st_transform(counties, 5070)

# Assign IDs if missing
cell_polygons$cell_id <- seq_len(nrow(cell_polygons))
counties$county_id <- seq_len(nrow(counties))

# Create bounding box pre-filter (speeds up intersection massively)
county_bounds <- st_bbox(counties)
#cell_polygons <- cell_polygons[st_intersects(cell_polygons, counties, sparse = FALSE), ]
cell_polygons <- cell_polygons[lengths(st_intersects(cell_polygons, counties)) > 0, ]

# Convert to list of cell geometries to parallelize
cell_list <- split(cell_polygons, cell_polygons$cell_id)

# Function to intersect one grid cell with all counties
process_cell <- function(cell_sf) {
  suppressWarnings({
    overlaps <- st_intersection(cell_sf, counties)
    if (nrow(overlaps) == 0) return(NULL)
    
    # Compute coverage fraction
    overlaps$intersect_area <- st_area(overlaps)
    overlaps$cell_area <- st_area(cell_sf)[1]
    overlaps$coverage_fraction <- as.numeric(overlaps$intersect_area / overlaps$cell_area)
    
    return(overlaps[, c("cell_id", "county_id", "coverage_fraction")])
  })
}

# Parallel intersection
overlap_list <- future_lapply(cell_list, process_cell)

# Combine into one data.table
overlap_dt <- rbindlist(overlap_list, use.names = TRUE, fill = TRUE)
