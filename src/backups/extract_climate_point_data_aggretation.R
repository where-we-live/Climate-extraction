source("./src/extract_climate_point_data.R")
source("./src/extract_climate_aggretation_point.R")
# Define the variables you want to extract
variables <- c("tmmx", "tmmn", "rmin", "rmax", "sph", "srad", "th", "vs", "bi", "fm100", "fm1000", "erc", "pdsi", "vpd")


for (var_type in variables) {
  message("Processing variable: ", var_type)
  
extract_climate_point_data(var_type)

}

for (var_type in variables) {
  message("Processing variable: ", var_type)
  
  extract_climate_aggretation_point(var_type)
  
}