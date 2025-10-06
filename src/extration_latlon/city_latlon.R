
#city_latlon extracts the lat and lon for a city and state name
#both need to have "" around, and the state needs to be a two prefix abbreviation.

city_latlon <- function(cityname, state) {

  library(tidygeocoder)
  library(dplyr)
  
  
# Define city and state
location <- tibble(
  city = cityname,
  state = state
)

# Get latitude and longitude
coordinates <- location %>%
  geocode(city = city, state = state, method = "osm", full_results = FALSE)

#print(coordinates)
return(coordinates)

}
