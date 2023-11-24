library(tidyverse)
library(leaflet)
library(scales)

# Example data with latitude, longitude, and diameter of coverage
data <- data.frame(
  latitude = c(-2.1900, -0.2200, -2.8974), # Example latitudes
  longitude = c(-79.8875, -78.5125, -79.0045), # Example longitudes
  diameter = c(2, 2, 2) # Example diameters for coverage in kilometers
)

# centeres of security
data2 <- data.frame(
  latitude = c(-2.1800, -0.200, -2.700), # Example latitudes
  longitude = c(-79.800, -78.4900, -78.0001), # Example longitudes
  diameter = c(2, 2, 2) # Example diameters for coverage in kilometers
)

# Create a leaflet map
map <- leaflet(data) %>%
  addTiles() %>%
  addCircles(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~diameter * 1000, # Convert diameter from km to meters
    color = "red",
    fillOpacity = 0.2
  )

# Display the map
map
