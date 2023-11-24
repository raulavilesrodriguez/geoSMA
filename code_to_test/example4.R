library(tidyverse)
library(leaflet)
library(scales)

# Example data with latitude, longitude, and diameter of coverage
data1 <- data.frame(
  latitude = c(-2.1900, -0.2200, -2.8974), # Example latitudes
  longitude = c(-79.8875, -78.5125, -79.0045), # Example longitudes
  diameter = c(0.5, 0.5, 0.5) # Example diameters for coverage in kilometers
)

# 700 MHZ rbs
data2 <- data.frame(
  latitude = c(-2.1800, -0.200, -2.8800), # Example latitudes
  longitude = c(-79.800, -78.4900, -79.005), # Example longitudes
  diameter = c(3, 3, 3) # Example diameters for coverage in kilometers
)

# centers of security
data3 <- data.frame(
  latitude = c(-2.1700, -0.2100, -2.8500), # Example latitudes
  longitude = c(-79.900, -78.4200, -79.003), # Example longitudes
  diameter = c(3, 3, 3) # Example diameters for coverage in kilometers
)

map <- leaflet() %>% addTiles()

# Create a leaflet map
map <- map %>%
  addCircles(
    data = data1,
    lng = ~longitude,
    lat = ~latitude,
    radius = ~diameter * 1000, # Convert diameter from km to meters
    color = "red",
    fillOpacity = 0.5,
    group = "Data 1",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = data2,
    lng = ~longitude,
    lat = ~latitude,
    radius = ~diameter * 1000, # Convert diameter from km to meters
    color = "blue",
    fillOpacity = 0.2,
    group = "Data 2",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircleMarkers(
    data = data3,
    lng = ~longitude,
    lat = ~latitude,
    radius = ~diameter * 1, # Convert diameter from km to meters
    color = "green",
    fillOpacity = 0.2,
    group = "Data 3"
  )

# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("Data 1", "Data 2", "Data 3"),
  options = layersControlOptions(collapsed = FALSE)
)

# Display the map
#map
