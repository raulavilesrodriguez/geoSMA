library(leaflet)
library(dplyr)

# Example data with latitude, longitude, and diameter of coverage
data <- data.frame(
  latitude = c(-2.1900, -0.2200, -2.8974), # Example latitudes
  longitude = c(-79.8875, -78.5125, -79.0045), # Example longitudes
  diameter = c(100, 150, 200) # Example diameters for coverage in kilometers
)

# Create a leaflet map
m <- leaflet(data = data) %>%
  addTiles() %>%
  setView(lng = mean(data$longitude), lat = mean(data$latitude), zoom = 3)

# Add circle markers representing coverage areas
for (i in 1:nrow(data)) {
  radius_in_meters <- data$diameter[i] * 1000
  
  m <- m %>%
    addCircleMarkers(lng = data$longitude[i], lat = data$latitude[i], radius = 5, color = "blue", fillColor = "blue") %>%
    addCircleMarkers(lng = data$longitude[i], lat = data$latitude[i], radius = radius_in_meters, color = "red", fill = FALSE) # Multiply by 1000 for km to meters
}

m  # Show the map


