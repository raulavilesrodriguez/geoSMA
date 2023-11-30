library(terra)
library(sp)

# Define the projection information for Zone 17S in Ecuador
utm_zone <- "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Example projected coordinates (replace these with your actual coordinates)
x <- c(711824, 879519.34, 781994)
y <- c(10036426.68, 10071422.19, 9970503)

# Combine the coordinates into a matrix
xy <- cbind(x, y)

# Create a SpatVector object with the projected coordinates
pts <- vect(xy, crs = utm_zone)

# Transform the projected coordinates to longitude and latitude
pts_ll <- project(pts, "+proj=longlat +datum=WGS84")

# Display the transformed coordinates (longitude and latitude)
longlat <- geom(pts_ll)[, c("x", "y")]
longlat
