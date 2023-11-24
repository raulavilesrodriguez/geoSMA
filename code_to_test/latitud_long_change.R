# Latitude in degrees, minutes, and seconds
latitude <- "00°17'09.40''N"

# Split the string by °, ', and '' to extract degrees, minutes, and seconds
parts <- unlist(strsplit(latitude, "[°'']"))

# Extract degrees, minutes, and seconds
degrees <- as.numeric(parts[1])
minutes <- as.numeric(parts[2])
seconds <- as.numeric(parts[3])

# Convert to decimal degrees
decimal_degrees <- degrees + minutes / 60 + seconds / 3600

# Print the result
print(decimal_degrees)

# Convert the latitude to a decimal number
# Extract the degrees, minutes, and seconds from the latitude value
lat_degrees <- as.numeric(strsplit(latitude, "°")[[1]][1])
lat_minutes <- as.numeric(strsplit(strsplit(latitude, "°")[[1]][2], "'")[[1]][1])
lat_seconds <- as.numeric(strsplit(strsplit(latitude, "'")[[1]][2], "''")[[1]][1])

# Convert the latitude value to decimal degrees
lat_decimal <- lat_degrees + lat_minutes / 60 + lat_seconds / 3600

# Print the result
cat("Latitude in decimal degrees:", lat_decimal)
