library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)
library(here)
library(writexl)
library(fontawesome)

db.conecel <- read_excel('./ResultGeoTOTAL/dbconecel.xlsx')
db.otecel <- read_excel('./ResultGeoTOTAL/dfotecel.xlsx')
db.cnt <- read_excel('./ResultGeoTOTAL/dfcnt.xlsx')
db.educacion <- read_excel('./establecimientos/educaciónLatLong.xlsx')


# Conecel
db.conecel2G850 <- db.conecel |> filter(TECNOLOGIA == 'GSM', banda == '850')
db.conecel2G1900 <- db.conecel |> filter(TECNOLOGIA == 'GSM', banda == '1900')
db.conecel3G850 <- db.conecel |> filter(TECNOLOGIA == 'UMTS', banda == '850')
db.conecel3G1900 <- db.conecel |> filter(TECNOLOGIA == 'UMTS', banda == '1900')
db.conecel4G850 <- db.conecel |> filter(TECNOLOGIA == 'LTE', banda == '850')
db.conecel4G1700 <- db.conecel |> filter(TECNOLOGIA == 'LTE', banda == '1700')
db.conecel4G1900 <- db.conecel |> filter(TECNOLOGIA == 'LTE', banda == '1900')

# Otecel
db.otecel2G850 <- db.otecel |> filter(TECNOLOGIA == 'GSM', banda == '850')
db.otecel2G1900 <- db.otecel |> filter(TECNOLOGIA == 'GSM', banda == '1900')
db.otecel3G850 <- db.otecel |> filter(TECNOLOGIA == 'UMTS', banda == '850')
db.otecel3G1900 <- db.otecel |> filter(TECNOLOGIA == 'UMTS', banda == '1900')
db.otecel4G850 <- db.otecel |> filter(TECNOLOGIA == 'LTE', banda == '850')
db.otecel4G1900 <- db.otecel |> filter(TECNOLOGIA == 'LTE', banda == '1900')

# CNT
db.cnt3G1900 <- db.cnt |> filter(TECNOLOGIA == 'UMTS', banda == '1900')
db.cnt4G850 <- db.cnt |> filter(TECNOLOGIA == 'LTE', banda == '700')
db.cnt4G1700 <- db.cnt |> filter(TECNOLOGIA == 'LTE', banda == '1700')
db.cnt4G1900 <- db.cnt |> filter(TECNOLOGIA == 'LTE', banda == '1900')

# Educacion
# to suppress duplicate to sector x y z
db.educacion <- db.educacion |> group_by(NOM_INSTIT) |>
  distinct()

db.educacion1 <- db.educacion |> filter(Cobertura == 'Alta')
db.educacion2 <- db.educacion |> filter(Cobertura == 'Media')
db.educacion3 <- db.educacion |> filter(Cobertura == 'Baja')
db.educacion4 <- db.educacion |> filter(Cobertura == 'Sin cobertura')



#----Ploting-----
saludIcon <- makeIcon(
  iconUrl = "location1.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
)

greenLeafIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)


# Create a leaflet map
map <- leaflet() %>% addTiles()

#--Conecel--
map <- map %>%
  addCircles(
    data = db.conecel2G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#F8DE22",
    fillOpacity = 0.3,
    group = "Conecel2G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel3G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "blue",
    fillOpacity = 0.2,
    group = "Conecel3G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel3G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FB2576",
    fillOpacity = 0.2,
    group = "Conecel3G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel4G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#4F200D",
    fillOpacity = 0.3,
    group = "Conecel4G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel4G1700,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "red",
    fillOpacity = 0.2,
    group = "Conecel4G1700",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel4G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FF1700",
    fillOpacity = 0.2,
    group = "Conecel4G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

#--Otecel--
map <- map %>%
  addCircles(
    data = db.otecel2G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#3876BF",
    fillOpacity = 0.2,
    group = "Otecel2G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.otecel2G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#113946",
    fillOpacity = 0.5,
    group = "Otecel2G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.otecel3G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FF0060",
    fillOpacity = 0.2,
    group = "Otecel3G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.otecel3G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#00FFAB",
    fillOpacity = 0.3,
    group = "Otecel3G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.otecel4G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#005B41",
    fillOpacity = 0.2,
    group = "Otecel4G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.otecel4G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#3F1D38",
    fillOpacity = 0.2,
    group = "Otecel4G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

#---CNT---
map <- map %>%
  addCircles(
    data = db.cnt3G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#190482",
    fillOpacity = 0.2,
    group = "Cnt3G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.cnt4G850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#427D9D",
    fillOpacity = 0.2,
    group = "Cnt4G850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.cnt4G1700,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#F806CC",
    fillOpacity = 0.3,
    group = "Cnt4G1700",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.cnt4G1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FA1E0E",
    fillOpacity = 0.3,
    group = "Cnt4G1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addMarkers(
    data = db.educacion4,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    label = ~NOM_INSTIT,
    group = "Educación"
  )


# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("Conecel2G850", 
                    "Conecel3G850", 
                    "Conecel3G1900",
                    "Conecel4G850",
                    "Conecel4G1700",
                    "Conecel4G1900",
                    "Otecel2G850",
                    "Otecel2G1900",
                    "Otecel3G850",
                    "Otecel3G1900",
                    "Otecel4G850",
                    "Otecel4G1900",
                    "Cnt3G1900",
                    "Cnt4G850",
                    "Cnt4G1700",
                    "Cnt4G1900",
                    "Educación"
  ),
  options = layersControlOptions(collapsed = FALSE)
)






