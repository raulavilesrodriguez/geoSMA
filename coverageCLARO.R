library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)
library(here)

source(here::here('grades_to_decimal.R'))
source(here::here('join_parroquias.R'))
source(here::here('propagation.R'))

#________Wrangling_________
df.poblacion <- read_excel('poblacion.xlsx')
db.conecel <- read_excel('01 Rbs_Conecel_2023-10.xlsm')
colnames(db.conecel) <- db.conecel[1,]
db.conecel <- db.conecel[-1,]
db.conecel <- db.conecel[,c(2,3,4,8,11,12,13,14,15,16,17)]

#---Change LATITUDE---
db.conecel <- db.conecel |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.conecel <- grades_to_decimal(db.conecel$LATITUD)
db.conecel$LATITUD <- lat.conecel

#---Change LONGUITUDE---
db.conecel <- db.conecel |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long.conecel <- grades_to_decimal(db.conecel$LONGITUD)
db.conecel$LONGITUD <- long.conecel

colnames(db.conecel)[8] <- "DPA"

#---Join to base poblacion to determinate if is URBAN, RURAL, ETC
tipoConecel <- join_parroquias(db.conecel$DPA, df.poblacion)

db.conecel <- cbind(db.conecel, as.matrix(tipoConecel))
colnames(db.conecel)[ncol(db.conecel)] <- "tipo"


#Add Propagation radio[Km]
colnames(db.conecel)[2] <- "banda"

db.conecel <- db.conecel |> 
  mutate(coverage = ifelse(tipo=='RURAL' & banda=='850', rural850, 
                    ifelse(banda=='850', urbana850, 
                    ifelse(tipo=='RURAL' & banda=='1900', rural1900,
                    ifelse(banda=='1900', urbana1900,
                    ifelse(tipo=='RURAL' & banda=='1700', ruralAWS, urbanaAWS))))))

db.conecel850 <- db.conecel |> filter(banda=='850')
db.conecel1900 <- db.conecel |> filter(banda=='1900')
db.conecel1700 <- db.conecel |> filter(banda=='1700')

#----Ploting-----
map <- leaflet() %>% addTiles()
# Create a leaflet map
map <- map %>%
  addCircles(
    data = db.conecel850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "green",
    fillOpacity = 0.1,
    group = "850",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel1900,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "blue",
    fillOpacity = 0.1,
    group = "1900",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel1700,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "red",
    fillOpacity = 0.1,
    group = "1700",
    weight = 0  # Set weight to 0 to remove the border
  )

# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("850", "1900", "1700"),
  options = layersControlOptions(collapsed = FALSE)
)
