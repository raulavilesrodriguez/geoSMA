library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)

#________Wrangling_________
df.poblacion <- read_excel('poblacion.xlsx')
db.conecel <- read_excel('01 Rbs_Conecel_2023-10.xlsm')
colnames(db.conecel) <- db.conecel[1,]
db.conecel <- db.conecel[-1,]
db.conecel <- db.conecel[,c(2,3,4,8,11,12,13,14,15,16,17)]

#---Change LATITUDE---
db.conecel <- db.conecel |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))

lat.conecel <- sapply(db.conecel$LATITUD, function(x){
  x <- unlist(x)
  degrees <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  seconds <- as.numeric(x[3])
  direction <- x[5]
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  if(direction == 'S'){
    decimal_degrees <- -decimal_degrees
  } else{
    decimal_degrees
  }
})
db.conecel$LATITUD <- lat.conecel

#---Change LONGUITUDE---
db.conecel <- db.conecel |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long.conecel <- sapply(db.conecel$LONGITUD, function(x){
  x <- unlist(x)
  degrees <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  seconds <- as.numeric(x[3])
  direction <- x[5]
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  if(direction == 'W'){
    decimal_degrees <- -decimal_degrees
  } else{
    decimal_degrees
  }
})
db.conecel$LONGITUD <- long.conecel

colnames(db.conecel)[8] <- "DPA" 

tipoConecel <- lapply(db.conecel$DPA, function(x){
    #unlist(x)
    t <- ifelse(x==df.poblacion$DPA_PARROQ, df.poblacion$TIPO, NA)
    ifelse(which(!is.na(t)), t[which(!is.na(t))], "HO")
  })
db.conecel <- cbind(db.conecel, as.matrix(tipoConecel))
colnames(db.conecel)[ncol(db.conecel)] <- "tipo"


#Propagation radio[Km]
urbana850 <- 0.5
rural850 <- 3.0
urbana1900 <- 0.4
rural1900 <- 2.0
urbanaAWS <- 1.05
ruralAWS <- 3.23

colnames(db.conecel)[2] <- "banda"

db.conecel <- db.conecel |> 
  mutate(coverage = ifelse(tipo=='RURAL' & banda=='850', 3.0, 
                    ifelse(banda=='850', 0.5, 
                    ifelse(tipo=='RURAL' & banda=='1900', 2.0,
                    ifelse(banda=='1900', 0.4,
                    ifelse(tipo=='RURAL' & banda=='1700', 3.23, 1.05))))))

db.conecel850 <- db.conecel |> filter(banda=='850')
db.conecel1900 <- db.conecel |> filter(banda=='1900')
db.conecel1700 <- db.conecel |> filter(banda=='1700')

map <- leaflet() %>% addTiles()
# Create a leaflet map
map <- map %>%
  addCircles(
    data = db.conecel850,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "red",
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
    color = "green",
    fillOpacity = 0.1,
    group = "1700",
    weight = 0  # Set weight to 0 to remove the border
  )

# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("850", "1900", "1700"),
  options = layersControlOptions(collapsed = FALSE)
)
