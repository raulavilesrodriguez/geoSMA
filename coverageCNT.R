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
db.cnt.LTE700 <- read_excel('01 Rbs_Cnt_octubre_2023.xlsm',
                               sheet = 'LTE(700)')
db.cnt.UMTS1900 <- read_excel('01 Rbs_Cnt_octubre_2023.xlsm',
                                sheet = 'UMTS(1900)')
db.cnt.LTE1900 <- read_excel('01 Rbs_Cnt_octubre_2023.xlsm',
                               sheet = 'LTE 1900')
db.cnt.AWS1700 <- read_excel('01 Rbs_Cnt_octubre_2023.xlsm',
                                sheet = 'AWS (17002100)')

colnames(db.cnt.LTE700) <- c(1:52)
db.cnt.LTE700 <- db.cnt.LTE700[-1,]
db.cnt.LTE700 <- db.cnt.LTE700[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.UMTS1900) <- c(1:52)
db.cnt.UMTS1900 <- db.cnt.UMTS1900[-1,]
db.cnt.UMTS1900 <- db.cnt.UMTS1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.LTE1900) <- c(1:52)
db.cnt.LTE1900 <- db.cnt.LTE1900[-1,]
db.cnt.LTE1900 <- db.cnt.LTE1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.AWS1700) <- c(1:53)
db.cnt.AWS1700 <- db.cnt.AWS1700[-1,]
db.cnt.AWS1700 <- db.cnt.AWS1700[,c(2,3,4,8,11,12,13,14,15,16,17)]

db.cnt <- rbind(
  db.cnt.LTE700,
  db.cnt.UMTS1900,
  db.cnt.LTE1900,
  db.cnt.AWS1700
)

colnames(db.cnt) <- c(
  "NOMBRE DE LA RADIOBASE",
  "banda",
  "TECNOLOGIA",
  "CENTRAL A LA QUE SE CONECTA (MSC)",
  "PROVINCIA",
  "CANTON",
  "PARROQUIA",
  "DPA",
  "DIRECCCION",
  "LATITUD",
  "LONGITUD"
)

# to suppress duplicate to sector x y z
db.cnt <- db.cnt |> group_by(`NOMBRE DE LA RADIOBASE`) |>
  distinct()







