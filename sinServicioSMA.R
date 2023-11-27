library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)

#________Wrangling_________
df.rbs <- read_excel(
  '1.2-Radiobases-por-operador-y-tecnologia-nivel-provincial_Sep23.xlsx',
  sheet = 'RBSxPARQ')
df.poblacion <- read_excel('poblacion.xlsx')

# transform to numeric the poblation variable
colnames(df.poblacion)[colnames(df.poblacion) == '2020'] <- 'poblacion' 
df.poblacion[,9] <- lapply(df.poblacion[,9], as.numeric)
df.poblacion$poblacion <- ifelse(is.na(df.poblacion$poblacion), 0, df.poblacion$poblacion)
df.poblacion <- df.poblacion |> mutate(poblacion = round(poblacion, 0))

# change DPA to numeric to compare with db.rbs
df.poblacion[,5] <- sapply(df.poblacion[,5], as.numeric)

#df.rbs
df.rbs <- df.rbs[-c(1:8, 1054),]
df.rbs[1,(5:11)]='CONECEL S.A.'
df.rbs[1,(12:17)]='OTECEL S.A.'
df.rbs[1,(18:21)]='CNT E.P'
df.rbs[1, which(is.na(df.rbs[1, ]))] = ''
new_names <- sapply(df.rbs[1:2, ], paste, collapse=" ")
colnames(df.rbs) <- new_names
df.rbs <- df.rbs[-(1:2),]

# transform of character to numeric
df.rbs[,-c(1:3)] <- lapply(df.rbs[,-c(1:3)], as.numeric)
colnames(df.rbs)[4] <- "DPA_PARROQ"

df.rbs2 <- df.rbs |>
  mutate(
    CONECEL_2G = `CONECEL S.A. GSM 850` + `CONECEL S.A. GSM 1900`,
    CONECEL_3G = `CONECEL S.A. UMTS 850` + `CONECEL S.A. UMTS 1900`,
    CONECEL_4G = `CONECEL S.A. LTE 850` + `CONECEL S.A. LTE 1700` + `CONECEL S.A. LTE 1900`,
    OTECEL_2G = `OTECEL S.A. GSM 850` + `OTECEL S.A. GSM 1900`,
    OTECEL_3G = `OTECEL S.A. UMTS 850` + `OTECEL S.A. UMTS 1900`,
    OTECEL_4G = `OTECEL S.A. LTE 1900` + `OTECEL S.A. LTE 850`,
    CNT_3G = `CNT E.P UMTS 1900`,
    CNT_4G = df.rbs[["CNT E.P LTE\r\n700"]] + df.rbs[["CNT E.P LTE\r\n1700"]] + 
      df.rbs[["CNT E.P LTE 1900"]]
  )

df.rbs2 <- df.rbs2 |> 
  mutate(TOTAL = CONECEL_2G + CONECEL_3G + CONECEL_4G +
           OTECEL_2G + OTECEL_3G + OTECEL_4G +
           CNT_3G + CNT_4G)

# join tibbles
df.total <- left_join(df.poblacion, df.rbs2, by = "DPA_PARROQ")

df.sin <- df.total |>
  filter(TIPO != 'URBANO', TOTAL==0)

df.sin <- df.sin[, -c(7,10:38)]
colnames(df.sin)[2] <- 'PROVINCIA'
colnames(df.sin)[4] <- 'CANTÓN'
colnames(df.sin)[6] <- 'PARROQUIA'

df.internet <- read_excel('./internetFijo/dfinternet.xlsx')


df.internet$DPA_PARROQ <- sapply(df.internet$DPA_PARROQ, as.numeric)
df.sin2 <- left_join(df.sin, df.internet, by = "DPA_PARROQ")

#Export tibble witout mobile service telecom
writexl::write_xlsx(df.sin2, 'dfsinservicio.xlsx')


