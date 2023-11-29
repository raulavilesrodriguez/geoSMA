library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust) # to find the ideal number of clusters

df.poblacionE <- read_excel('poblacion.xlsx')
df.salud <- read_excel('./establecimientos/salud.xlsx')
df.internetE <- read_excel('./internetFijo/dfinternet.xlsx')
df.totalE <- read_excel('dftotalSMA.xlsx')

df.totalE <- df.totalE |> select(-c(1:4,7,10:37))

# change NA to 0
#df.totalE[which(is.na(df.totalE$TOTAL)), 10] <-0

# transform to numeric the population variable
colnames(df.poblacionE)[colnames(df.poblacionE) == '2020'] <- 'poblacion' 
df.poblacionE[,9] <- lapply(df.poblacionE[,9], as.numeric)
df.poblacionE$poblacion <- ifelse(is.na(df.poblacionE$poblacion), 0, df.poblacionE$poblacion)
df.poblacionE <- df.poblacionE |> mutate(poblacion = round(poblacion, 0))

# change DPA to numeric to compare with db.rbs
df.poblacionE[,5] <- sapply(df.poblacionE[,5], as.numeric)
df.internetE[,5] <- sapply(df.internetE[,5], as.numeric)

# Filter High Coverage
df.salud <- df.salud |> filter(Cobertura != 'Alta')
colnames(df.salud)[9] <- "DPA_PARROQ"

# join data frames of health with radio Bases SMA
df.salud <- left_join(df.salud, df.totalE, by = "DPA_PARROQ")
colnames(df.salud)[42] <- "rbs"

# join data frames of health with with fix internet
df.internetE[,5] <- sapply(df.internetE[,5], as.character)
df.salud[,9] <- sapply(df.salud[,9], as.character)

s1 <- apply(df.salud, 1, function(x){
  t <- ifelse(x[["PRV_DESC"]]==df.internetE[["PROVINCIA"]] &
                str_detect(df.internetE[["DPA_PARROQ"]], x[["DPA_PARROQ"]]), 
              df.internetE[["cuentasInt"]], '')
})
cuentasI <- apply(s1, 2, paste, collapse="")
df.salud$cuentasInt <- cuentasI

# select ot clasification
df <- df.salud |> select(poblacion, Orden, cuentasInt)
df[,3] <- sapply(df[,3], as.numeric)

# Replace NA with 0
df[is.na(df)] <- 0

# normalize data
df <- scale(df)
head(df)

# K-means Algorithm
set.seed(145)
k2 <- kmeans(df, centers = 8, nstart = 25)  
str(k2)
k2

fviz_cluster(k2, geom="point", data = df)

df.salud <- df.salud |> mutate(cluster = k2$cluster)

#testing
count(df.salud[which(df.salud$cluster == 1),])
count(df.salud[which(df.salud$cluster == 2),])
count(df.salud[which(df.salud$cluster == 3),])
count(df.salud[which(df.salud$cluster == 4),])
count(df.salud[which(df.salud$cluster == 5),])
count(df.salud[which(df.salud$cluster == 6),])
count(df.salud[which(df.salud$cluster == 7),])
count(df.salud[which(df.salud$cluster == 8),])

df.salud <- df.salud |>
  mutate(grupo = ifelse(cluster == 2, "G1",
                        ifelse(cluster == 3 | cluster == 5, "G2",
                               ifelse(cluster == 1 | cluster == 4 | cluster == 6, "G3",
                                      ifelse(cluster == 7 | cluster == 8, "G4", "G1")))))

df.salud <- df.salud |> 
  mutate(periodo = ifelse(grupo == "G1", "1-2 años",
                          ifelse(grupo == "G2", "3-4 años",
                                 ifelse(grupo == "G3", "5-6 años",
                                        ifelse(grupo == "G4", "7-8 años", "1-2 años")))))


# Number sities Group 1
nrow(df.salud |> filter(grupo == "G1"))

# Number sities Group 2
nrow(df.salud |> filter(grupo == "G2"))

# Number sities Group 3
nrow(df.salud |> filter(grupo == "G3"))

# Number sities Group 4
nrow(df.salud |> filter(grupo == "G4"))


finalSalud<- df.salud |> select(-c(29:36, 39))
#Export tibble witout mobile service telecom
writexl::write_xlsx(finalSalud, './ResultadosExpansión/saludClasificado.xlsx')



