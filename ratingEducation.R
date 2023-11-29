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
df.education <- read_excel('./establecimientos/educación.xlsx')
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
df.education <- df.education |> filter(Cobertura != 'Alta')
colnames(df.education)[6] <- "DPA_PARROQ"

# join data frames of education with radio Bases SMA
df.education <- left_join(df.education, df.totalE, by = "DPA_PARROQ")
colnames(df.education)[32] <- "rbs"

# join data frames of education with with fix internet
df.internetE[,5] <- sapply(df.internetE[,5], as.character)
df.education[,6] <- sapply(df.education[,6], as.character)

e1 <- apply(df.education, 1, function(x){
  t <- ifelse(x[["DPA_DESPRO"]]==df.internetE[["PROVINCIA"]] &
    str_detect(df.internetE[["DPA_PARROQ"]], x[["DPA_PARROQ"]]), 
              df.internetE[["cuentasInt"]], '')
})
cuentasI <- apply(e1, 2, paste, collapse="")
df.education$cuentasInt <- cuentasI

# select ot clasification
df <- df.education |> select(poblacion, Orden, cuentasInt)
df[,3] <- sapply(df[,3], as.numeric)

# Replace NA with 0
df[is.na(df)] <- 0

# normalize data
df <- scale(df)
head(df)

# K-means Algorithm
set.seed(203)
k1 <- kmeans(df, centers = 8, nstart = 25)  
str(k1)
k1

fviz_cluster(k1, geom="point", data = df)

df.education <- df.education |> mutate(cluster = k1$cluster)

#testing
count(df.education[which(df.education$cluster == 1),])
count(df.education[which(df.education$cluster == 2),])
count(df.education[which(df.education$cluster == 3),])
count(df.education[which(df.education$cluster == 4),])
count(df.education[which(df.education$cluster == 5),])
count(df.education[which(df.education$cluster == 6),])
count(df.education[which(df.education$cluster == 7),])
count(df.education[which(df.education$cluster == 8),])

# to split cluster 4 in two
cluster4 <- c(which(df.education$cluster == 4))
df.education$cluster[cluster4[1:length(cluster4)/2]] <- 4.1

# new test
count(df.education[which(df.education$cluster == 4),])
count(df.education[which(df.education$cluster == 4.1),])

df.education <- df.education |>
  mutate(grupo = ifelse(cluster == 4.1, "G1",
                    ifelse(cluster == 4 , "G2",
                        ifelse(cluster == 1 | cluster == 2 | cluster == 3 | cluster== 5, "G3", 
                            ifelse(cluster == 6 | cluster == 7 | cluster == 8, "G4", "G1")))))


df.education <- df.education |> 
  mutate(periodo = ifelse(grupo == "G1", "1-2 años",
                          ifelse(grupo == "G2", "3-4 años",
                                 ifelse(grupo == "G3", "5-6 años",
                                        ifelse(grupo == "G4", "7-8 años", "1-2 años")))))


# Number sities Group 1
nrow(df.education |> filter(grupo == "G1"))

# Number sities Group 2
nrow(df.education |> filter(grupo == "G2"))

# Number sities Group 3
nrow(df.education |> filter(grupo == "G3"))

# Number sities Group 4
nrow(df.education |> filter(grupo == "G4"))

finalEdu <- df.education |> select(-c(15:26,29))
#Export tibble witout mobile service telecom
writexl::write_xlsx(finalEdu, './ResultadosExpansión/educaciónClasificado.xlsx')



