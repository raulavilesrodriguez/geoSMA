library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinyjs)
library(rsconnect)
library(DT)
library(here)
library(readxl)
library(stringr)
library(highcharter)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust) # to find the ideal number of clusters
library(writexl)

#----Read Financial statements (eeff tibble)-------
dfsinservicio <- read_excel('dfsinservicio.xlsx')
str(dfsinservicio)

df <- dfsinservicio |> select(poblacion, cuentasInt)
# Replace NA with 0
df[is.na(df)] <- 0
row.names(df) <- dfsinservicio$DPA_PARROQ

# normalize data
df <- scale(df)
head(df)

#Check for data-types of columns
#kmeans can only be used on numerical columns, because it needs 
#to compute the mean.
sapply(df, class)

# set up the seed
set.seed(123)

#----DETERMING OPTIMAL CLUSTERS----
# Elbow Method
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(df, kmeans, method = "wss")

#Average Silhouette Method
fviz_nbclust(df, kmeans, method = "silhouette")

#Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

#---NBUST METHOD---
set.seed(123)
res<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, 
             method = "ward.D", index = "all")
#  According to the majority rule, the best number of clusters is  3

# adding nstart = 25 will generate 25 initial configurations. 
#This approach is often recommended.

set.seed(44)
k1 <- kmeans(df, centers = 9, nstart = 25)  
str(k1)
k1

fviz_cluster(k1, geom="point", data = df)

dataFinal <- dfsinservicio |> mutate(cluster = k1$cluster)

#testing
count(dataFinal[which(dataFinal$cluster == 1),])
count(dataFinal[which(dataFinal$cluster == 2),])
count(dataFinal[which(dataFinal$cluster == 3),])
count(dataFinal[which(dataFinal$cluster == 4),])
count(dataFinal[which(dataFinal$cluster == 5),])
count(dataFinal[which(dataFinal$cluster == 6),])
count(dataFinal[which(dataFinal$cluster == 7),])
count(dataFinal[which(dataFinal$cluster == 8),])
count(dataFinal[which(dataFinal$cluster == 9),])

dataFinal <- dataFinal |>
  mutate(grupo = ifelse(cluster == 8, "G1",
                        ifelse(cluster == 7 | cluster == 1, "G2",
                               ifelse(cluster == 9 | cluster == 4 , "G3", 
                                      ifelse(cluster == 6 | cluster == 5 | cluster == 3 | cluster == 2, "G4", "G1")))))

dataFinal <- dataFinal |> 
  mutate(periodo = ifelse(grupo == "G1", "1-2 años",
                          ifelse(grupo == "G2", "3-4 años",
                                 ifelse(grupo == "G3", "5-6 años",
                                        ifelse(grupo == "G4", "7-8 años", "1-2 años")))))


# Number sities Group 1
nrow(dataFinal |> filter(grupo == "G1"))

# Number sities Group 2
nrow(dataFinal |> filter(grupo == "G2"))

# Number sities Group 3
nrow(dataFinal |> filter(grupo == "G3"))

# Number sities Group 4
nrow(dataFinal |> filter(grupo == "G4"))


dataFinal <- dataFinal |> select(-c(9:12))
#Export tibble witout mobile service telecom
writexl::write_xlsx(dataFinal, './ResultadosExpansión/sinservicioClasificado.xlsx')

