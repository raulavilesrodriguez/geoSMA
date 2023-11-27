library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)


df.poblacion <- read_excel('poblacion.xlsx')
df.internet <- read_excel('Junio_2023_internet_fijo.xls')
colnames(df.internet)[14] <- 'internetFijo'
df.internet <- df.internet[, c(4:6, 14)]
df.internet <- df.internet |> group_by(PROVINCIA,Cantón,PARROQUIA) |>
  summarise(
    cuentasInt = sum(internetFijo)
  )
df.internet <- df.internet[-1,]

# apply work to dataframe or tibble to extract DPA
we <- apply(df.internet, 1, function(x){
  t <- ifelse(x[["Cantón"]]==df.poblacion[["DPA_DESCAN"]] & 
                (str_detect(df.poblacion[["DPA_DESPAR"]], x[["PARROQUIA"]])),
              df.poblacion[["DPA_PARROQ"]], ''
  )
})
ve <- apply(we, 2, paste, collapse="")
df.internet$DPA_PARROQ <- ve

#df.internet <- df.internet |> mutate(DPA1 = str_extract(DPA_PARROQ, "\\d{6}"))

df.int1 <- df.internet |> filter(DPA_PARROQ !='')
df.int2 <- df.internet |> filter(DPA_PARROQ == '')

# apply work to dataframe or tibble to extract DPA
match1 <- apply(df.int2, 1, function(x){
  t <- ifelse(x[["PROVINCIA"]]==df.poblacion[["DPA_DESPRO"]] & 
                (str_detect(df.poblacion[["DPA_DESPAR"]], x[["PARROQUIA"]])),
              df.poblacion[["DPA_PARROQ"]], ''
  )
})

m1 <- apply(match1, 2, paste, collapse="")
df.int2$DPA_PARROQ <- m1

df.int3 <- df.int2 |> filter(DPA_PARROQ != '')
df.int3[which(df.int3$PARROQUIA == 'SAN SEBASTIAN'),5] <- '110102'
df.int3[which(df.int3$PARROQUIA == 'EL PARAISO'),5] <- '070702'

df.intermedio <- rbind(df.int1, df.int3)

df.int4 <- df.int2 |> filter(DPA_PARROQ == '')

writexl::write_xlsx(df.int4, './internetFijo/completar.xlsx')

df.int4 <- read_excel('./internetFijo/completar -lleno.xlsx')

df.int4$DPA_PARROQ <- sapply(df.int4$DPA_PARROQ, as.character)

# join to rows the 2 data frames
df.final <- rbind(df.intermedio, df.int4)

writexl::write_xlsx(df.final, './internetFijo/dfinternet.xlsx')

