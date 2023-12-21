library(tidyverse)
library(patchwork)
# Lê os dados em csv
df <- read_csv("/home/mcure/Downloads/Dados supressão de Pinus PMMC - Pontos.xlsx - Pontos Lucas(1).csv")

# Substitui símbolos por espaço mantendo apenas os números das coordenadas
## Longitude
df$`Coordenada x` <- gsub(df$`Coordenada x`, pattern = "°", replacement = " ")
df$`Coordenada x` <- gsub(df$`Coordenada x`, pattern = "'", replacement = " ")
df$`Coordenada x` <-gsub(df$`Coordenada x`, pattern = "\"", replacement = " ")
df$`Coordenada x` <-gsub(df$`Coordenada x`, pattern = "S", replacement = "")
df$`Coordenada x` <-gsub(df$`Coordenada x`, pattern = "O", replacement = "")

## Latitude
df$`Coordenada Y` <- gsub(df$`Coordenada Y`, pattern = "°", replacement = " ")
df$`Coordenada Y` <- gsub(df$`Coordenada Y`, pattern = "'", replacement = " ")
df$`Coordenada Y` <-gsub(df$`Coordenada Y`, pattern = "\"", replacement = " ")
df$`Coordenada Y` <-gsub(df$`Coordenada Y`, pattern = "S", replacement = "")
df$`Coordenada Y` <-gsub(df$`Coordenada Y`, pattern = "O", replacement = "")

# Rename colums
df <- df %>%
  mutate(Data = as.Date(df$Data, format = "%d.%m.%Y")) %>%
  add_column(anotador = rep("Lucas", dim(df)[1]))

df <- df %>%
  rename(Lat = `Coordenada x`) %>%
  rename(Long = `Coordenada Y`)

########## Pontos Alexandre

df1 <- read_csv("/home/mcure/Downloads/Dados supressão de Pinus PMMC - Pontos.xlsx - Pontos Alexandre.csv")

###

to_decimal <- function(coord) {
  parts <- strsplit(coord, "[°'\" ]")
  degrees <- as.numeric(substr(parts[[1]][1],2,nchar(parts[[1]][1])))
  minutes <- as.numeric(substr(parts[[1]][3],1,2))
  seconds <- as.numeric(substr(parts[[1]][3],3,nchar(parts[[1]][3])))
  direction <- parts[[1]][4]
  decimal <- degrees + minutes / 60 + seconds / 3600
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Convertendo para decimal
latitude_decimal <- to_decimal(df1$Latitutde)
longitude_decimal <- to_decimal(df1$Longitude)

latitude <- vector("list", length = dim(df1)[1])
for (i in 1:dim(df1)[1]) {
  latitude[i] <- to_decimal(df1$Latitutde[i])
  print(latitude%>% unlist)
}

longitude <- vector("list", length = dim(df1)[1])
for (i in 1:dim(df1)[1]) {
  longitude[i] <- to_decimal(df1$Longitude[i])
  print(longitude[i]%>% unlist)
}


df1 <- df1 %>%
  mutate(Latitutde = latitude %>% unlist) %>%
  mutate(Longitude = longitude %>% unlist) %>%
  add_column(anotador = rep("Cure", dim(df1)[1]))

###
# Rename colums
df1 <- df1 %>%
  mutate(Data = as.Date(df1$Data, format = "%d.%m.%Y"))

df1 <- df1 %>%
  rename(Long = Longitude) %>%
  rename(Lat = Latitutde)

##########
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}


df$Lat <- angle2dec(df$Lat)
df$Long <- angle2dec(df$Long)


df <- rbind(df, df1)

df <- df %>%
  mutate(Lat = -Lat) %>%
  mutate(Long = -Long)
