# Salvar dados em .kml

df <- readr::read_csv("tabela_de_dados.csv")

coordinates(df)<-c("Long","Lat")          #Build a SpatialPointsData Frame
proj4string(df)<-CRS("+proj=longlat +datum=WGS84")

##The two lines below convert the month and day columns to character dat
##(both of these line are originally 'factor' data, which is not compatible)
df$Data<-as.character(df$Data)
df$anotador <-as.character(df$anotador)

##The one line of code below writes our SpatialPointsDataFrame to a KML File
writeOGR(df, dsn="dados_pinus.kml", layer= "df", driver="KML")
