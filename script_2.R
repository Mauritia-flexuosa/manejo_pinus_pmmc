##############
library(tidyverse)
library(patchwork)

df <- read_csv("tabela_de_dados.csv")

library(sf)
library(rnaturalearth)
#library(rnaturalearthhires)


MC <- st_read("/home/mcure/Downloads/PNMMC25.10.23.kmz.kml")

MC <- MC[2]

MC %>% plot
# BRA <- ne_states(country = "Brazil",
#                  returnclass = "sf")

qtde <- df %>% group_by(Pontos) %>%
  summarise(qdt = sum(Pequenos, Médios, Grandes))


mapa1 <- ggplot(MC) +
  geom_sf(fill = "grey", color = "purple")+
  coord_sf() +
  geom_jitter(data = df %>%
                add_column(qtd = qtde$qdt), mapping = aes(x = Long, y = Lat, color = factor(Data), alpha = 0.7), show.legend = T)+
  xlim(c(-48.524, -48.485))+
  ylim(c(-27.639, -27.59))+
  #  facet_wrap(~factor(anotador))+
  ylab("Latitude")+
  xlab("Longitude")+
  guides(shape = "none", alpha = "none")+
  labs(color = "Data do manejo")+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid = element_line(colour = "grey",linetype = "dotted"))


disco <- df %>%
  pivot_longer(cols = c("Pequenos", "Médios", "Grandes")) %>%
  group_by(name) %>%
  summarise(contagem = sum(value)) %>%
  ggplot(aes(fill = factor(name), x = "", y = contagem))+
  geom_bar(width = 1,stat = "identity")+
  scale_fill_manual(values = c("purple", "orange", "pink"))+
  coord_polar("y", start = 0, direction = -1)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13))+
  geom_text(aes(x ="", y=700, label = "1284"))+
  geom_text(aes(x ="", y=1700, label = "922"))+
  geom_text(aes(x ="", y=2950, label = "1609")) +
  labs(title = "Quantidade de Pinus removidos", fill = "Tamanho")

# png("/home/mcure/Documents/Alexandre/mapa_pinus.png", res = 300, width = 2400, height = 1800)
# mapa1/disco
# dev.off()
##################

library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)
library(sf)

MC <- sf::st_as_sf(MC)
MC %>% plot

MC <- readOGR(dsn = "/home/mcure/Documents/maciço da costeira/UCs_floripa/", layer = "ma_uc", verbose = F, GDAL1_integer64_policy = F)

ne_mc <- subset(MC, MC$nm_uc %in% c(
  "MACICO DA COSTEIRA"
))

shapeMC <- spTransform(ne_mc, CRS("+proj=longlat +ellps=WGS84"))

Pequenos <- df$Pequenos
Médios <- df$Médios
Grandes <- df$Grandes
Cones <- df$Cones
Data <- df$Data

# Adiciona perfumarias
mydrawPolylineOptions <- function(allowIntersection = TRUE,
                                  drawError = list(color = "#b00b00", timeout = 2500),
                                  guidelineDistance = 20, metric = TRUE, feet = FALSE, zIndexOffset = 2000,
                                  shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
  leaflet::filterNULL(list(allowIntersection = allowIntersection,
                           drawError = drawError, guidelineDistance = guidelineDistance,
                           metric = metric, feet = feet, zIndexOffset = zIndexOffset,
                           shapeOptions = shapeOptions,  repeatMode = repeatMode)) }


greenLeafIcon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/128/2220/2220059.png",
  iconWidth = 35, iconHeight = 53,
  iconAnchorX = 22, iconAnchorY = 54)


pinus <- leaflet()  %>% addTiles() %>%
  #  setView(lng = -106.363590, lat=31.968483,zoom=11) %>%
  addPolygons(data=shapeMC,weight=2,col = 'grey') %>%
  addProviderTiles('Esri.WorldImagery') %>%
  leaflet::addMarkers(lng = df$Long, lat = df$Lat,
                      popup = paste0("<b>Data: </b>", Data,"<br>","<b>Total de Pinus derrubados: </b>", qtde$qdt, "<br>", "<b>Pequenos: </b>", Pequenos,"<br>",
                                     "<b>Médios: </b>", Médios,"<br>","<b>Grandes: </b>", Grandes,"<br>","<b>Cones: </b>", Cones),
                      group = "addMarkers", label = paste0(qtde$qdt, " Pinus a menos neste ponto"),icon = greenLeafIcon) %>%
  addScaleBar(position = "topright") %>%
  addDrawToolbar(
    polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
    editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()))
# %>%
#  addLayersControl(baseGroups = c("addMarkers","addCircleMarkers"),
#                  options = layersControlOptions(collapsed = F))
