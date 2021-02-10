#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------

library(sf)
# Telecharger le fichier shape sous : https://cadastre.data.gouv.fr/data/etalab-cadastre/2020-01-01/shp/france/
shpsections <- st_read(dsn = '/DataCadastres/shape',layer = 'sections')

shpsections1<-shpsections[1:25000,]

library(rmapshaper)
shp1 <- rmapshaper::ms_simplify(shpsections1, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp1, file = "/shp1.rds")

shpsections2<-shpsections[25001:50000,]
shp2 <- rmapshaper::ms_simplify(shpsections2, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp2, file = "/shp2.rds")

shpsections3<-shpsections[50001:75000,]
shp3 <- rmapshaper::ms_simplify(shpsections3, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp3, file = "/shp3.rds")

shpsections4<-shpsections[75001:100000,]
shp4 <- rmapshaper::ms_simplify(shpsections4, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp4, file = "/shp4.rds")

shpsections5<-shpsections[100001:125000,]
shp5 <- rmapshaper::ms_simplify(shpsections5, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp5, file = "/shp5.rds")

shpsections6<-shpsections[125001:150000,]
shp6 <- rmapshaper::ms_simplify(shpsections6, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp6, file = "/shp6.rds")

shpsections7<-shpsections[150001:175000,]
shp7 <- rmapshaper::ms_simplify(shpsections7, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp7, file = "/shp7.rds")

shpsections8<-shpsections[175001:200000,]
shp8 <- rmapshaper::ms_simplify(shpsections8, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp8, file = "/shp8.rds")

shpsections9<-shpsections[200001:225000,]
shp9 <- rmapshaper::ms_simplify(shpsections9, keep = 0.01, keep_shapes = TRUE)
saveRDS(object = shp9, file = "/shp9.rds")

shpsections10<-shpsections[225001:250000,]
shpsections11<-shpsections[250001:275000,]
shpsections12<-shpsections[275001:300000,]
shpsections13<-shpsections[300001:325000,]
shpsections14<-shpsections[325001:350000,]
shpsections15<-shpsections[350001:375000,]
shpsections16<-shpsections[375001:400000,]
shpsections17<-shpsections[400001:425000,]
shpsections18<-shpsections[425001:450000,]

shp10 <- rmapshaper::ms_simplify(shpsections10, keep = 0.01, keep_shapes = TRUE)
shp11 <- rmapshaper::ms_simplify(shpsections11, keep = 0.01, keep_shapes = TRUE)
shp12 <- rmapshaper::ms_simplify(shpsections12, keep = 0.01, keep_shapes = TRUE)
shp13 <- rmapshaper::ms_simplify(shpsections13, keep = 0.01, keep_shapes = TRUE)
shp14 <- rmapshaper::ms_simplify(shpsections14, keep = 0.01, keep_shapes = TRUE)
shp15 <- rmapshaper::ms_simplify(shpsections15, keep = 0.01, keep_shapes = TRUE)
shp16 <- rmapshaper::ms_simplify(shpsections16, keep = 0.01, keep_shapes = TRUE)
shp17 <- rmapshaper::ms_simplify(shpsections17, keep = 0.01, keep_shapes = TRUE)
shp18 <- rmapshaper::ms_simplify(shpsections18, keep = 0.01, keep_shapes = TRUE)

saveRDS(object = shp10, file = "/shp10.rds")
saveRDS(object = shp11, file = "/shp11.rds")
saveRDS(object = shp12, file = "/shp12.rds")
saveRDS(object = shp13, file = "/shp13.rds")
saveRDS(object = shp14, file = "/shp14.rds")
saveRDS(object = shp15, file = "/shp15.rds")
saveRDS(object = shp16, file = "/shp16.rds")
saveRDS(object = shp17, file = "/shp17.rds")
saveRDS(object = shp18, file = "/shp18.rds")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
# TEST

shp10 <- readRDS(file = "/shp10.rds")

m <- leaflet(shp10) %>%
  addTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons(weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))


bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Creation de la base

library(data.table)
library(purrr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(dplyr)

load("don3.RData")

# Notre base
donSection<-don3[,colnames(don3) %in% c("Code.postal","CODGEO","Section","Commune")]
dim(donSection)
donSection<-as.data.table(donSection)

# Suppression des doublons
donSection<-donSection[,param:=duplicated(paste(CODGEO,Section, sep = "", collapse = NULL))] 
donSection2<-donSection[param==FALSE,]
dim(donSection2)

donSection2$CodeParc<-paste(donSection2$CODGEO,"-",donSection2$Section, sep = "", collapse = NULL)
donSection2$param<-NULL

donSection2$DupCodeParc<-duplicated(donSection2$CodeParc) 
donSection3<-donSection2[donSection2$DupCodeParc==FALSE,]

# Cadastres
library(sf)
shpsections <- st_read(dsn = '/shape',layer = 'sections')

head(shpsections)
shpsections$CodeParc<-paste(shpsections$commune,"-",shpsections$code, sep = "", collapse = NULL)
head(shpsections)
dim(shpsections)

shpsections$DupCodeParc<-duplicated(shpsections$CodeParc) 
shpsections<-shpsections[shpsections$DupCodeParc==FALSE,]
dim(shpsections)
head(shpsections)

# Fusion
shpsectionsProjet<-merge(shpsections, donSection2, by ="CodeParc")
dim(shpsectionsProjet)
class(shpsectionsProjet)
names(shpsectionsProjet)
shpsectionsProjet2 <- shpsectionsProjet[,colnames(shpsectionsProjet) %in% c("id","commune","code","geometry","CodeParc")]
class(shpsectionsProjet2)

# Sauvegarde
shpsec1<-shpsectionsProjet2[1:25000,]
shpsec2<-shpsectionsProjet2[25001:50000,]
shpsec3<-shpsectionsProjet2[50001:75000,]
shpsec4<-shpsectionsProjet2[75001:100000,]
shpsec5<-shpsectionsProjet2[100001:125000,]
shpsec6<-shpsectionsProjet2[125001:150000,]
shpsec7<-shpsectionsProjet2[150001:175000,]
shpsec8<-shpsectionsProjet2[175001:186733,]
dim(shpsec8)
library(rmapshaper)

shp1 <- rmapshaper::ms_simplify(shpsec1, keep = 0.01, keep_shapes = TRUE)
shp2 <- rmapshaper::ms_simplify(shpsec2, keep = 0.01, keep_shapes = TRUE)
shp3 <- rmapshaper::ms_simplify(shpsec3, keep = 0.01, keep_shapes = TRUE)
shp4 <- rmapshaper::ms_simplify(shpsec4, keep = 0.01, keep_shapes = TRUE)
shp5 <- rmapshaper::ms_simplify(shpsec5, keep = 0.01, keep_shapes = TRUE)
shp6 <- rmapshaper::ms_simplify(shpsec6, keep = 0.01, keep_shapes = TRUE)
shp7 <- rmapshaper::ms_simplify(shpsec7, keep = 0.01, keep_shapes = TRUE)
shp8 <- rmapshaper::ms_simplify(shpsec8, keep = 0.01, keep_shapes = TRUE)

shpfin<-rbind(shp1,shp2,shp3,shp4,shp5,shp6,shp7,shp8)
class(shpfin)
saveRDS(object = shpfin, file = "/shpfin.rds")

