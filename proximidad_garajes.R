library(tidyverse)  # Paquete multiuso
library(sf)  # Paquete clave para manipular datos espaciales
library(leaflet)  # Uno de los paquetes para 
library(devtools)
library(hereR)



install_github(repo = "martinmontane/wrapar")




require(wrapar)





calles <- read_sf("D:/Documents/01-Ditella/Instrumentos de Análisis Urbano II/callejero-rar/callejero.shp")





salud <- read_csv("https://github.com/martinmontane/AnalisisEspacialEnR/raw/master/data/HospitalesYCentrosSalud.csv")




garajes <- read_delim("D:/Documents/02-trabajo/r/garajes-comerciales.csv",delim = ",")






leaflet(garajes) %>% 
  addTiles() %>%
  addMarkers(label = ~ nombre,
             popup = ~ pisos_16)



# Esta key es falsa, por obvias razones. Reemplacen este valor por uno que funcione

set_key("00")










garajes <- st_as_sf(garajes,coords=c("long","lat"), crs=4326)


manzanas <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/manzanas/mapa_manzanas.geojson")





garajes <- st_transform(garajes, crs="+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs")

manzanas <- st_transform(manzanas,
                         crs="+proj=tmerc +lat_0=-34.629269 +lon_0=-58.4633 +k=0.9999980000000001 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs ")






# Efectivamente calculamos las isocronas para cada uno de los puntos
isocronosgarajes <- map(1:nrow(garajes),function(x) {
  # Esta línea es solo para que nos vaya avisando qué está haciendo
  cat("Procesando: ",x,"\r")
  isoline(garajes[x,], mode = "pedestrian",range_type = "time",range = 60*3)
})
# Los juntamos en el mismo data frame
isocronosgarajes2 <- do.call(rbind,isocronosgarajes)
# Hacemos un gran poligono
isocronosgarajes2union <-  st_union(isocronosgarajes2)





leaflet(isocronosgarajes2union %>% st_transform(4326)) %>% 
  addTiles() %>% 
  addPolygons()




# Transformamos la proyección de las manzanas
manzanas <- manzanas %>%
  st_transform("+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs")
# Transformamos la proyección de la cobertura de espacios verdes
isocronosgarajes2union <- isocronosgarajes2union %>%
  st_transform("+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs")
# Lo convertimos en un objeto sf y creamos una columna, cobertura, que tendrá valor TRUE siempre
isocronosgarajes2union <- st_as_sf(isocronosgarajes2union) %>% 
  mutate(cobertura=TRUE)
# Spatial join
manzanas <- st_join(manzanas,st_as_sf(isocronosgarajes2union))
# Completamos los datos para los casos en los cuales no hubo ningún resultadoe en el match
manzanas <- manzanas %>% mutate(cobertura=ifelse(is.na(cobertura),FALSE,TRUE))





ggplot(manzanas) +
  geom_sf(aes(fill=cobertura), color=NA) +
  theme_minimal() +
  coord_sf(datum=NA) +
  scale_fill_manual(values = c("#377eb8","#e41a1c"),
                    breaks = c(TRUE,FALSE),
                    labels=c("Menos de 3 minutos","Más de 3 minutos"))






