# Minando datos CIC
library(RSocrata) #read socrata data
library(inegiR) #denue
library(dplyr) #data wrangle
library(leaflet) #mapa
library(ggplot2) #graficas

# importar datos 
servicios <- read.socrata(url = "https://data.cic.mx/Servicios-P-blicos/Servicios-Publicos/5xhc-cnbq")
  #errores con el tipo de fecha
  servicios$fecha <- as.Date(servicios$fecha)

#funciones para traer coordenadas...
ObtenerLatitud<-function(x)
{
  as.numeric(substring(text = x, first = 2, last = regexpr(pattern = ",", x)-1))
}
ObtenerLongitud<-function(x)
{
  a<-substring(text = x, 
               first = regexpr(pattern = ", ", x), 
               last = 1000)
  b<-substring(text = a, 
               first = 3, 
               last = regexpr(pattern = ")", a)-1)
  as.numeric(b)
}

# traerme solo los baches y agregar las coordenadas
baches <- servicios %>% 
  filter(substring(text = contenido, first = 2, last = 7)=="BACHES") %>%
  mutate("Longitud" = ObtenerLongitud(ubicacion), 
         "Latitud" = ObtenerLatitud(ubicacion))

# Traer estadisticas del denue
token_api <- "f3fe034d-3273-4be5-a5b3-45b990eb0534" #no show
estadisticas_baches <- inegiR::denue_varios_stats(baches, 7, 6, token = token_api)

# unir 
baches <- cbind.data.frame(baches, estadisticas_baches)

# agrupar 
baches$NEGOCIOS_GRUPO<-unlist(lapply(X = baches$NEGOCIOS, function(x){
  if(x>0 && x<31){"0 A 30"}else{
    if(x>30 && x<51){"31 A 50"}else{
      if(x>50 && x<101){"51 A 100"} else{
        if(x>100 && x<201){"101 a 200"}else{
          if(x>200 && x<401){"201 A 400"}else{
            "MAS DE 401"}
        }
      }
    }
  }
}))

# mapear -------------------------------------------------------
baches_coordenadas <- baches %>% select(Latitud, Longitud, NEGOCIOS_GRUPO)
mapa <- leaflet(data = baches_coordenadas) %>%
        addTiles() %>%
        addMarkers(~Longitud, ~Latitud)
#reordenar
ggplot(test, aes(x = reorder(NEGOCIOS_GRUPO, CONTEO), 
                 y = CONTEO)) +
  geom_bar(stat = "identity")

# visualizar objetos -----------------------------------------
# mapa de baches
mapa

# WIP-----------------------
test<-estadisticas_baches %>% 
  group_by(NEGOCIOS) %>% 
  summarise("BACHES" = n()) %>% 
  arrange(desc(BACHES))



ggplot(test, aes(x = c2))+geom_histogram()

ggplot(test, aes(x = BACHES, y = NEGOCIOS))+geom_point()+geom_smooth()

test<-estadisticas_baches %>% 
  group_by(NEGOCIOS_SOBRE_AVENIDA) %>% 
  summarise("BACHES" = n()) %>% 
  arrange(desc(BACHES)) %>% 
  filter(NEGOCIOS_SOBRE_AVENIDA>1)

ggplot(test, aes(x = BACHES, y = NEGOCIOS_SOBRE_AVENIDA))+geom_point()+geom_smooth()

