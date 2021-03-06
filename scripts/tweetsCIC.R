# datos CIC
library(RSocrata) #read socrata data (github de Chicago)
library(inegiR) #denue [install_github("eflores89/inegiR")]
library(dplyr) #data wrangle
library(leaflet) #mapa
library(ggplot2) #graficas
library(eem) #graficas [install_github("eflores89/eem")]

# importar datos 
servicios <- read.socrata(url = "https://data.cic.mx/Servicios-P-blicos/Servicios-Publicos/5xhc-cnbq")
  #errores con el tipo de fecha
  servicios$fecha <- base::as.Date(servicios$fecha)

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
token_api <- "[token]" #no show
estadisticas_baches <- inegiR::denue_varios_stats(baches, 7, 6, token = token_api)

# unir 
baches <- cbind.data.frame(baches, estadisticas_baches)

# agrupar --------------------
  # Cantidad de negocios
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

# agrupar --------------------
  # Cantidad de empleados
  baches$EMPLEADOS_GRUPO<-unlist(lapply(X = baches$EMPLEADOS_EST, function(x){
    if(x>0 && x<501){"0 A 500"}else{
      if(x>500 && x<1001){"501 A 1000"}else{
        if(x>1000 && x<1501){"1001 A 1500"} else{
          if(x>1500 && x<2001){"1501 a 2000"}else{
            "Más de 2001"
          }
        }
      }
    }
  }))

# histograma por días
ggplot(baches, aes(x = fecha))+
  geom_histogram(fill = "#A84A44")+
  theme_eem()+
  labs(title = "Baches en Monterrey", x = "Fecha", y = "Baches Reportados")
  
# Gráfica en el tiempo ----------------------------------------
ggplot(baches, 
       aes(y = EMPLEADOS_EST, 
           x = fecha)) +
  geom_point(colour = "#A84A44")+
  theme_eem()+
  labs(title = "Baches en Monterrey", x = "Fecha", y = "Afectados Est.") 

# mapear a todos ---------------------------------------------
# iconos 
cafe <- "http://2.bp.blogspot.com/-Ae2bV9yrqKU/VcdwhpWi26I/AAAAAAAAAWs/3kbp3W2UYGI/s1600/popo2.png"
azul <- "http://2.bp.blogspot.com/-esfUfORj8OE/VcdwhmvoFGI/AAAAAAAAAW0/ZQUBwu0cYv0/s1600/popo2_blue.png"
rojo <- "http://2.bp.blogspot.com/-mNSxVkTwO38/VcdwiIY48_I/AAAAAAAAAW4/S3zLh5mM4zU/s1600/popo2_red.png"
verde <- "http://3.bp.blogspot.com/-WHcMOZg4cnk/Vcdwhj8uTqI/AAAAAAAAAWw/Dcuuiod2XF4/s1600/popo2_green.png"
naranja <- "http://3.bp.blogspot.com/-KSIhSiJm0nE/VcdwiEaKz1I/AAAAAAAAAW8/R5yf34ujA2s/s1600/popo2_orange.png"
sombra <- "http://3.bp.blogspot.com/-nPEuvF6pfYs/Vcdwislx78I/AAAAAAAAAXA/Dn_73FAGbEo/s1600/popo_shadow.png"

baches_coordenadas <- baches %>% select(Latitud, Longitud, NEGOCIOS_GRUPO)
mapa <- leaflet(data = baches_coordenadas) %>%
        addTiles() %>%
        addMarkers(~Longitud, ~Latitud, 
                   icon = makeIcon(cafe, sombra, 16, 15))

### Analizar, distribución ----------------------------------------------
# ver cuales son los más prioritarios
# primero, por la cantidad de negocios a 250 mts
baches_pornegocios <- baches %>% 
  group_by(NEGOCIOS_GRUPO) %>% 
  summarise(CONTEO = n_distinct(ubicacion))

ggplot(order_axis(baches_pornegocios, NEGOCIOS_GRUPO, CONTEO), 
       aes(x = NEGOCIOS_GRUPO_o, 
           y = CONTEO)) +
  geom_bar(stat = "identity", fill = "#A84A44") +
  theme_eem()+
  labs(title = "Baches y negocios \n A 250 metros", x = "Num. Negocios", y = "Num. Baches") 

# Ahora, por la cantidad de empleados, potenciales usuarios
baches_porempleados <- baches %>% 
  group_by(EMPLEADOS_GRUPO) %>% 
  summarise(CONTEO = n_distinct(ubicacion))

ggplot(order_axis(baches_porempleados, EMPLEADOS_GRUPO, CONTEO), 
       aes(x = EMPLEADOS_GRUPO_o, 
           y = CONTEO)) +
  geom_bar(stat = "identity", fill = "#A84A44") +
  theme_eem()+
  labs(title = "Baches y empleados est. \n A 250 metros", x = "Num. Empleados est.", y = "Num. Baches") 

### para ver cuales arreglar primero, varias consideraciones, 
 # baches más viejos y que afectan a más usuarios; 
baches_prioridad<- baches %>% mutate("PRIORIDAD" = 
                    ifelse(EMPLEADOS_GRUPO == "Más a 2000" | 
                           EMPLEADOS_GRUPO == "1501 a 2000" & fecha<'2013-07-01',1,
                    ifelse(EMPLEADOS_GRUPO == "Más a 2000" | 
                           EMPLEADOS_GRUPO == "1501 a 2000", 2, 
                    ifelse(EMPLEADOS_GRUPO == "1001 A 1500" | 
                           EMPLEADOS_GRUPO == "501 A 1000" , 3,4)))
                  )

baches_prioridad_mapear <- baches_prioridad %>% select(Latitud, Longitud, PRIORIDAD)

#Hacer iconos
iconos<-baches_prioridad_mapear %>% mutate("icono" = ifelse(PRIORIDAD == 1, rojo, 
                                                     ifelse(PRIORIDAD == 2, naranja, 
                                                     ifelse(PRIORIDAD == 3, azul, cafe))))
mapa_prioridades <- leaflet(data = baches_coordenadas) %>%
                    addTiles() %>%
                    addMarkers(~Longitud, ~Latitud, 
                    icon = makeIcon(iconos$icono, sombra, 16, 15))
