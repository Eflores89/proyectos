# Script para bajar información de casas en Venta en Mty, NL.
# versión 1 - 6 Diciembre, 2015
library(rvest)
library(eem)

# -------
# página de avisos de ocasión de El Norte
# avisosdeocasion.com / bienes raíces, NL y venta solamente
# el url base
url <- "http://www.avisosdeocasion.com/Resultados-Inmuebles.aspx?n=venta-casas-nuevo-leon&PlazaBusqueda=2&Plaza=2&pagina="
# y quiero ver la cantidad de anuncios que hay, para ver los loops que voy a necesitar...
  totales <- read_html(paste0(url,1)) %>% 
              html_node(".ar13naranja") %>% 
              html_text()

  # solamente funciona si hay más de mil entradas...
  totales_n <- as.numeric(gsub(pattern = ",",
                               replacement = "",
                               stringi::stri_extract(totales,
                                                     regex = "[0-9],[0-9][0-9][0-9]")))
  loops <- ceiling(totales_n/15)

# voy a obtener el link directo a cada mini url con una descripción de casa
b <- NULL
bd <- NULL
for (i in 1:loops){
  url_descargar <- paste0(url,i) 
  b <- read_html(url_descargar) %>% 
    html_nodes(".tituloresult , .nombre") %>%
    html_nodes("a") %>%
    html_attr("href")

  b <- as.data.frame(unique(b))
  # store
  bd <- rbind.data.frame(bd, b)  
  # mensaje para el desesperado
  print(paste0("Lista descarga de sitio #",i))
}

# ya con todos los links, me voy anuncio por anuncio, extrayendo lo de interés...

