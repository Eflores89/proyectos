# Script para bajar información de casas en Venta en Mty, NL.
# versión 1 - 6 Diciembre, 2015
library(rvest)
library(eem)
library(stringi)

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

# imprimir para cuando lo haga con source
print(paste0("Encontrando.. ",loops," casos por descargar"))

# voy a obtener el link directo a cada mini url con una descripción de casa
b <- NULL
bd <- NULL
for (i in 1:loops){
  url_descargar <- paste0(url, i) 
  b <- read_html(url_descargar) %>% 
    html_nodes(".tituloresult , .nombre") %>%
    html_nodes("a") %>%
    html_attr("href")

  b <- as.data.frame(unique(b))
  # store
  bd <- rbind.data.frame(bd, b)  
  # mensaje para el desesperado
  print(paste0("Lista descarga de sitio #", i))
}

# asegurarme de no repetir el mismo url...
bd <- unique(bd)
# quitar espacios y poner %20 en su lugar...
espacio <- function (x) gsub("\\s", "%20", x)
bd <- as.data.frame(apply(bd, 1, espacio))

# ya con todos los links, me voy a ir anuncio por anuncio, extrayendo lo de interés...
# hice unas funciones para esto...

# para quitar trailing spaces
trimws <- function (x) gsub("^\\s+|\\s+$", "", x)

# para buscar la zona
extraerZona <- function(p){
  index <- which(grepl(pattern = "ZONA:", unlist(p)))
  df <- data.frame("ZONA" = as.character(unlist(p)[index+1]))
   if(nrow(df)==0){df <- data.frame("ZONA" = "ND")}
  return(df)
}

# para buscar la colonia
extraerColonia <- function(p){
  index <- which(grepl(pattern = "COLONIA:",unlist(p)))
  colonia <- trimws(paste0(unlist(p)[index+1:6], 
                           collapse = " "))
  df <- data.frame("COLONIA" = colonia)
   if(nrow(df)==0){df <- data.frame("COLONIA" = "ND")}
  return(df)
}

# para buscar el precio
extraerPrecio <- function(p){
  index <- which(grepl(pattern = '\\$' ,unlist(p)))
  precio <- trimws(unlist(p)[index])
  a_pesos <- gsub(pattern = ",", "", precio)
  a_pesos <- as.numeric(gsub(pattern = '\\$', "", a_pesos))
  df <- data.frame("PRECIO" = a_pesos)
  if(nrow(df)==0){df <- data.frame("PRECIO" = "ND")}
  return(df)
}

# para buscar el numero de recamaras
extraerRecamaras <- function(p){
  index <- which(grepl(pattern = "Rec[áa]maras" ,unlist(p)))
  recamaras <- trimws(unlist(p)[index-1])
  recamaras <- as.numeric(recamaras)
  df <- data.frame("RECAMARAS" = recamaras)
  if(nrow(df)==0){df <- data.frame("RECAMARAS" = "ND")}
  return(df)
}

# para buscar el numero de baños
extraerWC <- function(p){
  index <- which(grepl(pattern = "Ba[nñ]o[\\ss]" ,unlist(p)))
  wcs <- trimws(unlist(p)[index-1])
  wcs <- as.numeric(wcs)
  df <- data.frame("WC" = wcs)
  if(nrow(df)==0){df <- data.frame("WC" = "ND")}
  return(df)
}

# para buscar los metros de terreno
extraerTerreno <- function(p){
  index <- which(grepl(pattern = "Terreno", unlist(f),
                       ignore.case = TRUE))
  if(grepl(pattern = "de", 
           x = unlist(p)[index-1], 
           ignore.case = TRUE)){
    index <- index-1
  }else{}
  terreno <- trimws(unlist(p)[index-1])
  terreno <- gsub(pattern = "m²",
                  replacement = "",
                  x = terreno)
  terreno <- as.numeric(terreno)
  df <- data.frame("TERRENO" = terreno)
   if(nrow(df)==0){df <- data.frame("TERRENO" = "ND")}
  return(df)
}

# para buscar los metros de construccion
extraerConstruccion <- function(p){
  index <- which(grepl(pattern = "Construcci[oó]n", unlist(f),
                       ignore.case = TRUE))
  if(grepl(pattern = "de", 
           x = unlist(p)[index-1], 
           ignore.case = TRUE)){
    index <- index-1
  }else{}
  const <- trimws(unlist(p)[index-1])
  const <- gsub(pattern = "m²",
                  replacement = "",
                  x = const)
  const <- as.numeric(const)
  df <- data.frame("CONSTRUCCION" = const)
   if(nrow(df)==0){df <- data.frame("CONSTRUCCION" = "ND")}
  return(df)
}

# para buscar el numero de pisos
extraerPisos <- function(p){
  index <- which(grepl(pattern = "Planta", unlist(f),
                       ignore.case = TRUE))
  if(grepl(pattern = "de", 
           x = unlist(p)[index-1], 
           ignore.case = TRUE)){
    index <- index-1
  }else{}
  pisos <- trimws(unlist(p)[index-1])
  pisos <- gsub(pattern = "m²",
                replacement = "",
                x = pisos)
  pisos <- as.numeric(pisos)
  df <- data.frame("PISOS" = pisos)
  if(nrow(df)==0){df <- data.frame("PISOS" = "ND")}
  return(df)
}


## ---- 
# Loop, para cada link de un caso de casa en venta...
# extraer todos los indicadores y pegar en un data.frame a todos...
n_anuncios <- length(bd[,1])
bd_elnorte <- NULL
for(m in 1:n_anuncios){
  start <- Sys.time()
  # columna de highlights (zona, colonia, precio)
  p <- read_html(as.character(bd[m,1])) %>% 
    html_nodes("#highlights") %>%
    html_text(trim = TRUE) %>%
    gsub(pattern = "\\s", "--", .) %>%
    stringi::stri_split_regex(str = ., 
                              pattern = "--",
    )
  # Construir un data frame con esta información...
  highlights <- NULL
  highlights <- cbind.data.frame(
    extraerZona(p),
    extraerColonia(p),
    extraerPrecio(p)
    )
  
  # descripción larga 
  inf <- read_html(as.character(bd[m,1])) %>% 
    html_nodes("#infocompleta") %>% html_text(trim = TRUE)
  
    # incorporar a hihghlights
    highlights$INFO_COMPLETA <- inf
  
  # descriptores n13 a un lado... 
  f <- read_html(as.character(bd[m,1])) %>% 
    html_nodes("#detallep") %>% 
    html_nodes(".carac_td") %>% 
    html_nodes("h3") %>% 
    html_nodes(".ar13gris") %>% 
    html_text(trim = TRUE) %>% 
    gsub(pattern = "\\s", "--", .) %>%
    stringi::stri_split_regex(str = ., 
                              pattern = "--",
    )
  
  if(length(f)==5){
      # un data frame normal
    n_desc <- NULL
    n_desc <- cbind.data.frame(
      extraerPisos(f),
      extraerConstruccion(f),
      extraerTerreno(f),
      extraerWC(f),
      extraerRecamaras(f)
    ) 
    
  }else{
    n_desc <- NULL
    n_desc <- data.frame(
      PISOS = "ND", 
      CONSTRUCCION = "ND",
      TERRENO = "ND", 
      WC = "ND",
      RECAMARAS = "ND"
      )
  }
      # agregar a highlights...
      highlights <- cbind.data.frame(highlights, n_desc)
  
  bd_elnorte <- rbind.data.frame(bd_elnorte, highlights)
  
  end <- Sys.time()
  dif <- end-start
  # Mensaje de avance
  print(paste0("Descargada información de #", m, " en ",round(dif, 4), " segundos"))
}



