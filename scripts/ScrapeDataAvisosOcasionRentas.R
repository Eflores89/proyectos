# Script para bajar información de casas en Venta en Mty, NL.
# versión 2 - 11 Diciembre, 2015
library(rvest)
library(eem)
library(stringi)
library(inegiR)

# -------
# página de inmuebles24.com
# el url base
url <- "http://www.inmuebles24.com/casas-en-renta-en-nuevo-leon.html"
# y quiero ver la cantidad de anuncios que hay, para ver los loops que voy a necesitar...
  totales <- read_html(paste0(url)) %>% 
              html_node("#id-resultado-busqueda") %>% 
              html_text() %>% as.numeric()

  loops <- ceiling(totales/24)

# imprimir para cuando lo haga con source
print(paste0("Encontrando.. ",totales," casos por descargar"))

# tipo de cambio a usar
tipocambio <- as.numeric(ultimos(series_tipocambio("61c36253-47f6-c616-034e-7bf43b1aaba4"), n = 0)['Valores'])

# voy a obtener el link directo a cada mini url con una descripción de casa
b <- NULL
bd <- NULL
for (i in 1:loops){
  url_descargar <- paste0("http://www.inmuebles24.com/casas-en-renta-en-nuevo-leon-pagina-",i,".html") 
  b <- read_html(url_descargar) %>% 
    html_nodes(".post-title") %>%
    html_nodes("a") %>%
    html_attr("href")

  b <- as.data.frame(unique(b))
  b <- paste0("http://www.inmuebles24.com", unlist(b))
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
  zona <- trimws(paste0(unlist(p)[index+1:3], 
                        collapse = " "))
  df <- data.frame("ZONA" = zona)
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
  if(trimws(unlist(p)[index+1])=="usd"){
    a_pesos <- gsub(pattern = ",", "", precio)
    a_pesos <- as.numeric(gsub(pattern = '\\$', "", a_pesos))
    a_pesos <- a_pesos*tipocambio
    df <- data.frame("PRECIO" = a_pesos)
  }else{
    a_pesos <- gsub(pattern = ",", "", precio)
    a_pesos <- as.numeric(gsub(pattern = '\\$', "", a_pesos))
    df <- data.frame("PRECIO" = a_pesos)
  }
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
  print("highlights ... ")
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
  print("descriptores ...")
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
      print("agregar a highlights...")
      highlights <- cbind.data.frame(highlights, n_desc)

  # agregar id y fechas
  i_s1 <- substring(text = bd[m,1], 
                   (regexpr(bd[m,1],
                            pattern = "ClaveAviso",
                            ignore.case = TRUE)[1])+11,
                   10000)
  i_s2 <- substring(i_s1,1,regexpr(pattern = "&",text = i_s1)-1)
        highlights$ID <- as.character(i_s2)
        highlights$HORA <- Sys.time()
        highlights$FECHA <- Sys.Date()
  
  bd_elnorte <- rbind.data.frame(bd_elnorte, highlights)
  
  end <- Sys.time()
  dif <- end-start
  # Mensaje de avance
  print(paste0("Descargada información de #", m, " en ",round(dif, 4), " segundos"))
}

print("Limpiando base...")
# Consistencia con acentos... 
bd_elnorte$COLONIA <- toupper(x = bd_elnorte$COLONIA)
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = "A", regex = "Á")
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = "E", regex = "É")
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = "I", regex = "Í")
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = "O", regex = "Ó")
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = "U", regex = "Ú")
# espacios de más 
bd_elnorte$COLONIA <- stri_replace_all(bd_elnorte$COLONIA, 
                                       replacement = " ", regex = "  ")
bd_elnorte$COLONIA <- trimws(bd_elnorte$COLONIA)

# columnas de numeros 
bd_elnorte$PRECIO <- as.numeric(as.character(bd_elnorte$PRECIO))
bd_elnorte$PISOS <- as.numeric(as.character(bd_elnorte$PISOS))
bd_elnorte$CONSTRUCCION <- as.numeric(as.character(bd_elnorte$CONSTRUCCION))
bd_elnorte$TERRENO <- as.numeric(as.character(bd_elnorte$TERRENO))
bd_elnorte$WC <- as.numeric(as.character(bd_elnorte$WC))
bd_elnorte$RECAMARAS <- as.numeric(as.character(bd_elnorte$RECAMARAS))

print("Creando columnas adicionales...")

bd_elnorte$TERRAZA <- grepl(pattern = "terraza", 
                            x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$VIGILANCIA <- grepl(pattern = "vigilancia", 
                            x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$JUEGOS <- grepl(pattern = "juego", 
                               x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$PARQUE <- grepl(pattern = "parque", 
                           x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$ALBERCA <- grepl(pattern = "alberca", 
                           x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$ASADOR <- grepl(pattern = "asador", 
                            x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)
bd_elnorte$NEGOCIABLE <- grepl(pattern = "negociable", 
                            x = bd_elnorte$INFO_COMPLETA, ignore.case = TRUE)

print("Generando archivos de resumen y base")
fecha <- paste0(substring(as.character(Sys.Date()),1,4),
                substring(as.character(Sys.Date()),6,7),
                substring(as.character(Sys.Date()),9,11))

bd_resumen <- bd_elnorte[!is.na(bd_elnorte$PRECIO),]
library(dplyr)
resumen <- bd_resumen %>% summarize("PROMEDIO" = mean(PRECIO),
                                    "DESVIACION" = sd(PRECIO),
                                    "CASAS" = n(), 
                                    "COLONIASUNICAS" = n_distinct(COLONIA),
                                    "PRECIOSUNICOS" = n_distinct(PRECIO),
                                    "TIEMPODESCARGA" = max(HORA)-min(HORA))

assign(x = paste0("resumen_", fecha),resumen)
assign(x = paste0("base_casas_venta_", fecha), bd_elnorte)

print("Guardando...")
setwd("/Users/eduardoflores/Documents/Dropbox/Data Science/R/proyectos/data/casas")
save(list = paste0("resumen_", fecha),
     file = paste0("resumen_", fecha,".RData"))
save(list = paste0("base_casas_venta_", fecha),
     file = paste0("base_casas_venta_", fecha,".RData"))
