### descarga de datos de vuelos en México

series <- c("15112","15113","15122","15123")
nombres <- c("Llegadas Nac.", "Llegadas Int.","Salidas Nac.", "Salidas Int.")
token_inegi <- "*******"
  
str_inegi <- function(codigo){
  s <- paste0("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/", codigo, "/00000/en/false/xml/")
  s
}


# esta funcion la tome de inegiR, pero como las series tienen un error (nov-2005), hice un cambio minimo...
serie_inegi2 <- function (serie, token, metadata = FALSE, coercionar = TRUE) 
{
  if (grepl(pattern = "xml/$", x = serie) | grepl(pattern = "json/$", 
                                                  x = serie)) {
  }
  else {
    stop("La serie no termina con xml/ o json/")
  }
  if (grepl(pattern = "json/$", x = serie)) {
    df <- serie_inegi_json(serie, token, metadata, coercionar)
    return(df)
  }
  else {
    serie <- paste0(serie, token)
    s <- xmlToList(serie)
    if (!is.null(s$ErrorInfo)) {
      warning(paste0("Error de INEGI: ", s$ErrorInfo))
      return(NULL)
    }
    Fechas <- ldply(.data = s$Data$Serie, .fun = "[[", "TimePeriod")[, 
                                                                     "[["]
    Fechas <- Fechas[!Fechas == "2005/11"] # error fix
    if (s$MetaData$Freq == "Anual" | s$MetaData$Freq == "Yearly" | 
        s$MetaData$Freq == "Annual" | s$MetaData$Freq == 
        "Quinquenal" | s$MetaData$Freq == "Decenal" | s$MetaData$Freq == 
        "Bienal") {
      Fechas_Date <- as.Date(zoo::as.yearmon(x = paste0("01/", 
                                                        Fechas), format = "%m/%Y"))
    }
    else {
      if (s$MetaData$Freq == "Trimestral" | s$MetaData$Freq == 
          "Quarterly") {
        Fechas <- gsub(pattern = "/04", replacement = "/10", 
                       x = Fechas)
        Fechas <- gsub(pattern = "/03", replacement = "/07", 
                       x = Fechas)
        Fechas <- gsub(pattern = "/02", replacement = "/04", 
                       x = Fechas)
        Fechas_Date <- as.Date(zoo::as.yearmon(Fechas, 
                                               "%Y/%m"))
      }
      else {
        if (s$MetaData$Freq == "Quincenal") {
          if (coercionar) {
            Mensaje <- "Importante: El indicador quincenal fue coercionado a meses - para evitar, correr con opcion coercionar=FALSE"
            Fechas_Date <- as.Date(zoo::as.yearmon(Fechas, 
                                                   "%Y/%m"))
          }
          else {
            FechasN <- gsub(pattern = "/02$", replacement = "/15", 
                            x = Fechas)
            Fechas_Date <- as.Date(x = FechasN, format = "%Y/%m/%d")
          }
        }
        else {
          Fechas_Date <- zoo::as.Date(zoo::as.yearmon(Fechas, "%Y/%m"))
        }
      }
    }
    Valores <- as.numeric(ldply(s$Data$Serie, "[[", "CurrentValue")[, 
                                                                    "[["])
    df <- cbind.data.frame(as.numeric(Valores), Fechas_Date)
    names(df) <- c("Valores", "Fechas")
    class(df[, "Valores"]) <- "numeric"
    if (metadata) {
      MetaData <- list(Nombre = s$MetaData$Name, UltimaActualizacion = s$MetaData$LastUpdate, 
                       Region = s$MetaData$Region, Unidad = s$MetaData$Unit, 
                       Indicador = s$MetaData$Indicator, Frecuencia = s$MetaData$Freq)
      Obj <- list(MetaData = MetaData, Datos = df)
      if (exists("Mensaje")) {
        warning(print(Mensaje))
      }
      return(Obj)
    }
    else {
      if (exists("Mensaje")) {
        warning(print(Mensaje))
      }
      return(df)
    }
  }
}

df <- NULL
for(i in 1:length(series)){
  tmp <- serie_inegi2(serie = str_inegi(series[i]), 
                     token = token_inegi, metadata = T)
  tmp_df <- tmp$Datos
  tmp_df$Indicador <- paste0(tmp$MetaData$Nombre, tmp$MetaData$Unidad)
  tmp_df$ID <- tmp$MetaData$Indicador
  tmp_df$NOMBRE <- nombres[i]
  
  df <- rbind.data.frame(df, tmp_df)
}


library(eem)
library(ggplot2)

ggplot(data = df, aes(x = Fechas, y = Valores/1000, group = NOMBRE)) + 
  geom_path(aes(colour = NOMBRE)) + 
  theme_eem() + 
  scale_colour_eem(20) + 
  labs(title = "Pasajeros Aeros en México", y = "Miles de Pasajeros", x = "Fecha")