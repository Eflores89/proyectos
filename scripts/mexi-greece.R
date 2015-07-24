# gráficas para artículo de EnElMargen.org: 

  library(ggplot2) #plots
  library(eem) #formatos de gráficas eem
  library(inegiR) #datos de inegi
  library(reshape2) #para arreglar info para plots

#add token 
  token<-"[token]"

# gráfica de tipo de cambio
  Tcambio<-ultimos(series_tipocambio(token),n = 36)
  ggplot(data = Tcambio, aes(x = Fechas, y = Valores))+
    geom_line(colour = "#A84A44")+
    labs(y = "MXP x 1USD", title = "Tipo de Cambio")+
    theme_eem()

#gráficas de balanza comercial
  Balanza<-ultimos(series_balanza_comercial(token), n = 36)
  #imports and exports solamente
  ggplot(data = melt(subset(Balanza, select = c(Fechas, Importaciones, Exportaciones)), id = "Fechas"))+
    geom_line(aes(x=Fechas, y = value, colour = variable))+
    labs(y = "Millones de dólares", title = "Imports - Exports")+
    scale_colour_manual(values = c("#A84A44","#E47D04"))+
    theme_eem()
  #balance
  ggplot(data = Balanza, aes(x = Fechas, y = Balance))+
    geom_line(colour = "#A84A44")+
    labs(y = "Miles de dólares", title = "Balance")+
    theme_eem()

#gráficas de crecimiento por zona
  regiones<-series_crecimiento_regiones(token)
  crecimiento<-cbind.data.frame("Fechas" = regiones$Fechas, 
                                apply(regiones[,2:length(regiones)], 2, function(x){ YoY(x, 4, FALSE)})
                                )
  ggplot(data = melt(data = ultimos(crecimiento, n = 11), id = "Fechas"))+
    geom_line(aes(x = Fechas, y = value, colour = variable))+
    theme_eem()+
    labs(x = "Fechas (Mes de Trimestre)", y = "Crecimiento anual (%)", title = "Crecimientos por Zona")+
    scale_colour_manual(values = c("#A84A44","#E47D04","#D8A19E","#ae8b38","#4d7c28"))
