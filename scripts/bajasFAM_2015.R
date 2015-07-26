# Analisis de las bajas en las fuerzas armadas.
# información de: http://www.sedena.gob.mx/transparencia/personal-dado-de-baja-por-diferentes-motivos
library(xlsx)
library(ggplot2)
library(eem)
library(dplyr)

bajas<-read.xlsx("data/militar/bajas_tidy_excel.xlsx", 
                 sheetIndex = 1)

# Comportamiento a lo largo de la historia de las bajas...
ggplot(data = bajas, aes(x = FECHA, y = BAJAS)) +
  geom_bar(stat = "identity", fill = "#A84A44") +
    #estimación de tasa de bajas en 2015
    geom_point(aes(x ='2015p', 
                   y = sum(subset(bajas, FECHA == bajas$FECHA[length(bajas[,1])])$BAJAS)/3*12),
              colour = "#D8A19E")+
    annotate(geom = "text", x ='2015p', y = sum(subset(bajas, FECHA == bajas$FECHA[length(bajas[,1])])$BAJAS)/3*15,
              label ="Est.")+
  labs(title = "Bajas totales por Año - \n Fuerzas Armadas de México", 
       x = "Año", 
       y = "Bajas") +
  theme_eem()

# Porque cayeron las bajas tanto?
ggplot(data = bajas, aes(x = FECHA, y = RAZON)) + 
  geom_point(aes(size = BAJAS), colour = "#A84A44") + 
  labs(title = "Bajas por Fecha y Razón", x = "Año", y = "Razones") + 
  theme_eem()

# la razón, predominante, es la deserción... en que niveles de rango ha cambiado? 
deserciones<-subset(bajas, RAZON == unique(bajas$RAZON)[4])

ggplot(data = deserciones, aes(FECHA, BAJAS))+
  geom_bar(stat = "identity", aes(fill = RANGO_GRUPO))+
  labs(title = "Deserciones", x = "Año", y = "Bajas")+
  theme_eem()+
  scale_fill_manual(values = c("#4d7c28","#E47D04","#D8A19E","#A84A44"))

# La tropa todavía se divide en más rangos, sospecho que será el más bajo, pero investiguemos:
ggplot(data = subset(deserciones, RANGO_GRUPO == "TROPA"), aes(FECHA, BAJAS, fill = RANGO)) + 
  geom_bar(stat = "identity") +
  labs(title = "Deserciones (Solo Tropa)", x = "Año", y = "Bajas") +
  theme_eem()+
  scale_fill_manual(values = c("#4d7c28","#E47D04","#D8A19E","#A84A44"))


#### regresando a la alza en el 2015, que pasa contra el 2014? 
ultimos_2014<-subset(bajas, FECHA == '2014')
  #estimamos, a la misma tasa, todo el 2015
ultimos_2015<-subset(bajas, FECHA == '2015p')
  ultimos_2015$BAJAS<-ultimos_2015$BAJAS/3*12
ultimos<-rbind(ultimos_2015, ultimos_2014)

# en alguna razón hay una alza importante? 
ultimos_x_razon<-ultimos %>% group_by(FECHA, RAZON) %>% summarise(BAJAS = sum(BAJAS)) 

  #limpiar algunos nombres, para gráficar bien: 
  ultimos_x_razon$RAZON <- unlist(lapply(ultimos_x_razon$RAZON, function(x){gsub(pattern = "POR", replacement = "", x)}))  

ggplot(ultimos_x_razon, aes(x = FECHA, y = BAJAS, group = RAZON, fill = RAZON))+
  geom_path(aes(colour = RAZON), size = 1)+
  theme_eem()+
  theme(legend.position = "left")+
  scale_colour_manual(values = c("#A84A44","#E47D04","#D8A19E","#ae8b38",
                                 "#4d7c28","#38b6a6","#2080c7","#ce726e","#155685"))+
  labs(title = "Bajas 2014 y 2015 estimadas", legend = "Razones", y = "Bajas", x = "Año")


#Las alzas están en: "pasar rsv corresp, retiro y deserción";
ultimos_x_razones_alza<-subset(ultimos, RAZON == "PASAR RVA.CORRESP." | 
                                        RAZON == "DESERCIÓN" | 
                                        RAZON == "RETIRO")

ggplot(ultimos_x_razones_alza, 
       aes(x = RAZON, y = BAJAS, fill = RANGO_GRUPO))+
  geom_bar(stat = "identity")+
  facet_grid(. ~ FECHA)+
  scale_fill_manual(values = c("#A84A44","#E47D04","#D8A19E","#ae8b38",
                                 "#4d7c28","#38b6a6","#2080c7","#ce726e","#155685"))+
  theme_eem()
