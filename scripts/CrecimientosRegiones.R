# crecimientos de las regiones en México...

library(ggplot2)
library(inegiR)
library(reshape2)
library(eem)

token<-"[token]"
crecimientos<-series_crecimiento_regiones(token)

df<-data.frame("fecha" = crecimientos$Fechas, 
     "norte" = YoY(crecimientos$Norte, 4)*100, 
     "centro_nte" = YoY(crecimientos$Centro_Norte, 4)*100,
     "centro" = YoY(crecimientos$Centro, 4)*100, 
     "centro_sur" = YoY(crecimientos$Centro_Sur, 4)*100, 
     "sur" = YoY(crecimientos$Sur, 4)*100) 

ggplot(melt(df, id.vars = "fecha"), 
       aes(x = fecha, y = value, group = variable, colour = variable))+
  geom_path()+
  theme_eem()+
  scale_colour_eem(20)+
  labs(title = "Crecimiento Zonas México", x = "Fecha", y = "Tasa Anual")
