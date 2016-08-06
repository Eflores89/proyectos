# Datos de inflación por ciudad...
library(inegiR)
library(eem)
library(dplyr)
library(reshape2)
library(ggplot2)

# Descargar datos ----
token <- "****"
d <- inflacion_ciudades(token)
e <- inflacion_general(token)


# Limpiar para graficar ----
dg <- d %>% 
  filter(Fechas>'2012-12-01') %>%
  select(c(DF, Monterrey, Guadalajara, Fechas)) %>%
  melt(id.vars = "Fechas") %>%
  mutate("Tipo" = "Ciudad grande", 
         "Ciudad" = variable)

dnc <- d %>%
  filter(Fechas>'2012-12-01') %>%
  select(c(Monclova, LaPaz, Torreon, Queretaro, 
           Culiacan, Toluca, Fechas)) %>%
  melt(id.vars = "Fechas") %>%
  mutate("Tipo" = "sin CNTE", 
         "Ciudad" = variable)

dc <- d %>%
  filter(Fechas>'2012-12-01') %>%
  select(c(Iguala, Oaxaca, Tapachula, Fechas)) %>%
  melt(id.vars = "Fechas") %>%
  mutate("Tipo" = "CNTE", 
         "Ciudad" = variable)
  
general <- e %>% 
  filter(Fechas>'2012-12-01') %>%
  melt(id.vars = "Fechas") %>%
  mutate("Tipo" = "Nacional", 
         "Ciudad" = "Nacional")

d_all <- rbind.data.frame(
  rbind.data.frame(
  rbind.data.frame(dg, dnc), dc),
  general)

# Graficar -----

ggplot(d_all, aes(x = Fechas, y = value, 
                  group = Ciudad, color = Tipo))+
  geom_path()+
  theme_eem()+
  labs(x = "Fechas", y = "Inflación (YoY)", 
       title = "Inflación por ciudad (seleccionadas)")

# tablas

tb1 <- d_all %>% 
  group_by(Tipo) %>% 
  summarise("Promedio" = mean(value),
            "Mediana" = median(value),
            "Desv." = sd(value),
            "Max" = max(value))

tb2 <- d_all %>%
  group_by(Ciudad) %>% 
  summarise("Promedio" = mean(value),
            "Mediana" = median(value),
            "Desv." = sd(value),
            "Max" = max(value)) %>%
  arrange(Promedio)

tb3 <- d_all %>%
  filter(Fechas>'2015-12-01') %>%
  group_by(Ciudad) %>% 
  summarise("Promedio" = mean(value),
            "Mediana" = median(value),
            "Desv." = sd(value),
            "Max" = max(value)) %>%
  arrange(Promedio)

# boxplot ---- 
ggplot(d_all, 
       aes(x = Tipo, y = value))+
  geom_boxplot(color = eem_colors[1], 
               fill = eem_colors[3]) + 
  theme_eem() + labs(x = "Tipo de Ciudad", y = "Tasa de inflación", 
                     title = "Comparativa de tasas de inflación \n por tipo de ciudad")

# Efectos con cambios... por tipo de fecha... 
efcnte_cause <- d_all %>% 
  mutate("FechaTipo" = ifelse(Fechas<'2015-12-01',"Antes", "Actual") ) %>% 
  group_by(Ciudad, Tipo, FechaTipo) %>% 
  summarise("Promedio" = mean(value)) 
efcnte_cause$FechaTipo <- factor(efcnte_cause$FechaTipo, 
                                 levels = c("Antes", "Actual"))


ggplot(efcnte_cause, 
       aes (x = FechaTipo, 
            y = Promedio, 
            group = Ciudad))+
  geom_path(aes(colour = Tipo)) +
  labs(x = "Fecha (2016)", y = "Promedio de tasas de Inflación", 
       title = "Cambio en tasas promedio de inflación \n por tipo de ciudad")+
  theme_eem()+
  scale_colour_eem(20)

# tablas --- 

kable(tb1)
kable(tb2)
kable(tb3)