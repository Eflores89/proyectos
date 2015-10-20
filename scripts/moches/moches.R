library(rvest)

# --- obtener la info...

setwd("/Users/eduardoflores/Documents/Dropbox/Data Science/R/proyectos/scripts/moches")

moches_1 <- "home.html"
moches_2 <- "home2.html"
moches_3 <- "home3.html"
moches_4 <- "home4.html"
moches_5 <- "home5.html"
moches_6 <- "home6.html"

p <- "#moches-lista > div.table-responsive > table"

tabla_moches_1 <- moches_1 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()
tabla_moches_2 <- moches_2 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()
tabla_moches_3 <- moches_3 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()
tabla_moches_4 <- moches_4 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()
tabla_moches_5 <- moches_5 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()
tabla_moches_6 <- moches_6 %>% html() %>% html_nodes(p) %>% html_table() %>% data.frame()

tabla_moches_todos <- rbind(tabla_moches_1, 
                            tabla_moches_2)
tabla_moches_todos <- rbind(tabla_moches_todos, 
                            tabla_moches_3)
tabla_moches_todos <- rbind(tabla_moches_todos, 
                            tabla_moches_4)
tabla_moches_todos <- rbind(tabla_moches_todos, 
                            tabla_moches_5)
tabla_moches_todos <- rbind(tabla_moches_todos, 
                            tabla_moches_6)

# ---- analizar

library(dplyr)

# - por municipio (quito los estatales)

moches_municipio <- tabla_moches_todos %>% 
  filter(A.quién != "Administración estatal") %>%
  group_by(Municipio) %>%
  summarise("Casos" = n(),
            "Promedio" = mean(Moche),
            "Mediana" = median(Moche),
            "Desviación" = sd(Moche)
            )


# - por razon
moches_razon <- tabla_moches_todos %>% 
  group_by(Para.qué) %>%
  summarise("Casos" = n(),
            "Promedio" = mean(Moche),
            "Mediana" = median(Moche),
            "Desviación" = sd(Moche)
  )

# gráfica calor de razones y municipios selectos

tabla_grafica <- tabla_moches_todos %>%
  filter(Para.qué == "Conectar o reconectar un servicio" |
         Para.qué == "No ser detenido" | 
         Para.qué == "No pagar una multa" | 
         Para.qué == "Me dejara ir" | 
         Para.qué == "Que no se llevaran mi coche"
           ) %>%
  filter(Municipio == "Monterrey" | 
        Municipio == "San Pedro Garza García" | 
        Municipio == "San Nicolás de los Garza" |
        Municipio == "Guadalupe") %>%
# quitar dos outliers...
  filter(Moche<29999)

library(ggplot2)
library(eem)

ggplot(tabla_grafica, 
       aes(x = Municipio, 
           fill = Para.qué))+
  geom_histogram(stat = "bin")+
  theme_eem() +
  scale_fill_eem(20) +
  labs(x = "Municipio",
       y = "Casos",
       title = "Moches por municipio")

# heatmap
tabla_hm <- tabla_grafica %>% 
  group_by(Para.qué, 
           Municipio) %>%
  summarise("Conteo" = n(),
            "Promedio" = mean(Moche)) %>%
  ungroup()
  
# --- por conteo 
ggplot(tabla_hm, 
       aes(x = Para.qué, 
           y = Municipio, 
           fill = Conteo))+
  geom_tile()+
  theme_eem_white() +
  scale_colour_eem(20)

# --- por promedio de mordida
ggplot(tabla_hm, 
       aes(x = Para.qué, 
           y = Municipio, 
           fill = Promedio))+
  geom_tile()+
  theme_eem() +
  theme(line = element_blank())+
scale_fill_eem(20)
