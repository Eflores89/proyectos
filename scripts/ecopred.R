# setwd - no read
folder<-"/Users/eduardoflores/Documents/Dropbox/Data Science/R/proyectos/ecopred14_bd/"
setwd(folder)

# bibliotecas
library(foreign) #para leer dbf
library(dplyr) #para datos
library(tidyr) #datos
library(ggplot2) #graphs
library(eem) #graphs [devtools::install_github("eflores89/eem")]
library(stringr) #reclassificar

# Importar datos ----------------------------------- 
# consultar en: http://www.inegi.org.mx/est/contenidos/proyectos/encuestas/hogares/especiales/ecopred/2014/default.aspx
# agrego las llaves y otras variables, para no dificultarme las tareas despues...
viviendas <- read.dbf("tviviendas.dbf") %>% 
  mutate("KEY" = paste0(CONTROL, VIV_SEL),
         #clasificar estado por region
         "REGION" = ifelse(ENT == "02"|ENT == "03"|ENT == "05"|ENT == "08"|ENT == "19"|
                           ENT == "25"|ENT == "26"|ENT == "28", "Norte", 
                    ifelse(ENT == "01"|ENT == "06"|ENT == "10"|ENT == "11"|ENT == "14"
                          |ENT == "18"|ENT == "24"|ENT == "32", "Centro-Norte",
                    ifelse(ENT == "09"|ENT == "15", "Centro",
                    ifelse(ENT == "12"|ENT == "13"|ENT == "16"|ENT == "17"|ENT == "21"
                          |ENT == "22"|ENT == "29", "Centro-Sur","Sur-Sureste")))))

demografia <- read.dbf("tsdem.dbf") %>% mutate("KEY" = paste0(CONTROL, VIV_SEL),
                                               "KEY_U" = paste0(CONTROL, VIV_SEL, N_REN))

jefes <- read.dbf("tjefehogar.dbf") %>% mutate("KEY" = paste0(CONTROL, VIV_SEL))

jovenes_1 <- read.dbf("tjovenes_i_ii.dbf") %>% 
              mutate("KEY" = paste0(CONTROL, VIV_SEL), 
                     "KEY_U" = paste0(CONTROL, VIV_SEL, R_SEL))

jovenes_3 <- read.dbf("tjovenes_iii_iv.dbf") %>% 
              mutate("KEY" = paste0(CONTROL, VIV_SEL), 
                     "KEY_U" = paste0(CONTROL, VIV_SEL, R_SEL))

jovenes_5 <- read.dbf("tjovenes_v.dbf") %>% 
              mutate("KEY" = paste0(CONTROL, VIV_SEL), 
                     "KEY_U" = paste0(CONTROL, VIV_SEL, R_SEL))

jovenes_6 <- read.dbf("tjovenes_vi.dbf") %>% 
              mutate("KEY" = paste0(CONTROL, VIV_SEL), 
                     "KEY_U" = paste0(CONTROL, VIV_SEL, R_SEL)) 

jovenes_7 <- read.dbf("tjovenes_vii_viii.dbf") %>% 
              mutate("KEY" = paste0(CONTROL, VIV_SEL), 
                     "KEY_U" = paste0(CONTROL, VIV_SEL, R_SEL))
                     
#---------------------------------------------------------------------
#IMPORTANTE; 
# Tener cuidado, por que uso indices de columnas para extraer data sets, 
# si la fuente origen cambia esto ya no es válido
#---------------------------------------------------------------------

# Pregunta: cambiarte de casa  ----------------------------------------
# Cuestionario jovenes
# 5.3 Imagina que tú y tu familia tuvieran la oportunidad de mudarse a otra casa o departamento. 
# Si pudieran hacerlo, se mudarían a...

P_mudarte <- inner_join(jovenes_5[,c("P5_3", "KEY_U")], 
                        inner_join(demografia, 
                                   viviendas, 
                                   by = "KEY"), 
                        by = "KEY_U")

P_mudarte_xrazon <- P_mudarte %>% 
  group_by(EDAD, REGION) %>%
  mutate("CONTEO_TOTAL" = n_distinct(KEY_U)) %>%
  group_by(P5_3, CONTEO_TOTAL, add = TRUE) %>%
  summarise("CONTEO" = n_distinct(KEY_U)) %>%
  mutate("PORCENTAJE" = CONTEO/CONTEO_TOTAL)
 
# cambiar nombres...
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "1", "Otra Colonia")
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "2", "Otra Ciudad")
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "3", "Otro Estado")
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "4", "Otro País")
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "5", "No mudaría")
P_mudarte_xrazon$P5_3 <- str_replace_all(P_mudarte_xrazon$P5_3, pattern = "9", "No sabe/NR")

# graficar
ggplot(P_mudarte_xrazon, 
       aes(x = EDAD, 
           y = PORCENTAJE,
           fill = P5_3))+
  geom_bar(stat = "identity")+
  facet_grid(. ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  ylim(0,1)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "Si pudieras, te mudarías a...", 
       x = "Edad", 
       y = "% Respuestas")

# Pregunta: lo que sucede en tu barrio --------------------------------
# 5.19 En lo que va del año, 
# ¿qué tan frecuente has visto gente en tu colonia o barrio...

P_sucede <- inner_join(jovenes_5[,c(118:133, 192)], 
                        inner_join(demografia, 
                                   viviendas, 
                                   by = "KEY"), 
                        by = "KEY_U")
P_sucede <- P_sucede[, c(1:18,25,26,27,28,29,40,41,56)]

# todos los tipos
P_sucede_xfrec <- P_sucede %>% 
  gather(PREGUNTA, VALOR, 2:17) %>%
  filter(VALOR<9) %>%
  group_by(REGION, PREGUNTA, EDAD) %>%
  mutate("CONTEO_TOTAL" = n_distinct(KEY_U)) %>%
  group_by(CONTEO_TOTAL, VALOR, add = TRUE) %>%
  summarise("CONTEO" = n_distinct(KEY_U)) %>%
  mutate("PORCENTAJE" = CONTEO/CONTEO_TOTAL)

P_sucede_xfrec$VALOR <- str_replace_all(P_sucede_xfrec$VALOR, pattern = "1", "Muy Frec.")
P_sucede_xfrec$VALOR <- str_replace_all(P_sucede_xfrec$VALOR, pattern = "2", "Frecuente")
P_sucede_xfrec$VALOR <- str_replace_all(P_sucede_xfrec$VALOR, pattern = "3", "Poco Frec.")
P_sucede_xfrec$VALOR <- str_replace_all(P_sucede_xfrec$VALOR, pattern = "4", "Nada Frec.")

# Partiendo por percepción de peligro
  #minima: tomando alcohol, bloqueando calle, vendiendo piratería, peleas vecinos
P_sucede_xfrec_minimo <- P_sucede_xfrec %>%
  filter(PREGUNTA == "P5_19_1" | PREGUNTA == "P5_19_6" | PREGUNTA == "P5_19_9" | PREGUNTA == "P5_19_11")

P_sucede_xfrec_minimo$PREGUNTA <- str_replace_all(P_sucede_xfrec_minimo$PREGUNTA, pattern = "P5_19_11", "Discusiones")
P_sucede_xfrec_minimo$PREGUNTA <- str_replace_all(P_sucede_xfrec_minimo$PREGUNTA, pattern = "P5_19_1", "Ruido")
P_sucede_xfrec_minimo$PREGUNTA <- str_replace_all(P_sucede_xfrec_minimo$PREGUNTA, pattern = "P5_19_6", "Piratería")
P_sucede_xfrec_minimo$PREGUNTA <- str_replace_all(P_sucede_xfrec_minimo$PREGUNTA, pattern = "P5_19_9", "Bloqueando calle")

ggplot(P_sucede_xfrec_minimo,
       aes(x = EDAD, 
           y = PORCENTAJE, 
           fill = VALOR))+
  geom_bar(stat = "identity")+
  facet_grid(PREGUNTA ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "¿Qué tan frecuente...?", 
       x = "Edad", 
       y = "% Respuestas")

  #intensa: disparando armas, drogandose, extorsiones
P_sucede_xfrec_intenso <- P_sucede_xfrec %>%
  filter(PREGUNTA == "P5_19_16" | PREGUNTA == "P5_19_8" | PREGUNTA == "P5_19_15")

P_sucede_xfrec_intenso$PREGUNTA <- str_replace_all(P_sucede_xfrec_intenso$PREGUNTA, pattern = "P5_19_8", "Drogas")
P_sucede_xfrec_intenso$PREGUNTA <- str_replace_all(P_sucede_xfrec_intenso$PREGUNTA, pattern = "P5_19_15", "Extorsión")
P_sucede_xfrec_intenso$PREGUNTA <- str_replace_all(P_sucede_xfrec_intenso$PREGUNTA, pattern = "P5_19_16", "Disparos")

ggplot(P_sucede_xfrec_intenso,
       aes(x = EDAD, 
           y = PORCENTAJE, 
           fill = VALOR))+
  geom_bar(stat = "identity")+
  facet_grid(PREGUNTA ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "¿Qué tan frecuente...?", 
       x = "Edad", 
       y = "% Respuestas")

  #Inseguridad: prostitución, asaltos, robos 
P_sucede_xfrec_inseguro <- P_sucede_xfrec %>%
  filter(PREGUNTA == "P5_19_12" | PREGUNTA == "P5_19_13" | PREGUNTA == "P5_19_14")

P_sucede_xfrec_inseguro$PREGUNTA <- str_replace_all(P_sucede_xfrec_inseguro$PREGUNTA, pattern = "P5_19_12", "Prostitución")
P_sucede_xfrec_inseguro$PREGUNTA <- str_replace_all(P_sucede_xfrec_inseguro$PREGUNTA, pattern = "P5_19_13", "Robando casas")
P_sucede_xfrec_inseguro$PREGUNTA <- str_replace_all(P_sucede_xfrec_inseguro$PREGUNTA, pattern = "P5_19_14", "Asaltando Personas")

ggplot(P_sucede_xfrec_inseguro,
       aes(x = EDAD, 
           y = PORCENTAJE, 
           fill = VALOR))+
  geom_bar(stat = "identity")+
  facet_grid(PREGUNTA ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "¿Qué tan frecuente...?", 
       x = "Edad", 
       y = "% Respuestas")


# Pregunta: contra - medidas  --------------------------------
# Ante la situación... (RESPUESTA 5.19)
# ¿qué pasa cuando pasa x...?

P_medidas <- inner_join(jovenes_5[,c(134:149, 192)], 
                       inner_join(demografia, 
                                  viviendas, 
                                  by = "KEY"), 
                       by = "KEY_U")
P_medidas <- P_medidas[, c(1:18,25:29,40,41,56)]

names(P_medidas) <- c(names(P_medidas)[1], 
                      "RUIDO", "GRAFITI", "VENTANAS", "ARRANCONES",
                      "ALCOHOL","PIRATERIA","VENTA DROGA", "DROGA", "BLOQUEO", 
                      "PELEAS", "DISCUSIONES", "PROSTITUCION", 
                      "ROBANDO CASA", "ASALTO PERSONA", "EXTORSION", "DISPAROS", 
                      names(P_medidas)[18:length(names(P_medidas))])
                      
P_medidas_xrazon <- P_medidas %>% 
  gather(PREGUNTA, VALOR, 2:17) %>%
  filter(VALOR<9) %>%
  filter(!VALOR == "<NA>") %>%
  group_by(REGION, PREGUNTA, EDAD) %>%
  mutate("CONTEO_TOTAL" = n_distinct(KEY_U)) %>%
  group_by(CONTEO_TOTAL, VALOR, add = TRUE) %>%
  summarise("CONTEO" = n_distinct(KEY_U)) %>%
  mutate("PORCENTAJE" = CONTEO/CONTEO_TOTAL)

ggplot(P_medidas_xrazon,
       aes(x = EDAD, 
           y = PORCENTAJE, 
           fill = VALOR))+
  geom_bar(stat = "identity")+
  facet_grid(PREGUNTA ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "¿Qué tan frecuente...?", 
       x = "Edad", 
       y = "% Respuestas")

# solo algunas situaciones... 
P_medidas_xrazon_algunas <- P_medidas_xrazon %>% 
  filter(PREGUNTA == "RUIDO" | PREGUNTA == "GRAFITI" | PREGUNTA == "ROBANDO CASA" |
         PREGUNTA == "ASALTO PERSONA" | PREGUNTA == "EXTORSION" | PREGUNTA == "DISPAROS")

P_medidas_xrazon_algunas$VALOR <- str_replace_all(P_medidas_xrazon_algunas$VALOR, pattern = "1", "Vecinos dicen algo")
P_medidas_xrazon_algunas$VALOR <- str_replace_all(P_medidas_xrazon_algunas$VALOR, pattern = "2", "Vecinos lo resuelven")
P_medidas_xrazon_algunas$VALOR <- str_replace_all(P_medidas_xrazon_algunas$VALOR, pattern = "3", "Policía interviene")
P_medidas_xrazon_algunas$VALOR <- str_replace_all(P_medidas_xrazon_algunas$VALOR, pattern = "4", "Nada")

ggplot(P_medidas_xrazon_algunas,
       aes(x = EDAD, 
           y = PORCENTAJE, 
           fill = VALOR))+
  geom_bar(stat = "identity")+
  facet_grid(PREGUNTA ~ REGION)+
  theme_eem()+
  scale_fill_eem(20)+
  scale_x_discrete(breaks=c(12, 15, 18, 21, 24, 27))+
  labs(title = "Ante la situación, ¿Qué se hace?", 
       x = "Edad", 
       y = "% Respuestas")

# -------------------------------------------------
# Predecir con bosque
library(randomForest)
library(rpart)
library(partykit)

# Hacer data frame con info
df_mod <- inner_join(P_medidas, 
                     P_mudarte, by = "KEY_U") %>%
  inner_join(., P_sucede, by = "KEY_U") %>% 
  select(c(2:17,27,67:91)) %>%
  select(-c(KEY)

# hacer factores a character para manipular.
i <- sapply(df_mod, is.factor)
df_mod[i] <- lapply(df_mod[i], as.character)

# eliminar los que están vacios.
df_mod[,c(1:16)][is.na(df_mod[,c(1:16)])]<-0

# Información generada de si o no...
# mudarte 0 = No, 1 = Si
df_mod$P5_3 <- vapply(df_mod$P5_3, 
                      function(x) {if(x=="1" | x=="2" | x=="3" | x=="4")
                        {1} else {0}}, 
                      FUN.VALUE=double(1)) 

# cambiar nombres 
names(df_mod)<-c(names(df_mod)[1:6],
                 "VENTA_DROGA", 
                 names(df_mod)[8:12],
                 "ROBANDO_CASA","ASALTO_PERSONA",
                 names(df_mod)[15:41])

# volver a factorizar para modelo...
i <- sapply(df_mod, is.character)
df_mod[i] <- lapply(df_mod[i], as.factor)

# primero un arbolito, 
un_arbol <- rpart(formula = as.factor(P5_3) ~ .,
                  data = df_mod)

# un bosque...
modelo <- randomForest(as.factor(P5_3) ~ .,
                       data = df_mod,
                       importance = TRUE, 
                       ntree = 2000)
# graficar las importancias...
randomForest::varImpPlot(modelo, n.var = 20)


