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

# TODOS LOS TIPOS
P_sucede_xfrec <- P_sucede %>% 
  gather(PREGUNTA, VALOR, 2:17) %>%
  filter(VALOR<9)%>%
  group_by(REGION, PREGUNTA, EDAD) %>%
  summarise("FREC_PROMEDIO" = mean(as.numeric(VALOR)))

# Partiendo por percepción de peligro
  #minima: tomando alcohol, bloqueando calle, vendiendo piratería, peleas vecinos
P_sucede_xfrec_minimo <- P_sucede_xfrec %>%
  filter(PREGUNTA == "P5_19_1" | PREGUNTA == "P5_19_6" | PREGUNTA == "P5_19_9" | PREGUNTA == "P5_19_11")


  #intensa: disparando armas, drogandose, extorsiones
P_sucede_xfrec_intenso <- P_sucede_xfrec %>%
  filter(PREGUNTA == "P5_19_16" | PREGUNTA == "P5_19_8" | PREGUNTA == "P5_19_15")


ggplot(P_sucede_xfrec_intenso,
       aes(x = PREGUNTA, 
           y = FREC_PROMEDIO,
           colour = EDAD))+
  geom_point()+
  facet_grid(. ~ REGION)









# Pregunta: hacer deporte  ----------------------------------------
# Cuestionario jovenes
# 5.13 En lo que va del año, 
# ¿con qué frecuencia participas en esta asociación o grupo?
#  respuestas de: Deportivo (futbol, baloncesto, voleibol, beisbol, baile, clases de zumba, etc.)

P_deporte <- jovenes_5[,c("P5_13_2","KEY")]
P_deporte_xfrec <- P_deporte %>% 
  group_by(P5_13_2) %>%
  summarise("CONTEO" = n_distinct(KEY))

# graficar
ggplot(order_axis(P_deporte_xfrec, P5_13_2, CONTEO), 
       aes(x = P5_13_2_o, 
           y = CONTEO))+
  geom_bar(stat = "identity", fill = "#A84A44")+
  theme_eem()+
  labs(title = "Participación en actividad deportiva", 
       x = "Lugar", 
       y = "Respuestas")

# no puede ser...
#situaciones_emborracharte <- situaciones %>% 
 #                 mutate("KEY" = paste0(CONTROL,VIV_SEL)) %>%
  #                #solo los que si han tomado/peda
   #               filter(P4_6_2==1) %>%
    #              select("EdadPrimeraVez" = P4_6_2A, 
     #                    "Actual"=P4_6_2B) 



# -------------------------------------------------

## con gps estaría interesante; 
# ¿Qué crees que piensan otras personas de la gente que vive en tu colonia?

## 
#5.11 Actualmente, ¿participas en algún tipo de actividad CIRCULE UN SOLO CÓDIGO
#o formas parte de un grupo, programa o campaña al
#interior de tu colonia o barrio (artístico, deportivo,
 #                                religioso, de apoyo social, político, de salud, de
  #                               fomento económico, profesional, comunitario o de
   #                              prevención de la violencia)?



# En lo que va del año, ¿qué tan frecuente has visto gente en tu colonia o barrio...


# 8.3 ¿Qué tan sencillo consideras que en un futuro, tú...




#Pregunta: tus amigos y su desmadre...
# Cuestionario jovenes
# 4.3 Piensa en tus mejores amigos o los compañeros con los que más convives. 
# De las situaciones descritas a continuación, dime por favor
# ¿si enlo que va del año alguno de ellos...

P_amigos <- inner_join(jovenes_3[,c(127:143,197)], 
                       demografia, 
                       by = c("KEY_U", "KEY"))


#Pregunta: Situaciones desmadrosas
# Cuestionario jovenes
# 4.6 De las situaciones descritas a continuación, dime por favor si...

P_situaciones <- jovenes_3[,c(1:5,157:193)]