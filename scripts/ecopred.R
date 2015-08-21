# setwd - no read
folder<-"C:/Users/eduardo.lomas/Downloads/ECOPRED/ecopred14_bd"
setwd(folder)

# bibliotecas
library(foreign) #para leer dbf
library(dplyr) #para datos
library(ggplot2) #graphs
library(eem) #graphs [devtools::install_github("eflores89/eem")]

viviendas <- read.dbf("tviviendas.dbf") 
demografia <- read.dbf("tsdem.dbf") 
jefes <- read.dbf("tjefehogar.dbf")
jovenes_1 <- read.dbf("tjovenes_i_ii.dbf") 
jovenes_3 <- read.dbf("tjovenes_iii_iv.dbf")
jovenes_5 <- read.dbf("tjovenes_v.dbf")
jovenes_6 <- read.dbf("tjovenes_vi.dbf")
jovenes_7 <- read.dbf("tjovenes_vii_viii.dbf")

#IMPORTANTE; 
# Tener cuidado, por que uso indices de columnas para extraer data sets, 
# si la fuente origen cambia esto ya no es válido

#---------------------------------------------------------------------

#Pregunta: tus amigos y su desmadre...
# Cuestionario jovenes
# 4.3 Piensa en tus mejores amigos o los compañeros con los que más convives. 
# De las situaciones descritas a continuación, dime por favor
# ¿si enlo que va del año alguno de ellos...

desmadres <- jovenes_3[,c(1:5,127:143)]


#Pregunta: Situaciones desmadrosas
# Cuestionario jovenes
# 4.6 De las situaciones descritas a continuación, dime por favor si...

situaciones <- jovenes_3[,c(1:5,157:193)]

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

