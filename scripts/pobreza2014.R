library(XLConnect) # para importar excel
library(plyr) # para loopear listas
library(reshape2) # para melt para graficas 
library(ggplot2) #plots
library(eem) # para formato graficas [install_github("eflores89/eem")]
library(inegiR) #para indicadores de crecimiento (cargar token) [install_github("eflores89/inegiR")]

# cargando la info de pobreza - solamente el primer cuadro de cada estado.
wb<-loadWorkbook("Anexo estadístico_entidades_2010-2014.xls")
l<-getSheets(wb)
l<-l[l!=c("Contenido","Estados Unidos Mexicanos")]

####### pobreza
estados_pobreza<-lapply(l, function(x){
                                       readWorksheet(wb,
                                       x,
                                       # importar del rango: B11:E16
                                       startRow = 10, endRow = 16,
                                       startCol = 2, endCol = 5)
                                       })

  #arreglar con nombres, poner en un data frame por año, para manipular
  nombres<-as.character(ldply(.data = estados_pobreza, .fun = "[[",1)[1,])
    
    #2010
    d1<-as.data.frame(ldply(.data = estados_pobreza, .fun = "[[",2))
    names(d1)<-nombres
    d1$Estado<-l
    d1$anio<-"2010"

    #2012
    d2<-as.data.frame(ldply(.data = estados_pobreza, .fun = "[[",3))
    names(d2)<-nombres
    d2$Estado<-l
    d2$anio<-"2012"
    
    #2014
    d3<-as.data.frame(ldply(.data = estados_pobreza, .fun = "[[",4))
    names(d3)<-nombres
    d3$Estado<-l
    d3$anio<-"2014"

#unir a los indicadores de pobreza en un set transversal y otro tidy
estados_transversal_pobreza<-rbind(d1,d2,d3)
estados_tidy_pobreza<-melt(estados_transversal_pobreza)
names(estados_tidy_pobreza)<-c("Estado", "Anio", "TipoPobreza", "Porcentaje")

####### carencias
estados_carencias<-lapply(l, function(x){
                                      readWorksheet(wb,
                                      x,
                                      # importar del rango: B21:E26
                                      startRow = 20, endRow = 26,
                                      startCol = 2, endCol = 5)
                                      })
  #2010
  d4<-as.data.frame(ldply(.data = estados_carencias, .fun = "[[",2))
  names(d4)<-as.character(ldply(.data = estados_carencias, .fun = "[[",1)[1,])
  d4$Estado<-l
  d4$anio<-"2010"

  #2012
  d5<-as.data.frame(ldply(.data = estados_carencias, .fun = "[[",3))
  names(d5)<-as.character(ldply(.data = estados_carencias, .fun = "[[",1)[1,])
  d5$Estado<-l
  d5$anio<-"2012"

  #2014
  d6<-as.data.frame(ldply(.data = estados_carencias, .fun = "[[",4))
  names(d6)<-as.character(ldply(.data = estados_carencias, .fun = "[[",1)[1,])
  d6$Estado<-l
  d6$anio<-"2014"

#unir a los indicadores de carencias en un set transversal y otro tidy
estados_transversal_carencias<-rbind(d4,d5,d6)
estados_tidy_carencias<-melt(estados_transversal_carencias)
names(estados_tidy_carencias)<-c("Estado", "Anio", "TipoCarencia", "Porcentaje")

####### poblacion
estados_poblacion <-lapply(l, function(x){
                                          readWorksheet(wb,
                                          x,
                                          startRow = 10, endRow = 16,
                                          startCol = 7, endCol = 9)
                                          })
  #2010
  d7<-as.data.frame(ldply(.data = estados_poblacion, .fun = "[[",1))
  names(d7)<-as.character(ldply(.data = estados_pobreza, .fun = "[[",1)[1,])
  d7$Estado<-l
  d7$anio<-"2010"
                      
  #2012
  d8<-as.data.frame(ldply(.data = estados_poblacion, .fun = "[[",2))
  names(d8)<-as.character(ldply(.data = estados_pobreza, .fun = "[[",1)[1,])
  d8$Estado<-l
  d8$anio<-"2012"
                          
  #2014
  d9<-as.data.frame(ldply(.data = estados_poblacion, .fun = "[[",3))
  names(d9)<-as.character(ldply(.data = estados_pobreza, .fun = "[[",1)[1,])
  d9$Estado<-l
  d9$anio<-"2014"

#unir a los indicadores de población en un set transversal y otro tidy
estados_transversal_poblacion<-rbind(d7,d8,d9)
estados_tidy_poblacion<-melt(estados_transversal_poblacion)
names(estados_tidy_poblacion)<-c("Estado", "Anio", "TipoPobreza", "Miles_Hab")

  ################################################
  ############                    GRAFICAS #######
  ################################################
  
  ### primero, algunas gráficas para observar cambios...
  
  ggplot(data = subset(
                      subset(estados_tidy_pobreza, Anio == "2010" | Anio == "2014"),
                      TipoPobreza == unique(estados_tidy_pobreza$TipoPobreza)[1]),
                aes(x = Anio, y = Porcentaje, colour = Estado, group = Estado )) +
    geom_path() +
    labs(title = "Cambios en Proporción de Pobreza (Total)", x ="Año", y = "Proporción")+
    theme_eem()+
    theme(legend.position = "none")
  
  ggplot(data = subset(
                      subset(estados_tidy_pobreza, Anio == "2010" | Anio == "2014"),
                      TipoPobreza == unique(estados_tidy_pobreza$TipoPobreza)[2]),
                aes(x = Anio, y = Porcentaje, colour = Estado, group = Estado )) +
    geom_path() +
    labs(title = "Cambios en Proporción de Pobreza Moderada", x ="Año", y = "Proporción")
  
  ggplot(data = subset(
                      subset(estados_tidy_pobreza, Anio == "2010" | Anio == "2014"),
                      TipoPobreza == unique(estados_tidy_pobreza$TipoPobreza)[3]),
                  aes(x = Anio, y = Porcentaje, colour = Estado, group = Estado )) +
    geom_path() +
    labs(title = "Cambios en Proporción de Pobreza Extrema", x ="Año", y = "Proporción")+
    theme_eem()+
    theme(legend.position="none")
  
  ggplot(data = subset(
                      subset(estados_tidy_pobreza, Anio == "2010" | Anio == "2014"),
                      TipoPobreza == unique(estados_tidy_pobreza$TipoPobreza)[6]),
                aes(x = Anio, y = Porcentaje, colour = Estado, group = Estado )) +
    geom_path() +
    labs(title = "Cambios en Proporción de Población \n No Vulnerable", x="Año", y= "Proporción")+
    theme_eem()+
    theme(legend.position="none")
  
  ### Una gráfica sencilla de las entidades con más pobreza
  ggplot(data = subset(
                      subset(estados_tidy_pobreza, Anio == "2014"),
                      TipoPobreza == unique(estados_tidy_pobreza$TipoPobreza)[1]),
                aes(x = Estado, y = Porcentaje)) +
    geom_bar(stat = "identity", fill = "#A84A44") +
    labs(title = "Pobreza Total por Estado", x ="Estado", y = "Proporción")+
    theme_eem()

  ### Que estados bajaron más sus tasas como % de la población entre 2010 y 2014?
        d<-subset(
          subset(estados_tidy_pobreza, Anio == "2014"),
          TipoPobreza==unique(estados_tidy_pobreza$TipoPobreza)[1])
        c<-subset(
          subset(estados_tidy_pobreza, Anio == "2010" ),
          TipoPobreza==unique(estados_tidy_pobreza$TipoPobreza)[1])

  estados_progreso<-data.frame("Estado" = d$Estado, 
                               "Diferencia" = d$Porcentaje - c$Porcentaje)
        rm(d,c) #delete
  ggplot(data = estados_progreso,
                aes(x = Estado, y = Diferencia)) +
          geom_bar(stat = "identity", fill = "#A84A44") +
          labs(title = "Cambios en % de Pobreza (Total)", x = "Estado", y = "Delta (PP)")+
          theme_eem()

  ### que estados aportaron más pobres al total?
  d<-subset(
    subset(estados_tidy_poblacion, Anio == "2014"),
    TipoPobreza == unique(estados_tidy_poblacion$TipoPobreza)[1])
  c<-subset(
    subset(estados_tidy_poblacion, Anio == "2010" ),
    TipoPobreza == unique(estados_tidy_poblacion$TipoPobreza)[1])

  estados_aportacion<-data.frame("Estado" = d$Estado, 
                                "Miles_Hab" = d$Miles_Hab - c$Miles_Hab)
  rm(d,c) #delete

  ggplot(data = estados_aportacion,
        aes(x = Estado, y = Miles_Hab)) +
  geom_bar(stat = "identity", fill = "#A84A44") +
  labs(title = "Aportación a la Pobreza (2010 - 2014)", x ="Estado", y = "Miles de Personas")+
  theme_eem() 

  #### que pasa con esos estados! MX, Puebla, Michoacan y Morelos!!
  carencias_estadosmalos<-subset(estados_tidy_carencias, Estado == "México" | 
                                                          Estado == "Michoacán" | 
                                                          Estado == "Morelos" | 
                                                          Estado == "Puebla")
    # cambiando nombres para facilitar lectura de gràfica, traerme solamente la ultima palabra...
  carencias_estadosmalos$TipoCarencia <-unlist(lapply(carencias_estadosmalos$TipoCarencia, 
                                                      function(x){tail(strsplit(as.character(x) ,split=" ")[[1]],1)}))

  ggplot(data = carencias_estadosmalos, 
         aes(x = Anio, y = Porcentaje, colour = TipoCarencia, group = TipoCarencia))+
  geom_path()+
  facet_grid(. ~ Estado)+
  theme_eem() +
  labs(title = "Carencias por Estado", x = "Año", y = "Proporción Pob.") +
  scale_colour_manual(values = c("#A84A44","#E47D04","#D8A19E","#ae8b38",
                                 "#4d7c28"))

  ### Aprovechando lo anterior, vamos a gráficar la relación con el crecimiento ecónomico de cada entidad, en el mismo periodo.
  # me traigo tasas de crecimiento (2009 - 2013, no hay hasta 2014 por estado)

pib_estados <-ultimos(series_PIB_estados(token), n = 5)
tasas_crecimiento_promedio <-data.frame(
                              "Estado" = names(pib_estados[,2:length(pib_estados)]),
                              "TasaCrecimiento" = unlist(
                                                    lapply(
                                                      names(pib_estados[,2:length(pib_estados)]), 
                                                      function(x){
                                                          mean(YoY(pib_estados[,x], 1)[2:6])
                                                                  }))
                              )

      #comprobe visualmente: el orden de los estados es el mismo aunque no se llaman exactamente igual.

estados_crecimiento_vs_progreso<-cbind(tasas_crecimiento_promedio, estados_progreso)[,2:4]

ggplot(data = estados_crecimiento_vs_progreso,
       aes(x = TasaCrecimiento*100, y = Diferencia)) +
  geom_point() +
  stat_smooth() + 
  geom_text(aes(label=Estado),hjust = 0, vjust = 0)+
  labs(title = "Crecimiento vs. Pobreza", x = "Crecimiento PIB (Prom.)", y = "Delta (PP)") +
  theme_eem()

# quitamos estados con petróleo
estados_crecimiento_vs_progreso_2<-subset(estados_crecimiento_vs_progreso, Estado!="Campeche")
estados_crecimiento_vs_progreso_2<-subset(estados_crecimiento_vs_progreso_2, Estado!="Veracruz")
estados_crecimiento_vs_progreso_2<-subset(estados_crecimiento_vs_progreso_2, Estado!="Tabasco")
estados_crecimiento_vs_progreso_2<-subset(estados_crecimiento_vs_progreso_2, Estado!="Tamaulipas")

ggplot(data = estados_crecimiento_vs_progreso_2,
       aes(x = TasaCrecimiento*100, y = Diferencia)) +
  geom_point() +
  stat_smooth() + 
  stat_hline(yintercept = 0)+
  geom_text(aes(label=Estado),hjust = 0, vjust = 0)+
  labs(title = "Crecimiento vs. Pobreza", x="Crecimiento PIB (Prom.)", y= "Delta (PP)") +
  theme_eem()

###### que hubiera pasado si todos los estados hubieran distribuido el crecimiento igual que los que redujeron pobreza?
m<-lm(formula = Diferencia ~ TasaCrecimiento, 
      data =  subset(estados_crecimiento_vs_progreso, Diferencia<0))

antes<-subset(
        subset(estados_tidy_poblacion, Anio == "2010" ),
        TipoPobreza==unique(estados_tidy_poblacion$TipoPobreza)[1])[4]
real<-subset(
        subset(estados_tidy_poblacion, Anio == "2014"),
        TipoPobreza==unique(estados_tidy_poblacion$TipoPobreza)[1])[4]

el_hubiera<-data.frame("TasaCrecimiento" = estados_crecimiento_vs_progreso$TasaCrecimiento, 
                       "Diferencia_Pred" = predict(m, estados_crecimiento_vs_progreso['TasaCrecimiento']),
                       "Estado" = estados_crecimiento_vs_progreso$Estado,
                       "Pobres2010" = as.numeric(antes[,1]), 
                       "Pobres2014" = as.numeric(real[,1])
                       )
el_hubiera$reduccion_teoria<-el_hubiera$Pobres2010*100/(100-el_hubiera$Diferencia_Pred)
el_hubiera$aportacion_teoria<-el_hubiera$reduccion_teoria-el_hubiera$Pobres2010
    
    #comparativas
    ggplot(data = el_hubiera,
           aes(x = Estado, y = aportacion_teoria))+
      geom_bar(stat = "identity", fill = "#A84A44")+
      labs(title = "Teoría y Realidad", x = "Estado", y = "Reducción (Miles Hab.)")+
      theme_eem()

#tristemente
PobresEvitables<-sum(el_hubiera$Pobres2014) - sum(el_hubiera$reduccion_teoria)
