library(eem)
library(inegiR)
library(dplyr)
library(tidyr)
library(ggplot2)

token <- "61c36253-47f6-c616-034e-7bf43b1aaba4"

# -- bajar data
sectores <- ultimos(inegiR::tasa_sectoresYoY(token), n = 13)
opiniones <- ultimos(inegiR::series_opiniones(token), n = 13) %>% 
  select(-c(Construcción,Manufacturas,Comercio))
# -- unir
df <- merge(sectores, opiniones, by = "Fechas") %>% 
  gather(data = ., key = "Fechas")
names(df) <- c("Fechas", "Variable", "YoY")
df$Grupo <- c(rep("Crecimiento",3*12),
              rep("Opinion",3*12))
# -- plot
ggplot(df, 
       aes(x = Fechas, 
           y = YoY, 
           group = Variable,
           colour = Grupo))+
  geom_path()+
  theme_eem()+
  scale_colour_eem(20)+
  labs(title = "Cosa de percepción",
       x = "Fechas", 
       y = "Tasa Crecimiento")