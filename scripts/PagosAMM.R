# Minando datos de pagos y proveedores proporcionados por ¿alcalde como vamos?
# set-up
library(ckanr)
ckanr_setup(url = "http://datamx.io")

# función para facilitar la vida...
Descargar<-function(url){
  res<- ckanr::resource_show(id = url, as = "table")
  as.data.frame(ckanr::fetch(res$url))
}

# Monterrey
mty_pagos_201402<-Descargar("a592221a-3543-4488-9f12-b3a3354bdff6")
mty_pagos_201308<-Descargar("fa62a9a3-2129-405c-be31-9637461f5086")
mty_proveedores<-Descargar("c163726a-b6d5-4e09-bdad-89275df247be")

# San Pedro
spgg_pagos_2013<-Descargar("705f401b-81a4-4a6d-8ce7-ba7c8eb118dc")
