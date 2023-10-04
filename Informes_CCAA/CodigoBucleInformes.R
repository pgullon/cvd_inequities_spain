library(tidyverse)
library(readxl)
library(ggplot2)


rm(list=ls())

setwd(paste0(getwd(),"/Informes_CCAA")) #Así nos carga la misma carpeta a todas, porque si no cada uno tiene su ruta específica

load("prevalencias_informes.RData")
load("rii_informes.RData")

# Creamos un objeto con las CCAA con los nombres para el bucle

ccaa <- data.frame(abreviatura = c("ES", "AN", "AR", "AS", "IB", "CN", "CB", "CM", "CL", "CT", "VC", "EX", "GA",
                                   "RI", "MD", "MC", "NC", "PV"),
                   nombre_ccaa = c("España", "Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                                   "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                                   "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                                   "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco"))

rii <- rii_informes %>% 
  left_join(ccaa)

prevalencias <- prevalencias %>% 
  left_join(ccaa)

save(prevalencias, file = "prevalencias_informes.RData")
save(rii, file = "RII_informes.RData")


## Creamos un objeto con las CCAA con los nombres para el bucle

nombre_ca <- c("España","Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                 "Cantabria", "Castilla y León", "Castilla-La Mancha", "Cataluña",
                 "Comunitat Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid",
                 "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco")

## Corremos el script de rmarkdown que contiene el bucle para que salgan los informes en html

for (v in 2:length(nombre_ca)) {
  
  print(paste0("Informe ", nombre_ca[v]))
  rmarkdown::render(input = paste0("Informe_madre.Rmd"),
                    output_dir = "html CCAA",
                    output_file = paste0("Informe_", nombre_ca[v], ".html"))
  
  print(paste0("PPT ", nombre_ca[v]))
  rmarkdown::render(input = paste0("ppt_figuras.Rmd"),
                    output_dir = "PPT CCAA",
                    output_file = paste0(nombre_ca[v], ".pptx"))
  
}
