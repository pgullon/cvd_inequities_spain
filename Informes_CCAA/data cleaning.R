library(tidyverse)
library(readxl)
library(ggplot2)

###Cargamos las dos bases de datos originales

desigualdades_rii_spain <- read.csv("rii_spain.csv")
desigualdades_rii_ccaa <- read.csv("rii_ccaa.csv")
prevalencias_spain <- read.csv("prevalencias_spain.csv")
prevalencias_ccaa <- read.csv("prevalencias_ccaa.csv")


#Homogeneizamos y creamos base de datos conjunta de RII para informes

desigualdades_rii_spain <- desigualdades_rii_spain %>% 
  rename(fr=risk_factor) %>% 
  select(-c(X)) %>% 
  filter(encuesta!="2009") %>% 
  mutate(abreviatura="ES",
         nombre_notilde="Espana",
         fr = recode(fr, "sedentario"="sedentarismo"))

desigualdades_rii_ccaa <- desigualdades_rii_ccaa %>% 
  select(-c(X, id_mapa, nombre)) %>%
  mutate(encuesta = recode(encuesta, "2001-01-01"="2001", "2003-01-01"="2003", "2006-01-01"="2006", "2011-01-01"="2011", "2014-01-01"="2014", "2017-01-01"="2017", "2020-01-01"="2020"),
         fr = recode(fr, "sedentario"="sedentarismo")) %>% 
  filter(encuesta!="2009-01-01")

rii <- desigualdades_rii_spain %>% 
  rbind(desigualdades_rii_ccaa) %>% 
  filter(encuesta!=2001) %>% 
  filter(encuesta!=2009) %>% 
  mutate(fr=recode(fr, fruta_verdura="food"), #Esto sería bueno llevarlo al código del proyecto para homogeneizar términos en todo el proyecto
         sexo=recode(sexo, overall="Global"),
         sexo=recode(sexo, Overall="Global")) #Esto sería bueno llevarlo al código del proyecto para homogeneizar términos en todo el proyecto

rii$ccaa <- as.factor(rii$ccaa)

#Hacemos lo mismo con la base de prevalencias: homogeneizamos y fusionamos

prevalencias_spain <- prevalencias_spain %>% 
  select(-c(X)) %>% 
  mutate(sexo=(case_when(sexo==0~"Mujeres", sexo==1~"Hombres", sexo=="Overall"~"Global")),
         abreviatura="ES",
         nombre_notilde="Espana",
         ccaa=0)

prevalencias_ccaa <- prevalencias_ccaa %>% 
  select(-c(X, id_mapa, nombre)) %>% 
  mutate(sexo=(case_when(sexo==0~"Mujeres", sexo==1~"Hombres", sexo=="Overall"~"Global")))

prevalencias <- prevalencias_spain %>% 
  rbind(prevalencias_ccaa) %>% 
  rename(Sedentarismo=sedentario,
         sedentarismo_low=sedentario_low,
         sedentarismo_upp=sedentario_upp)

prevalencias$ccaa <- as.factor(prevalencias$ccaa)

prevalencias <- prevalencias %>% 
  filter(encuesta != 2009) %>% 
  filter(encuesta != 2001)

save(prevalencias, file = "prevalencias_informes.RData")

rm(ccaa_nombres, prevalencias_ccaa, prevalencias_spain, sedentarismo_prevalencias, sedentarismo_rii)

