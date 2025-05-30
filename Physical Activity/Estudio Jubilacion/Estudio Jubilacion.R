# Autor: Luis Cereijo
# Proyecto: FIS Cardiodesigualdades
# Script análisis sobre Sedentarismo y Jubilación

## INDICE

# 1. Cargamos bases y homogeneizamos.
# 2. Hacemos análisis de desigualdad. 
# 3. Visualización gráfica de datos.
# 4. Análisis de desigualdades por Comunidades Autónomas.
# 5. Análisis prevalencias ponderadas en sedentarismo.
# 6. Análisis ICC prevalencias por Comunidad Autónoma


# Cargamos librerías

library(tidyverse)
library(plyr)
library(broom)
library(glmmTMB)
library(mixedup)
library(sf)
library(srvyr)
library(lpSolve)
library(irr)
library(scales)
library(segmented)



rm(list=ls())

select=dplyr::select

########################################################################
################### 1. CARGAMOS DATOS Y HOMOGENEIZAMOS #################
########################################################################


#Cargamos base de datos y homogeneizamos con el resto del proyecto

load("joined_dta.RData")


#Añadimos datos de ocupación

load("2001/ense2001.RData")

ense_2001 <- ense_2001 %>% 
  mutate(id=NCUEST) %>% 
  select()

#### ENSE 2017 ####

load("2017/ense2017.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2017 <-  ense_2017 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO),
         factor2=rescale(factor, to=c(1,100)),
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         education=na_if(NIVEST, "98"), education=as.numeric(na_if(education, "99")), 
         education_3=case_when((education==2 | education==3 | education==4)~1, 
                               (education==5 | education==6 | education==7 | education==8)~2, 
                               (education==9)~3),
         education_3_tr=((cume_dist(education_3)-1))*-1,
         education_5=case_when((education==2 | education==3)~1,
                               education==4~2,
                               (education==5 | education==7)~3,
                               (education==6 | education==8)~4, 
                               (education==9)~5),
         education_5_tr=((cume_dist(education_5)-1))*-1,
         diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, 
                            (G25a_12==2 | G25c_12==2) ~ 0), 
         hta=case_when((G25a_1==1 |G25c_1==1) ~ 1, 
                       (G25a_1==2 | G25c_1==2) ~ 0),
         col=case_when((G25a_15==1 |G25c_15==1) ~ 1, 
                       (G25a_15==2 | G25c_15==2) ~ 0),
         peso=na_if(S110, "998"), peso=as.numeric(na_if(peso, "999")), 
         altura=na_if(S109, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((V121==1 | V121==2) ~ 1 , 
                           (V121==3 | V121==4) ~ 0), 
         alcohol=na_if(W127, "98"), alcohol=as.numeric(na_if(alcohol, "99")),
         alcohol=case_when((alcohol==1 | alcohol==2 | alcohol==3 | alcohol==4 
                            | alcohol==5 | alcohol==6 | alcohol==7)~1, 
                           (alcohol==8 | alcohol==9)~0), 
         sedentario=case_when(T112==1~1, 
                              (T112==2 | T112==3 | T112==4)~0), 
         fruta=na_if(U120_1, 9), fruta=na_if(fruta, 8), 
         verdura=na_if(U120_7, 9), verdura=na_if(verdura, 8), 
         fruta=case_when(fruta==1~0, 
                         (fruta==2 | fruta==3 | fruta==4 | fruta==5 | fruta==6)~1), 
         verdura=case_when(verdura==1~0, 
                           (verdura==2 | verdura==3  | verdura==4 | verdura==5 | verdura==6)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0),
         jubilado= na_if(ACTIVa, 7), jubilado=na_if(ACTIVa, 8),
         jubilado= case_when((ACTIVa==1 | ACTIVa==6)~0,
                            ACTIVa==3~1,
                            (ACTIVa==2 | ACTIVa==4 | ACTIVa==5)~2) %>%
  select(id, factor, factor2, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura, ACTIVa, jubilado) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2017))


#### ENSE 2020 ####

load("2020/eese2020.RData")

eese_2020 <- eese_2020 %>% 
  select()



#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

#Duplicamos edad para tabla 1

dta <- dta %>% 
  mutate(edad_pura = edad)

#Centramos edad
dta <- dta %>%
  mutate(edad=scale(edad, center=T, scale=F))

#Eliminamos datos de 2009 porque no preguntan bien por sedentarismo
dta <- dta %>%
  filter(encuesta != "2009")

rii_sedentario_trabajadores <- ense_2017 %>%
  filter(jubilado==0) %>% 
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  dplyr::select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Sedentarismo", 
         sexo="Overall",
         situacion="No jubilados")

rii_sedentario_jubilados <- ense_2017 %>%
  filter(jubilado==1) %>% 
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  dplyr::select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Sedentarismo", 
         sexo="Overall",
         situacion="Jubilados")

rii_sedentario_jubilacion <- rii_sedentario_trabajadores %>% 
  rbind(rii_sedentario_jubilados)

jubilado <- glm(sedentario~jubilado+edad+sexo, data=ense_2017, family = "poisson")
summary(jubilado)

tidy(jubilado)

resultados_jubilado <- tidy(jubilado) %>%  
  mutate(PR=exp(estimate),
         infci=exp(estimate-1.96*std.error),
         supci=exp(estimate+1.96*std.error)) %>% 
  select(term, PR, infci, supci)

rii_sedentario_jubilados <- glm(sedentario~education_3_tr+edad+sexo, data=subset(dta, edad_pura>65 & encuesta==2020), family="poisson")

summary(rii_sedentario_jubilados)

rii_sedentario_trabajadores <- glm(sedentario~education_3_tr+edad+sexo, data=subset(dta, edad_pura<66 & encuesta==2020), family="poisson")

summary(rii_sedentario_trabajadores)

prevalencias_peso_sedentario_jub <- ense_2017 %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(jubilado, encuesta) %>%
  summarize(poblacion = survey_total(sedentario, na.rm = T, 
                                     vartype = c("ci"),
                                     level = 0.95,) , 
            sedentario = survey_mean(sedentario, na.rm = T, vartype = "ci"),
  ) %>%
  mutate(sex="Overall")
