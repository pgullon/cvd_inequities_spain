#This file is for data cleaning and harmonization of the data
library(tidyverse)
library(scales)


rm(list=ls())


# Nombres comunes para las variables en todas las encuestas
# Edad=age
# Sexo=sex
# Comunidad Autónoma=ccaa
# País de nacimiento=migration
# Nacionalidad española=nacionalidad
# Clase social=clase
# Clase social transformada 0-1 (0 baja y 1 alta)=clase_tr
# Nivel educativo=education
# Nivel educativo transformado 0-1=education_tr
# Renta del hoga=income
# Renta del hogar transformada 0-1=income_tr
# Personas por hogar=home_n
# Diabetes=diabetes
# Hipertensión=hta
# Hipercolesterolemia=col
# IMC (continua) =imc
# Obesidad=obesity
# Sobrepeso+obesidad=sobrepeso
# Tabaquismo=smoking
# Consumo de alcohol=alcohol
# Actividad física=activity
# Alimentación=food
# Faltan todavía calcular las de alimentación y actividad física



############################ EESE 2009 ###################################
load("2009/eese2009.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2009 <-  eese_2009 %>% 
  mutate(edad=as.numeric(EDAD), 
         sexo=as.numeric(SEXO), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(HH9_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(HH10_1a==1~1, E2_1a==6~2),
         clase=na_if(CLASE_PR.x, 9),
         clase_tr=((rescale(clase)-1))*-1,
         education=na_if(A10_i, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((rescale(education)-1))*-1,
         income=na_if(D28, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((rescale(income)-1))*-1,
         home_n=as.numeric(NADULTOS)+as.numeric(NMENORES),
         diabetes=case_when((G21a_11==1 |G21c_11==1) ~ 1, 
                            (G21a_11==6 | G21c_11==6) ~ 0), 
         hta=case_when((G21a_1==1 |G21c_1==1) ~ 1, 
                       (G21a_1==6 | G21c_1==6) ~ 0),
         col=case_when((G21a_14==1 |G21c_14==1) ~ 1, 
                       (G21a_14==6 | G21c_11==6) ~ 0),
         peso=na_if(R102, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(R103, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMCa==1 | IMCa==2 | IMCa==3) ~0, 
                           IMCa==4~1),
         sobrepeso=case_when((IMCa==1 | IMCa==2) ~0, 
                             (IMCa==4 | IMCa==3)~1),
         smoking=case_when((S105==1 | S105==2) ~ 1 , 
                           (S105==3 | S105==4) ~ 0), 
         alcohol=na_if(T120, 8), alcohol=as.numeric(na_if(alcohol, 9))) %>%
  select(IDENTHOGAR, FACTORADULTO, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()




############################ ENSE 2011 ###################################
load("2011/ense2011.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2011 <-  ense_2011 %>% 
  mutate(edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(E2_1a==1~1, E2_1a==6~2),
         clase=na_if(CLASE_PR.x, 9),
         clase_tr=((rescale(clase)-1))*-1,
         education=na_if(A10_i, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((rescale(education)-1))*-1,
         income=na_if(D28, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((rescale(income)-1))*-1,
         home_n=as.numeric(NADULTOS)+as.numeric(NMENORES),
         diabetes=case_when((G21a_11==1 |G21c_11==1) ~ 1, 
                            (G21a_11==6 | G21c_11==6) ~ 0), 
         hta=case_when((G21a_1==1 |G21c_1==1) ~ 1, 
                       (G21a_1==6 | G21c_1==6) ~ 0),
         col=case_when((G21a_14==1 |G21c_14==1) ~ 1, 
                       (G21a_14==6 | G21c_11==6) ~ 0),
         peso=na_if(R102, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(R103, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMCa==1 | IMCa==2 | IMCa==3) ~0, 
                           IMCa==4~1),
         sobrepeso=case_when((IMCa==1 | IMCa==2) ~0, 
                             (IMCa==4 | IMCa==3)~1),
         smoking=case_when((S105==1 | S105==2) ~ 1 , 
                           (S105==3 | S105==4) ~ 0), 
         alcohol=na_if(T120, 8), alcohol=as.numeric(na_if(alcohol, 9))) %>%
  select(IDENTHOGAR, FACTORADULTO, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()




############################ EESE 2014 ###################################
load("2014/eese2014.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2014 <-  eese_2014 %>% 
  mutate(edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=((rescale(clase)-1))*-1,
         education=na_if(ESTUDIOS, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((rescale(education)-1))*-1,
         income=na_if(D26, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((rescale(income)-1))*-1,
         home_n=as.numeric(NADULTOS)+as.numeric(NMENORES),
         diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, 
                            (G25a_12==2 | G25c_12==2) ~ 0), 
         hta=case_when((G25a_1==1 |G25c_1==1) ~ 1, 
                       (G25a_1==2 | G25c_1==2) ~ 0),
         col=case_when((G25a_15==1 |G25c_15==1) ~ 1, 
                       (G25a_15==2 | G25c_15==2) ~ 0),
         peso=na_if(S110, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(S109, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMC==1 | IMC==2 | IMC==3) ~0, 
                           IMC==4~1),
         sobrepeso=case_when((IMC==1 | IMC==2) ~0, 
                             (IMC==4 | IMC==3)~1),
         smoking=case_when((V121==1 | V121==2) ~ 1 , 
                           (V121==3 | V121==4) ~ 0), 
         alcohol=na_if(W127, 98), alcohol=as.numeric(na_if(alcohol, 99))) %>%
  select(IDENTHOGAR, FACTORADULTO, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()



############################ ENSE 2017 ###################################
load("2017/ense2017.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2017 <-  ense_2017 %>% 
  mutate(edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=((rescale(clase)-1))*-1,
         education=na_if(NIVEST, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((rescale(education)-1))*-1,
         income=na_if(D29, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((rescale(income)-1))*-1,
         home_n=as.numeric(NADULTOS)+as.numeric(NMENORES),
         diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, 
                            (G25a_12==2 | G25c_12==2) ~ 0), 
         hta=case_when((G25a_1==1 |G25c_1==1) ~ 1, 
                       (G25a_1==2 | G25c_1==2) ~ 0),
         col=case_when((G25a_15==1 |G25c_15==1) ~ 1, 
                       (G25a_15==2 | G25c_15==2) ~ 0),
         peso=na_if(S110, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(S109, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMCa==1 | IMCa==2 | IMCa==3) ~0, 
                           IMCa==4~1),
         sobrepeso=case_when((IMCa==1 | IMCa==2) ~0, 
                             (IMCa==4 | IMCa==3)~1),
         smoking=case_when((V121==1 | V121==2) ~ 1 , 
                           (V121==3 | V121==4) ~ 0), 
         alcohol=na_if(W127, 98), alcohol=as.numeric(na_if(alcohol, 99))) %>%
  select(IDENTHOGAR,FACTORADULTO, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()





############################ EESE 2020 ###################################
load("2020/eese2020.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2020 <-  eese_2020 %>% 
  mutate(edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=((rescale(clase)-1))*-1,
         education=na_if(ESTUDIOS, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((rescale(education)-1))*-1,
         income=na_if(D26, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((rescale(income)-1))*-1,
         home_n=as.numeric(NADULTOS)+as.numeric(NMENORES),
         diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, 
                            (G25a_12==2 | G25c_12==2) ~ 0), 
         hta=case_when((G25a_1==1 |G25c_1==1) ~ 1, 
                            (G25a_1==2 | G25c_1==2) ~ 0),
         col=case_when((G25a_15==1 |G25c_15==1) ~ 1, 
                            (G25a_15==2 | G25c_15==2) ~ 0),
         peso=na_if(S110, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(S109, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMC==1 | IMC==2 | IMC==3) ~0, 
                           IMC==4~1),
         sobrepeso=case_when((IMC==1 | IMC==2) ~0, 
                           (IMC==4 | IMC==3)~1),
         smoking=case_when((V121==1 | V121==2) ~ 1 , 
                           (V121==3 | V121==4) ~ 0), 
         alcohol=na_if(W127, 98), alcohol=as.numeric(na_if(alcohol, 99))) %>%
  select(IDENTHOGAR, FACTORADULTO, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()
         



