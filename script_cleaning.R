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




############################ ENSE 2001 ###################################


load("2001/ense2001.RData")

# Creamos las variables y nos quedamos solo con las variables de interés. 
#ESPERAR A EXTRAER DE DATOS DE ISABEL
# NO PAÍS DE NACIMIENTO. NO NACIONALIDAD. CLASE CNO
ense_2001 <-  ense_2001 %>% 
  mutate(edad=as.numeric(EDAD_i), 
         sexo=as.numeric(SEXO_i), 
         ccaa=as.numeric(na_if(CCAA, 18)),
         #migration=as.numeric(case_when(PAIS==724~1, B2!=724~2)),
         #nacionalidad=case_when(NACION==1 ~1, NACION==6~2),
         #clase=na_if(SPCLASE.x, 6), clase=na_if(clase, 9),
         #clase_tr=((cume_dist(clase)-1))*-1,
         education=p64a, 
         education_tr=((cume_dist(education)-1))*-1,
         income=as.numeric(na_if(p69, 9)),
         income_tr=((cume_dist(income)-1))*-1,
         nino=case_when(NINO==51~1, NINO==52~2, NINO==53~3, 
                        is.na(NINO)~0),
         home_n=as.numeric(ADULTO)+as.numeric(nino),
         diabetes=case_when(DIABETES==1 ~ 1, 
                            DIABETES==6 ~ 0), 
         hta=case_when(TENSION==1 ~ 1, 
                       TENSION==6 ~ 0),
         col=case_when(COLESTER==1 ~ 1, 
                       COLESTER==6 ~ 0),
         peso=na_if(PESO, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(ALTURA, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((FUMA==1 | FUMA==2) ~ 1 , 
                           (FUMA==3 | FUMA==4) ~ 0)) %>%
  select(NIDENTIF, FACTOR.x, edad, sexo, ccaa, nacionalidad, clase, clase_tr,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit()



############################ ENSE 2003 ###################################
load("2003/ense2003.RData")

# Creamos las variables y nos quedamos solo con las variables de interés. 
# NO PAÍS DE NACIMIENTO. ALCOHOL RARO
ense_2003 <-  ense_2003 %>% 
  mutate(id=NIDENTIF, 
         factor=FACTOR.x, 
         edad=as.numeric(EDAD.x), 
         sexo=as.numeric(SEXO.x), 
         ccaa=as.numeric(na_if(CCAA.x, 18)),
         #migration=as.numeric(case_when(PAIS==724~1, B2!=724~2)),
         nacionalidad=case_when(NACION==1 ~1, NACION==6~2),
         clase=na_if(SPCLASE.x, 7), clase=na_if(clase, 9),
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=SPESTUDI.x, 
         education_tr=((cume_dist(education)-1))*-1,
         income=as.numeric(na_if(IN_MENS, 9)),
         income_tr=((cume_dist(income)-1))*-1,
         nino=case_when(NINO==51~1, NINO==52~2, NINO==53~3, 
                        is.na(NINO)~0),
         home_n=as.numeric(ADULTO)+as.numeric(nino),
         diabetes=case_when(DIABETES==1 ~ 1, 
                          DIABETES==6 ~ 0), 
         hta=case_when(TENSION==1 ~ 1, 
                        TENSION==6 ~ 0),
         col=case_when(COLESTER==1 ~ 1, 
                       COLESTER==6 ~ 0),
         peso=na_if(PESO, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(ALTURA, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((FUMA==1 | FUMA==2) ~ 1 , 
                           (FUMA==3 | FUMA==4) ~ 0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2003, migration=NA)


save(ense_2003, file = "2003/ense2003_clean.RData")


############################ ENSE 2006 ###################################
load("2006/ense2006.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2006 <-  ense_2006 %>% 
  mutate(id=NIDENTIF, 
         factor=FACTOR.x,edad=as.numeric(EDAD.x), 
         sexo=as.numeric(SEXO.x), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=as.numeric(case_when(B2==724~1, B2!=724~2)),
         nacionalidad=case_when((B3==1 |B3==3) ~1, B3==2~2),
         clase=na_if(SPCLASE.x, 9),
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=na_if(A12, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=as.numeric(na_if(E3, 9)),
         income_tr=((cume_dist(income)-1))*-1,
         home_n=as.numeric(A9_1.y)+as.numeric(A9_2.y),
         diabetes=case_when((B15_11A==1 |B15_11C==1) ~ 1, 
                            (B15_11A==6 | B15_11C==6) ~ 0), 
         hta=case_when((B15_1A==1 |B15_1C==1) ~ 1, 
                       (B15_1A==6 | B15_1C==6) ~ 0),
         col=case_when((B15_14A==1 |B15_14C==1) ~ 1, 
                       (B15_14A==6 | B15_14C==6) ~ 0),
         peso=na_if(L1_128, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(L1_129, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((H1_67==1 | H1_67==2) ~ 1 , 
                           (H1_67==3 | H1_67==4) ~ 0), 
         alcohol=na_if(H2_81, 8), alcohol=as.numeric(na_if(alcohol, 9))) %>%
  select(id, factor, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2006)


save(ense_2006, file = "2006/ense2006_clean.RData")



############################ EESE 2009 ###################################
######################### NO USAR TODAVÍA #################################


load("2009/eese2009.RData")

# Creamos las variables y nos quedamos solo con las variables de interés. 
# NO CLASE, NO PERSONAS POR HOGAR, NO COLESTEROL. renta muchos perdidos
eese_2009 <-  eese_2009 %>% 
  mutate(id=IDENTHOGAR, 
         factor=FACTORADULTO,
         edad=as.numeric(EDAD), 
         sexo=as.numeric(SEXO), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(HH9_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(HH10_1a==1~1, HH10_1a==6~2),
         education=na_if(HH13, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=as.numeric(na_if(IN3.y, 98)),
         income_tr=((cume_dist(income)-1))*-1,
         diabetes=case_when((HS4_11==1 |HS5_11==1) ~ 1, 
                            (HS4_11==6 | HS5_11==6) ~ 0), 
         hta=case_when((HS4_5==1 | HS5_5==1) ~ 1, 
                       (HS4_5==6 | HS5_5==6) ~ 0),
         peso=na_if(BMI2, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(BMI1, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when((IMC==1 | IMC==2 | IMC==3) ~0, 
                           IMC==4~1),
         sobrepeso=case_when((IMC==1 | IMC==2) ~0, 
                             (IMC==4 | IMC==3)~1),
         smoking=case_when((SK1==1 | SK1==2) ~ 1 , 
                           (SK1==3 | SK1==4) ~ 0), 
         alcohol=na_if(AL1, 8), alcohol=as.numeric(na_if(alcohol, 9))) %>%
  select(id, factor, edad, sexo, ccaa, migration, nacionalidad,
         education, education_tr, diabetes, hta, 
         imc, obesity, sobrepeso, smoking, alcohol) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2009)




############################ ENSE 2011 ###################################
load("2011/ense2011.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2011 <-  ense_2011 %>% 
  mutate(id=IDENTHOGAR, 
         factor=FACTORADULTO, 
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(E2_1a==1~1, E2_1a==6~2),
         clase=na_if(CLASE_PR.x, 9),
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=na_if(A10_i, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=na_if(D28, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((cume_dist(income)-1))*-1,
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
  select(id, factor, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2011)

save(ense_2011, file = "2011/ense2011_clean.RData")


############################ EESE 2014 ###################################
load("2014/eese2014.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2014 <-  eese_2014 %>% 
  mutate(id=IDENTHOGAR, 
         factor=FACTORADULTO,
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=na_if(ESTUDIOS, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=na_if(D26, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((cume_dist(income)-1))*-1,
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
  select(id, factor, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2014)

save(eese_2014, file = "2014/eese2014_clean.RData")

############################ ENSE 2017 ###################################
load("2017/ense2017.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2017 <-  ense_2017 %>% 
  mutate(id=IDENTHOGAR, 
         factor=FACTORADULTO,
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=na_if(NIVEST, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=na_if(D29, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((cume_dist(income)-1))*-1,
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
  select(id,factor, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2017)


save(ense_2017, file = "2017/ense2017_clean.RData")


############################ EESE 2020 ###################################
load("2020/eese2020.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2020 <-  eese_2020 %>% 
  mutate(id=IDENTHOGAR, 
         factor=FACTORADULTO,
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         ccaa=na_if(CCAA.x, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         clase2=case_when((clase==1 | clase==2 | clase==3)~0,
                          (clase==4 | clase==5 | clase==6)~1),
         education=na_if(ESTUDIOS, 98), education=as.numeric(na_if(education, 99)), 
         education_tr=((cume_dist(education)-1))*-1,
         income=na_if(D26, 98), income=as.numeric(na_if(income, 99)),
         income_tr=((cume_dist(income)-1))*-1,
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
  select(id, factor, edad, sexo, ccaa, migration, nacionalidad, clase, clase_tr, clase2,
         education, education_tr, income, income_tr, home_n, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking) %>%
  filter(edad>17) %>%
  distinct() %>%
  na.omit() %>%
  mutate(encuesta=2020)
         

save(eese_2020, file = "2020/eese2020_clean.RData")


dta <- ense_2003 %>%
  rbind(ense_2006) %>%
  rbind(ense_2011) %>%
  rbind(eese_2014) %>%
  rbind(ense_2017) %>%
  rbind(eese_2020)

save(dta, file = "joined_dta.RData")
