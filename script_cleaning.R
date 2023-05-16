#This file is for data cleaning and harmonization of the data
library(tidyr)
library(tidyverse)
library(scales)
library(labelled)


rm(list=ls())


# Nombres comunes para las variables en todas las encuestas
# Edad=age
# Sexo=sex (0 mujer, 1 hombre)
# Comunidad Autónoma=ccaa
# País de nacimiento=migration
# Nacionalidad española=nacionalidad
# Clase social=clase
# Clase social transformada 0-1 (0 baja y 1 alta)=clase_tr
# Nivel educativo en 3 categorías=education_3
# Nivel educativo 3 categoráis transformado 0-1=education_3_tr
# Nivel educativo en 5 categorías=education_5
# Nivel educativo 5 categoráis transformado 0-1=education_5_tr
# Diabetes=diabetes
# Hipertensión=hta
# Hipercolesterolemia=col
# IMC (continua) =imc
# Obesidad=obesity
# Sobrepeso+obesidad=sobrepeso
# Tabaquismo=smoking
# Consumo de alcohol=alcohol
# Sedentarismo=sedentario
# Alimentación=fruta (por ahora solo fruta y verdura diaria en dicotómico con 0 si consume) 



############################ ENSE 2001 ###################################


load("2001/ense2001.RData")
labelled::unlabelled(ense_2001)

# Creamos las variables y nos quedamos solo con las variables de interés. 
# NO PAÍS DE NACIMIENTO. NO NACIONALIDAD. NO ALCOHOL
# Como en la encuesta de 2001 no está clase, la calculamos nosotros en correspondencia CNO-79

clase_2001 <- read_delim("clase_2001.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

ense_2001 <-  ense_2001 %>% 
    left_join(clase_2001) %>%
  mutate(id=NCUEST,
         factor=as.numeric(FACTOR),
         edad=as.numeric(P4_01), 
         sexo=as.numeric(P3_01), sexo=na_if(sexo, 9), 
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA, 18), ccaa=as.numeric(na_if(ccaa, 19)),
         migration=NA, nacionalidad=NA,
         clase=na_if(cso12, 9),
         clase_tr=cume_dist(clase),
         education_3=case_when((P57A==0 | P57A==1 | P57A==2)~1, 
                               (P57A==3 | P57A==4 | P57A==5 | P57A==6)~2, 
                               (P57A==7 | P57A==8 | P57A==9 | P57A==10 | P57A==11
                                | P57A==12 | P57A==13)~3), 
         education_3_tr=((cume_dist(education_3)-1))*-1,
         education_5=case_when((P57A==0 | P57A==1)~1, 
                               (P57A==2)~2, 
                               (P57A==3 | P57A==4)~3, 
                               (P57A==5 | P57A==6)~4, 
                               (P57A==7 | P57==8 | P57==9 |P57==10 | 
                                  P57A==11 | P57A==12 | P57A==13)~5),
         education_5_tr=((cume_dist(education_5)-1))*-1,
         diabetes=case_when(P11_3==1 ~ 1, 
                            P11_3==2 ~ 0), 
         hta=case_when(P11_1==1 ~ 1, 
                       P11_1==2 ~ 0),
         col=case_when(P11_2==1 ~ 1, 
                       P11_2==2 ~ 0),
         peso=na_if(P45, 998), peso=as.numeric(na_if(peso, 999)), 
         altura=na_if(P46, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((P28==1 | P28==2) ~ 1 , 
                           (P28==3 | P28==4) ~ 0), 
         alcohol=NA,
         sedentario=case_when(P40==1~1, 
                              (P40==2 | P40==3 | P40==4)~0), 
         fruta=na_if(P42_1, 9), 
         verdura=na_if(P42_6, 9), 
         fruta=case_when(fruta==1~0, 
                         (fruta==2 | fruta==3 | fruta==4 | fruta==5)~1), 
         verdura=case_when(verdura==1~0, 
                           (verdura==2 | verdura==3  | verdura==4 | verdura==5)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, education_3, education_3_tr, 
          education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, sedentario, fruta_verdura) %>%
  mutate(encuesta=2001)

save(ense_2001, file = "2001/ense2001_clean.RData")

############################ ENSE 2003 ###################################
load("2003/ense2003.RData")

# Creamos las variables y nos quedamos solo con las variables de interés. 
# NO PAÍS DE NACIMIENTO. NO POSIBLE EDUCACIÓN NE 5 CATEGORÍAS
ense_2003 <-  ense_2003 %>% 
  mutate(id=NIDENTIF, 
         factor=as.numeric(FACTOR.x), 
         edad=as.numeric(EDAD.x), 
         sexo=as.numeric(SEXO.x),
         sexo=case_when(sexo==1~1, sexo==6~0),
         ccaa=as.numeric(na_if(CCAA.x, "18")),
         migration=NA,
         nacionalidad=case_when(NACION==1 ~1, NACION==6~2),
         clase=na_if(SPCLASE.x, 7), clase=na_if(clase, 9),
         clase_tr=cume_dist(clase),
         education_3=case_when((SPESTUDI.x==1 | SPESTUDI.x==2)~1,
                               SPESTUDI.x==3~2, 
                               SPESTUDI.x==4~3),
         education_3_tr=((cume_dist(education_3)-1))*-1,
         education_5=NA, education_5_tr=NA,
         diabetes=case_when(DIABETES==1 ~ 1, 
                          DIABETES==6 ~ 0), 
         hta=case_when(TENSION==1 ~ 1, 
                        TENSION==6 ~ 0),
         col=case_when(COLESTER==1 ~ 1, 
                       COLESTER==6 ~ 0),
         peso=na_if(PESO, "998"), peso=as.numeric(na_if(peso, "999")), 
         altura=na_if(ALTURA, 998), altura=as.numeric(na_if(altura, 999)),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((FUMA==1 | FUMA==2) ~ 1 , 
                           (FUMA==3 | FUMA==4) ~ 0), 
         alcohol=case_when(BEBE==1~1, 
                           BEBE==6~0), 
         sedentario=case_when(D_ACFISO==1~1, 
                              D_ACFISO==2~0), 
         fruta=case_when(FRUTA==1~0, 
                         (FRUTA==2 | FRUTA==3 | FRUTA==4 | FRUTA==5)~1), 
         verdura=case_when(VERDURA==1~0, 
                           (VERDURA==2 | VERDURA==3  | VERDURA==4 | VERDURA==5)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, clase, clase_tr,
          education_3, education_3_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2003)


save(ense_2003, file = "2003/ense2003_clean.RData")


############################ ENSE 2006 ###################################
load("2006/ense2006.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
# Alcohol en los últimos 12 meses tiene muchos missing
ense_2006 <-  ense_2006 %>% 
  mutate(id=NIDENTIF, 
         factor=as.numeric(FACTOR.x),
         edad=as.numeric(EDAD.x), 
         sexo=as.numeric(SEXO.x), 
         sexo=case_when(sexo==1~1, sexo==6~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=as.numeric(case_when(B2==724~1, B2!=724~2)),
         nacionalidad=case_when((B3==1 |B3==3) ~1, B3==2~2),
         clase=na_if(SPCLASE.x, 9),
         clase_tr=cume_dist(clase),
         education=na_if(A12, "98"), education=as.numeric(na_if(education, "99")),
         education_3=case_when((education==1 | education==2 | education==3)~1, 
                               (education==4 | education==5 | education==6 | education==7)~2, 
                               (education==8 | education==9)~3),
         education_3_tr=((cume_dist(education_3)-1))*-1,
         education_5=case_when((education==1 | education==2)~1, 
                               education==3~2,
                               (education==4 | education==5)~3,
                               (education==6 | education==7)~4,
                               (education==8 | education==9)~5),
         education_5_tr=((cume_dist(education_5)-1))*-1,
         diabetes=case_when((B15_11A==1 |B15_11C==1) ~ 1, 
                            (B15_11A==6 | B15_11C==6) ~ 0), 
         hta=case_when((B15_1A==1 |B15_1C==1) ~ 1, 
                       (B15_1A==6 | B15_1C==6) ~ 0),
         col=case_when((B15_14A==1 |B15_14C==1) ~ 1, 
                       (B15_14A==6 | B15_14C==6) ~ 0),
         peso=na_if(L1_128, "998"), peso=as.numeric(na_if(peso, "999")), 
         altura=na_if(L1_129, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((H1_67==1 | H1_67==2) ~ 1 , 
                           (H1_67==3 | H1_67==4) ~ 0), 
         alcohol=case_when(H2_82==1~1, 
                           H2_82==6~0), 
         sedentario=case_when(H3_93==6~1, 
                              H3_93==1~0), 
         fruta=na_if(H4_96_1, 9), 
         verdura=na_if(H4_96_7, 9), 
         fruta=case_when(fruta==1~0, 
                         (fruta==2 | fruta==3 | fruta==4 | fruta==5)~1), 
         verdura=case_when(verdura==1~0, 
                           (verdura==2 | verdura==3  | verdura==4 | verdura==5)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, sedentario, fruta_verdura) %>%
  mutate(encuesta=2006)


save(ense_2006, file = "2006/ense2006_clean.RData")



############################ EESE 2009 ###################################

load("2009/eese2009.RData")

# Creamos las variables y nos quedamos solo con las variables de interés. 
# NO CLASE, NO COLESTEROL
eese_2009 <-  eese_2009 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO),
         edad=as.numeric(EDAD), 
         sexo=as.numeric(SEXO),
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=na_if(HH9_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(HH10_1a==1~1, HH10_1a==6~2),
         clase=NA, clase_tr=NA,
         education=na_if(HH13, "98"), education=as.numeric(na_if(education, "99")),
         education_3=case_when((education==1 | education==2 | education==3)~1, 
                               (education==4 | education==5 | education==6 | education==7)~2, 
                               (education==8 | education==9)~3),
         education_3_tr=((cume_dist(education_3)-1))*-1,
         education_5=case_when((education==1 | education==2)~1, 
                               education==3~2,
                               (education==4 | education==6)~3,
                               (education==5 | education==7)~4,
                               (education==8 | education==9)~5),
         education_5_tr=((cume_dist(education_5)-1))*-1,
         diabetes=case_when((HS4_11==1 |HS5_11==1) ~ 1, 
                            (HS4_11==6 | HS5_11==6) ~ 0), 
         col=NA,
         hta=case_when((HS4_5==1 | HS5_5==1) ~ 1, 
                       (HS4_5==6 | HS5_5==6) ~ 0),
         peso=na_if(BMI2, "998"), peso=as.numeric(na_if(peso, "999")), 
         altura=na_if(BMI1, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((SK1==1 | SK1==2) ~ 1 , 
                           (SK1==3 | SK1==4) ~ 0), 
         alcohol=case_when(AL1==1~0, 
                           (AL1==2 | AL1==2 | AL1==3 | AL1==4 
                            | AL1==5 | AL1==6)~1), 
         moderada=case_when((PE3==1 | PE3==2 | PE3==3 | PE3==4 | PE3==5 | PE3==6 | PE3==7)~0, 
                            PE3==0~1), 
         vigorosa=case_when((PE1==1 | PE1==2 | PE1==3 | PE1==4 | PE1==5 | PE1==6 | PE1==7)~0, 
                            PE1==0~1), 
         sedentario=case_when((moderada==1 | vigorosa==1)~1, 
                              (moderada==0 & vigorosa==0)~0), 
         fruta=na_if(FV1, 9), fruta=na_if(fruta, 8), 
         verdura=na_if(FV2, 9), verdura=na_if(verdura, 8), 
         fruta=case_when((fruta==1 | fruta==2)~0, 
                         (fruta==3 | fruta==4 | fruta==5 | fruta==6)~1), 
         verdura=case_when((verdura==1 | verdura==2)~0, 
                           (verdura==3 | verdura==4  | verdura==5 | verdura==6)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, 
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          imc, obesity, sobrepeso, smoking, sedentario) %>%
  mutate(encuesta=2009)

save(eese_2009, file = "2009/eese2009_clean.RData")


############################ ENSE 2011 ###################################
load("2011/ense2011.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
# Alcohol muchos perdidos, repensar
ense_2011 <-  ense_2011 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO), 
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=case_when(E2_1a==1~1, E2_1a==6~2),
         clase=na_if(CLASE_PR.x, 9),
         clase_tr=cume_dist(clase),
         education=na_if(A10_i, "98"), education=as.numeric(na_if(education, "99")), 
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
         diabetes=case_when((G21a_11==1 |G21c_11==1) ~ 1, 
                            (G21a_11==6 | G21c_11==6) ~ 0), 
         hta=case_when((G21a_1==1 |G21c_1==1) ~ 1, 
                       (G21a_1==6 | G21c_1==6) ~ 0),
         col=case_when((G21a_14==1 |G21c_14==1) ~ 1, 
                       (G21a_14==6 | G21c_11==6) ~ 0),
         peso=na_if(R102, "998"), peso=as.numeric(na_if(peso, "999")), 
         altura=na_if(R103, 998), altura=na_if(altura, 999),
         imc=round(peso/(altura/100)^2,2), 
         obesity=case_when(imc<30 ~0, imc>=30~1),
         sobrepeso=case_when(imc<25 ~0, imc>=25~1),
         smoking=case_when((S105==1 | S105==2) ~ 1 , 
                           (S105==3 | S105==4) ~ 0), 
         alcohol=case_when(T121==1~1, 
                           T121==6~0), 
         sedentario=case_when(U129==1~1, 
                              (U129==2 | U129==3 | U129==4)~0), 
         fruta=na_if(V133_1, 9), fruta=na_if(fruta, 8), 
         verdura=na_if(V133_7, 9), verdura=na_if(verdura, 8), 
         fruta=case_when(fruta==1~0, 
                         (fruta==2 | fruta==3 | fruta==4 | fruta==5)~1), 
         verdura=case_when(verdura==1~0, 
                           (verdura==2 | verdura==3  | verdura==4 | verdura==5)~1), 
         fruta_verdura=case_when(fruta==1~1, verdura==1~1, 
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, sedentario, fruta_verdura) %>%
  mutate(encuesta=2011)

save(ense_2011, file = "2011/ense2011_clean.RData")


############################ EESE 2014 ###################################
load("2014/eese2014.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2014 <-  eese_2014 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO),
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa), 
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         education=na_if(ESTUDIOS, "98"), education=as.numeric(na_if(education, "99")), 
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
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2014)

save(eese_2014, file = "2014/eese2014_clean.RData")

############################ ENSE 2017 ###################################
load("2017/ense2017.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
ense_2017 <-  ense_2017 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO),
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
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2017)


save(ense_2017, file = "2017/ense2017_clean.RData")


############################ EESE 2020 ###################################
load("2020/eese2020.RData")

# Creamos las variables y nos quedamos solo con las variables de interés
eese_2020 <-  eese_2020 %>% 
  mutate(id=IDENTHOGAR, 
         factor=as.numeric(FACTORADULTO),
         edad=as.numeric(EDADa), 
         sexo=as.numeric(SEXOa),
         sexo=case_when(sexo==1~1, sexo==2~0),
         ccaa=na_if(CCAA.x, "18"), ccaa=as.numeric(na_if(ccaa, "19")),
         migration=na_if(E1_1, 8), migration=na_if(migration, 9),
         nacionalidad=E2_1a, 
         clase=na_if(CLASE_PR.x, 8), clase=na_if(clase, 9), 
         clase_tr=cume_dist(clase),
         education=na_if(ESTUDIOS, "98"), education=as.numeric(na_if(education, "99")), 
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
                                 (fruta==0 | verdura==0)~0)) %>%
  select(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
         education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2020)
         

save(eese_2020, file = "2020/eese2020_clean.RData")


dta <- ense_2001 %>%
  rbind(ense_2003) %>%
  rbind(ense_2006) %>%
  rbind(eese_2009) %>%
  rbind(ense_2011) %>%
  rbind(eese_2014) %>%
  rbind(ense_2017) %>%
  rbind(eese_2020) 

save(dta, file = "joined_dta.RData")
