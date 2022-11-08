#This file is for data cleaning and harmonization of the data
library(tidyverse)


# Nombres comunes para las variables en todas las encuestas
# Edad=age
# Sexo=sex
# Comunidad Autónoma=ccaa
# País de nacimiento=migration
# Clase social=clase
# Clase social transformada 0-1=clase_tr
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
# Sobrepeso+obesidad=overweight
# Tabaquismo=smoking
# Consumo de alcohol=alcohol
# Actividad física=activity
# Alimentación=food


############################ EESE 2020 ###################################
load("2020/eese2020.RData")

# Creamos las variables 
eese_2020 <-  eese_2020 %>% 
  mutate(diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, 
                            (G25a_12==2 | G25c_12==2) ~ 0), 
         hta=case_when((G25a_1==1 |G25c_1==1) ~ 1, 
                            (G25a_1==2 | G25c_1==2) ~ 0),
         col=case_when((G25a_15==1 |G25c_15==1) ~ 1, 
                            (G25a_15==2 | G25c_15==2) ~ 0),
         peso=)


