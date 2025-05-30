library(plyr)
library(tidyverse)
library(scales)
library(broom)
library(survey)
library(lubridate)
library(lme4)
library(broom.mixed)
library(mixedup)
library(glmmTMB)
library(srvyr)


rm(list=ls())


load("joined_dta.RData")


# Invertimos alcohol 
dta <- dta %>%
  mutate(alcohol=(alcohol-1)*-1)

# Creamos bases para hombres y mujeres
dta_h <- dta %>%
  filter(sexo==1)
dta_m <- dta %>%
  filter(sexo==0)



#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)


#Centramos edad
dta <- dta %>%
  mutate(edad=scale(edad, center=T, scale=F))



################### DESIGUALDADES POR EDUCACIÓN #########################
# 1. Global para España. Poisson por encuesta (no multinivel)

################### DIABETES #################################################
rii_diabetes_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=diabetes~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="diabetes", 
         sexo="Global")



rii_diabetes_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=diabetes~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="diabetes", 
         sexo="Hombres")




rii_diabetes_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=diabetes~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="diabetes", 
         sexo="Mujeres")


rii_diabetes <- 
  rii_diabetes_Global %>%
  rbind(rii_diabetes_h) %>%
  rbind(rii_diabetes_m)




############################ HTA #################################################
rii_hta_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=hta~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="hta", 
         sexo="Global")



rii_hta_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=hta~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="hta", 
         sexo="Hombres")




rii_hta_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=hta~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="hta", 
         sexo="Mujeres")

rii_hta <- 
  rii_hta_Global %>%
  rbind(rii_hta_h) %>%
  rbind(rii_hta_m)



############################ COL #################################################
rii_col_Global <-  dta %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=col~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="col", 
         sexo="Global")



rii_col_h <- dta_h %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=col~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="col", 
         sexo="Hombres")




rii_col_m <- dta_m %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=col~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="col", 
         sexo="Mujeres")

rii_col <- 
  rii_col_Global %>%
  rbind(rii_col_h) %>%
  rbind(rii_col_m)




############################ OBESIDAD #################################################
rii_obesity_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=obesity~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="obesity", 
         sexo="Global")



rii_obesity_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=obesity~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="obesity", 
         sexo="Hombres")




rii_obesity_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=obesity~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="obesity", 
         sexo="Mujeres")

rii_obesity <- 
  rii_obesity_Global %>%
  rbind(rii_obesity_h) %>%
  rbind(rii_obesity_m)



############################ sobrepeso #################################################
rii_sobrepeso_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sobrepeso~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sobrepeso", 
         sexo="Global")



rii_sobrepeso_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sobrepeso~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sobrepeso", 
         sexo="Hombres")




rii_sobrepeso_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sobrepeso~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sobrepeso", 
         sexo="Mujeres")

rii_sobrepeso <- 
  rii_sobrepeso_Global %>%
  rbind(rii_sobrepeso_h) %>%
  rbind(rii_sobrepeso_m)



############################## SMOKING #######################################
rii_smoking_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=smoking~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="smoking", 
         sexo="Global")



rii_smoking_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=smoking~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="smoking", 
         sexo="Hombres")




rii_smoking_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=smoking~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="smoking", 
         sexo="Mujeres")

rii_smoking <- 
  rii_smoking_Global %>%
  rbind(rii_smoking_h) %>%
  rbind(rii_smoking_m)


################################# ALCOHOL #####################################
rii_alcohol_Global <-  dta %>%
  filter(encuesta!=2001) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=alcohol~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="alcohol", 
         sexo="Global")



rii_alcohol_h <- dta_h %>%
  filter(encuesta!=2001) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=alcohol~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="alcohol", 
         sexo="Hombres")




rii_alcohol_m <- dta_m %>%
  filter(encuesta!=2001) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=alcohol~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="alcohol", 
         sexo="Mujeres")


rii_alcohol <- 
  rii_alcohol_Global %>%
  rbind(rii_alcohol_h) %>%
  rbind(rii_alcohol_m)



################################# sedentarismo #####################################

rii_sedentarismo_Global <-  dta %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sedentarismo~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sedentarismo", 
         sexo="Global")



rii_sedentarismo_h <- dta_h %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sedentarismo~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sedentarismo", 
         sexo="Hombres")




rii_sedentarismo_m <- dta_m %>%
  filter(encuesta!=2009) %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=sedentarismo~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="sedentarismo", 
         sexo="Mujeres")

rii_sedentarismo <- 
  rii_sedentarismo_Global %>%
  rbind(rii_sedentarismo_h) %>%
  rbind(rii_sedentarismo_m)




################################# ALIMENTACIÓN #####################################
rii_food_Global <-  dta %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=food~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="food", 
         sexo="Global")



rii_food_h <- dta_h %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=food~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="food", 
         sexo="Hombres")




rii_food_m <- dta_m %>%
  mutate(education_prueba=((cume_dist(education_3)-1))*-1) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~svyglm(formula=food~education_prueba+edad+sexo, data=.x, 
                                 design=svydesign(ids=~1,weights=~factor2,data=.), 
                                 family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_prueba") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(fr="food", 
         sexo="Mujeres")


rii_food <- 
  rii_food_Global %>%
  rbind(rii_food_h) %>%
  rbind(rii_food_m)



rii_spain <- rii_diabetes %>%
  rbind(rii_hta) %>%
  rbind(rii_col) %>%
  rbind(rii_obesity) %>%
  rbind(rii_smoking) %>%
  rbind(rii_alcohol) %>%
  rbind(rii_sedentarismo) %>%
  rbind(rii_food) %>%
  mutate(ccaa=0, 
         abreviatura="ES",
         nombre_notilde="Espana", 
         nombre="Espana", 
         id_mapa=0)
  

write.csv(rii_spain, "rii_spain.csv")


###################################################################################################################################################
#2. Por CCAA


####################### DIABETES ##################################
#Relative Index of Inequality

#Estimamos con modelos multinivel que asumen que hay una desigualdad que cambia en el tiempo. Observaciones -> ccaa -> tiempo
rii_diabetes <- glmmTMB(diabetes~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                      family="poisson")

rii_diabetes <- rii_diabetes %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="diabetes")



rii_diabetes_h <- glmmTMB(diabetes~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                        family="poisson")

rii_diabetes_h <- rii_diabetes_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="diabetes")




rii_diabetes_m <- glmmTMB(diabetes~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                          family="poisson")

rii_diabetes_m <- rii_diabetes_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="diabetes")

rii_diabetes <- rii_diabetes %>%
  rbind(rii_diabetes_h) %>%
  rbind(rii_diabetes_m)



################################# HTA ######################################


rii_hta <- glmmTMB(hta~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                        family="poisson")

rii_hta <- rii_hta %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="hta")


rii_hta_h <- glmmTMB(hta~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                          family="poisson")

rii_hta_h <- rii_hta_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="hta")


rii_hta_m <- glmmTMB(hta~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                          family="poisson")

rii_hta_m <- rii_hta_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="hta")

rii_hta <- rii_hta %>%
  rbind(rii_hta_h) %>%
  rbind(rii_hta_m)



############################# COLESEROL #######################################

rii_col <- glmmTMB(col~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta, encuesta!=2009),
                   family="poisson")

rii_col <- rii_col %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="col")


rii_col_h <- glmmTMB(col~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2009),
                     family="poisson")

rii_col_h <- rii_col_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="col")


rii_col_m <- glmmTMB(col~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_m, encuesta!=2009),
                     family="poisson")

rii_col_m <- rii_col_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="col")

rii_col <- rii_col %>%
  rbind(rii_col_h) %>%
  rbind(rii_col_m)







############################### OBESIDAD #######################################


rii_obesity <- glmmTMB(obesity~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                        family="poisson")

rii_obesity <- rii_obesity %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="obesity")


rii_obesity_h <- glmmTMB(obesity~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                     family="poisson")

rii_obesity_h <- rii_obesity_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="obesity")


rii_obesity_m <- glmmTMB(obesity~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                     family="poisson")

rii_obesity_m <- rii_obesity_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="obesity")

rii_obesity <- rii_obesity %>%
  rbind(rii_obesity_h) %>%
  rbind(rii_obesity_m)







################################ SOBREPESO #####################################


rii_sobrepeso <- glmmTMB(sobrepeso~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                        family="poisson")

rii_sobrepeso <- rii_sobrepeso %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sobrepeso")



rii_sobrepeso_h <- glmmTMB(sobrepeso~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                         family="poisson")

rii_sobrepeso_h <- rii_sobrepeso_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sobrepeso")


rii_sobrepeso_m <- glmmTMB(sobrepeso~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                         family="poisson")

rii_sobrepeso_m <- rii_sobrepeso_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sobrepeso")

rii_sobrepeso <- rii_sobrepeso %>%
  rbind(rii_sobrepeso_h) %>%
  rbind(rii_sobrepeso_m)





############################## SMOKING #######################################
rii_smoking <- glmmTMB(smoking~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                        family="poisson")

rii_smoking <- rii_smoking %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="smoking")


rii_smoking_h <- glmmTMB(smoking~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                           family="poisson")

rii_smoking_h <- rii_smoking_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="smoking")


rii_smoking_m <- glmmTMB(smoking~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                           family="poisson")

rii_smoking_m <- rii_smoking_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="smoking")

rii_smoking <- rii_smoking %>%
  rbind(rii_smoking_h) %>%
  rbind(rii_smoking_m)




################################# ALCOHOL #####################################


rii_alcohol <- glmmTMB(alcohol~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta, encuesta!=2001),
                        family="poisson")

rii_alcohol <- rii_alcohol %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")



rii_alcohol_h <- glmmTMB(alcohol~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2001),
                         family="poisson")

rii_alcohol_h <- rii_alcohol_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")


rii_alcohol_m <- glmmTMB(alcohol~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_m, encuesta!=2001),
                         family="poisson")

rii_alcohol_m <- rii_alcohol_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")

rii_alcohol <- rii_alcohol %>%
  rbind(rii_alcohol_h) %>%
  rbind(rii_alcohol_m)




########################### sedentarismo #####################################
rii_sedentarismo <- glmmTMB(sedentarismo~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta, encuesta!=2009),
                        family="poisson")


rii_sedentarismo <- rii_sedentarismo %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentarismo")

rii_sedentarismo_h <- glmmTMB(sedentarismo~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2009),
                         family="poisson")

rii_sedentarismo_h <- rii_sedentarismo_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentarismo")


rii_sedentarismo_m <- glmmTMB(sedentarismo~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_m, encuesta!=2009),
                         family="poisson")

rii_sedentarismo_m <- rii_sedentarismo_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentarismo")

rii_sedentarismo <- rii_sedentarismo %>%
  rbind(rii_sedentarismo_h) %>%
  rbind(rii_sedentarismo_m)






########################### ALIMENTACIÓN #####################################
rii_food <- glmmTMB(food~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                          family="poisson")

rii_food <- rii_food %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Global") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="food")

rii_food_h <- glmmTMB(food~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                            family="poisson")

rii_food_h <- rii_food_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="food")


rii_food_m <- glmmTMB(food~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
                            family="poisson")

rii_food_m <- rii_food_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate( 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="food")

rii_food <- rii_food %>%
  rbind(rii_food_h) %>%
  rbind(rii_food_m)











rii_ccaa <- rii_diabetes %>%
  rbind(rii_hta) %>%
  rbind(rii_col) %>%
  rbind(rii_obesity) %>%
  rbind(rii_smoking) %>%
  rbind(rii_alcohol) %>%
  rbind(rii_sedentarismo) %>%
  rbind(rii_food)




write.csv(rii_ccaa, "rii_ccaa.csv")


## Base para los informes
rii_informes <- rii_spain %>%
  rbind(rii_ccaa) %>% 
  filter(encuesta!=2001) %>% 
  filter(encuesta!=2009) %>% 
  mutate(ccaa=as.factor(ccaa), 
         fr = recode(fr, "sedentario"="sedentarismo"))

save(rii_informes, file = "Informes_CCAA/RII_informes.RData")




