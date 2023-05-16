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


rm(list=ls())


load("joined_dta.RData")


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

# Invertimos alcohol 
dta <- dta %>%
  mutate(alcohol=(alcohol-1)*-1)


################### DESIGUALDADES POR EDUCACIÓN #########################
# 1. Global para España. Poisson por encuesta (no multinivel)

################### DIABETES #################################################
rii_diabetes_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="overall")

dta_h<- subset(dta, sexo==1)
rii_diabetes_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="Hombre")


dta_m<- subset(dta, sexo==0)
rii_diabetes_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="Mujeres")

rii_diabetes <- 
  rii_diabetes_overall %>%
  rbind(rii_diabetes_h) %>%
  rbind(rii_diabetes_m)




############################ HTA #################################################
rii_hta_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="Overall")

dta_h<- subset(dta, sexo==1)
rii_hta_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="Hombre")


dta_m<- subset(dta, sexo==0)
rii_hta_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="Mujeres")

rii_hta <- 
  rii_hta_overall %>%
  rbind(rii_hta_h) %>%
  rbind(rii_hta_m)



############################ COL #################################################
rii_col_overall <- dta %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="overall")

rii_col_h <- dta_h %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="men")

rii_col_m <- dta_m %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="women")

rii_col <- 
  rii_col_overall %>%
  rbind(rii_col_h) %>%
  rbind(rii_col_m)




############################ OBESIDAD #################################################
rii_obesity_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=obesity~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="obesity", 
         sexo="Overall")

rii_obesity_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=obesity~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="obesity", 
         sexo="Hombre")

rii_obesity_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=obesity~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="obesity", 
         sexo="Mujeres")

rii_obesity <- 
  rii_obesity_overall %>%
  rbind(rii_obesity_h) %>%
  rbind(rii_obesity_m)



############################ sobrepeso #################################################
rii_sobrepeso_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sobrepeso~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sobrepeso", 
         sexo="Overall")

rii_sobrepeso_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sobrepeso~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sobrepeso", 
         sexo="Hombre")

rii_sobrepeso_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sobrepeso~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sobrepeso", 
         sexo="Mujeres")

rii_sobrepeso <- 
  rii_sobrepeso_overall %>%
  rbind(rii_sobrepeso_h) %>%
  rbind(rii_sobrepeso_m)



############################## SMOKING #######################################
rii_smoking_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=smoking~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="smoking", 
         sexo="Overall")

rii_smoking_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=smoking~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="smoking", 
         sexo="Hombre")

rii_smoking_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=smoking~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="smoking", 
         sexo="Mujeres")

rii_smoking <- 
  rii_smoking_overall %>%
  rbind(rii_smoking_h) %>%
  rbind(rii_smoking_m)


################################# ALCOHOL #####################################
rii_alcohol_overall <- dta %>%
  filter(encuesta!=2001) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=alcohol~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="alcohol", 
         sexo="overall")

rii_alcohol_h <- dta_h %>%
  filter(encuesta!=2001) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=alcohol~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="alcohol", 
         sexo="men")

rii_alcohol_m <- dta_m %>%
  filter(encuesta!=2001) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=alcohol~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="alcohol", 
         sexo="women")

rii_alcohol <- 
  rii_alcohol_overall %>%
  rbind(rii_alcohol_h) %>%
  rbind(rii_alcohol_m)



################################# SEDENTARISMO #####################################

rii_sedentario_overall <- dta %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="overall")

rii_sedentario_h <- dta_h %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="men")

rii_sedentario_m <- dta_m %>%
  filter(encuesta!=2009) %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="women")

rii_sedentario <- 
  rii_sedentario_overall %>%
  rbind(rii_sedentario_h) %>%
  rbind(rii_sedentario_m)




################################# ALIMENTACIÓN #####################################
rii_food_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=fruta_verdura~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="fruta_verdura", 
         sexo="Overall")

rii_food_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=fruta_verdura~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="fruta_verdura", 
         sexo="Hombre")

rii_food_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=fruta_verdura~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="fruta_verdura", 
         sexo="Mujeres")

rii_food <- 
  rii_food_overall %>%
  rbind(rii_food_h) %>%
  rbind(rii_food_m)



rii_spain <- rii_diabetes %>%
  rbind(rii_hta) %>%
  rbind(rii_col) %>%
  rbind(rii_obesity) %>%
  rbind(rii_smoking) %>%
  rbind(rii_alcohol) %>%
  rbind(rii_sedentario) %>%
  rbind(rii_food) %>%
  mutate(ccaa=0)

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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
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
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")


dta_h <- dta_h %>%
  mutate(alcohol=(alcohol-1)*-1)

rii_alcohol_h <- glmmTMB(alcohol~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2001),
                         family="poisson")

rii_alcohol_h <- rii_alcohol_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")

dta_m <- dta_m %>%
  mutate(alcohol=(alcohol-1)*-1)

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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")

rii_alcohol <- rii_alcohol %>%
  rbind(rii_alcohol_h) %>%
  rbind(rii_alcohol_m)




########################### SEDENTARISMO #####################################
rii_sedentario <- glmmTMB(sedentario~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta, encuesta!=2009),
                        family="poisson")

rii_sedentario <- rii_sedentario %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentario")

rii_sedentario_h <- glmmTMB(sedentario~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2009),
                         family="poisson")

rii_sedentario_h <- rii_sedentario_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentario")


rii_sedentario_m <- glmmTMB(sedentario~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_m, encuesta!=2009),
                         family="poisson")

rii_sedentario_m <- rii_sedentario_m %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Mujeres") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentario")

rii_sedentario <- rii_sedentario %>%
  rbind(rii_sedentario_h) %>%
  rbind(rii_sedentario_m)






########################### ALIMENTACIÓN #####################################
rii_food <- glmmTMB(fruta_verdura~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                          family="poisson")

rii_food <- rii_food %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="food")

rii_food_h <- glmmTMB(fruta_verdura~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_h,
                            family="poisson")

rii_food_h <- rii_food_h %>%
  extract_random_coefs(re="encuesta:ccaa") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Hombre") %>%
  separate(group, c('encuesta', 'ccaa')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="food")


rii_food_m <- glmmTMB(fruta_verdura~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta_m,
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
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentario")

rii_food <- rii_food %>%
  rbind(rii_food_h) %>%
  rbind(rii_food_m)











rii_ccaa <- rii_diabetes %>%
  rbind(rii_hta) %>%
  rbind(rii_col) %>%
  rbind(rii_obesity) %>%
  rbind(rii_smoking) %>%
  rbind(rii_alcohol) %>%
  rbind(rii_sedentario) %>%
  rbind(rii_food)




write.csv(rii_ccaa, "rii_ccaa.csv")









