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


#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)



################### DESIGUALDADES POR EDUCACIÓN #########################

####################### DIABETES ##################################
#Primero el Relative Index of Inequality

#Estimamos con modelos multinivel que asumen que hay una desigualdad que cambia en el tiempo. Observaciones -> ccaa -> tiempo


rii_diabetes <- glmmTMB(diabetes~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                      family="poisson")

rii_diabetes <- rii_diabetes %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="diabetes")




################################# HTA ######################################


rii_hta <- glmmTMB(hta~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_hta <- rii_hta %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="hta")





############################# COLESEROL #######################################


rii_col <- glmmTMB(col~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=subset(dta, encuesta!=2009),
                        family="poisson")

rii_col <- rii_col %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="col")



############################### OBESIDAD #######################################


rii_obesity <- glmmTMB(obesity~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_obesity <- rii_obesity %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="obesity")


################################ SOBREPESO #####################################


rii_sobrepeso <- glmmTMB(sobrepeso~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_sobrepeso <- rii_sobrepeso %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sobrepeso")


############################## SMOKING #######################################


rii_smoking <- glmmTMB(smoking~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_smoking <- rii_smoking %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="smoking")



################################# ALCOHOL #####################################

dta <- dta %>%
  mutate(alcohol=(alcohol-1)*-1)

rii_alcohol <- glmmTMB(alcohol~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_alcohol <- rii_alcohol %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="alcohol")


########################### SEDENTARISMO #####################################


rii_sedentario <- glmmTMB(sedentario~education_3_tr+edad+sexo+(1+education_3_tr|ccaa) + (1+education_3_tr|ccaa: encuesta), data=dta,
                        family="poisson")

rii_sedentario <- rii_sedentario %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         rii_infci=exp(value-1.96*se),
         rii_supci=exp(value+1.96*se)) %>% 
  filter(effect=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas) %>%
  mutate(fr="sedentario")















