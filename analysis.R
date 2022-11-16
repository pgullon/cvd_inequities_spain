library(tidyverse)
library(scales)
library(broom)

rm(list=ls())

load("2003/ense2003_clean.RData")
load("2006/ense2006_clean.RData")
load("2011/ense2011_clean.RData")
load("2014/eese2014_clean.RData")
load("2017/ense2017_clean.RData")
load("2020/eese2020_clean.RData")


# Relative Index of Inequality

rii_hombres_2003<-glm(formula=diabetes~clase_tr+edad, data=subset(ense_2003,sexo==1), 
                    family=poisson(link="log"))
rii_hombres_2003 <- rii_hombres_2003 %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo=1) %>%
  mutate(encuesta=2003)
rii_hombres_2003

rii_mujeres_2003<-glm(formula=diabetes~clase_tr+edad, data=subset(ense_2003,sexo==6), 
                      family=poisson(link="log"))
rii_mujeres_2003 <- rii_mujeres_2003 %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo=2) %>%
  mutate(encuesta=2003)
rii_mujeres_2003

rii_hombres_2006<-glm(formula=diabetes~clase_tr+edad, data=subset(ense_2006,sexo==1), 
                      family=poisson(link="log"))
rii_hombres_2006 <- rii_hombres_2006 %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo=1) %>%
  mutate(encuesta=2006)
rii_hombres_2006

rii_mujeres_2006<-glm(formula=diabetes~clase_tr+edad, data=subset(ense_2006,sexo==6), 
                      family=poisson(link="log"))
rii_mujeres_2006 <- rii_mujeres_2006 %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo=2) %>%
  mutate(encuesta=2006)
rii_mujeres_2006





rii_regresion=rbind(coef_model2_hombres, coef_model2_mujeres)