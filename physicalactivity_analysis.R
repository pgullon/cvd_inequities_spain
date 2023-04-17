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
library(xlsx)
library(x)




rm(list=ls())


load("joined_dta.RData")



#Centramos edad
dta <- dta %>%
  mutate(edad=scale(edad, center=T, scale=F))

# RII EDUCACIÓN OVERALL - SEDENTARISMO

rii_sedentario_overall <- dta %>%
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

# RII EDUCACIÓN OVERALL - SEDENTARISMO

rii_sedentario_overall <- dta %>%
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


# RII EDUCACIÓN HOMBRE - SEDENTARISMO

dta_h<- subset(dta, sexo==1)
rii_sedentario_h <- dta_h %>%
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

# RII EDUCACIÓN MUJER - SEDENTARISMO

dta_m<- subset(dta, sexo==0)
rii_sedentario_m <- dta_m %>%
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


rii_sedentario_education <- 
  rii_sedentario_overall %>%
  rbind(rii_sedentario_h) %>%
  rbind(rii_sedentario_m)


#SII EDUCATION SEDENTARISM##

sii_sedentario_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="overall")


sii_sedentario_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="men")

sii_sedentario_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="sedentario", 
         sexo="women")

sii_sedentario_education <- 
  sii_sedentario_overall %>%
  rbind(sii_sedentario_h) %>%
  rbind(sii_sedentario_m)

sedentarismo_educacion <-
  rii_sedentario_education %>% 
  cbind(sii_sedentario_education)