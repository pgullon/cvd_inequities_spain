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



#Centramos edad
dta <- dta %>%
  mutate(edad=scale(edad, center=T, scale=F))



############################ COL #################################################
# RII EDUCACIÓN
 
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
         sexo="men")


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
         sexo="women")

rii_diabetes_education <- 
  rii_diabetes_overall %>%
  rbind(rii_diabetes_h) %>%
  rbind(rii_diabetes_m)



# SII EDUCACIÓN
sii_diabetes_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="overall")


sii_diabetes_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="men")

sii_diabetes_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="women")

sii_diabetes_education <- 
  sii_diabetes_overall %>%
  rbind(sii_diabetes_h) %>%
  rbind(sii_diabetes_m)




# RII CLASE
dta_clase <- subset(dta, encuesta!=2009)

rii_diabetes_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="overall")

dta_clase_h<- subset(dta_clase, sexo==1)
rii_diabetes_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="men")


dta_clase_m<- subset(dta_clase, sexo==0)
rii_diabetes_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="women")

rii_diabetes_clase <- 
  rii_diabetes_overall %>%
  rbind(rii_diabetes_h) %>%
  rbind(rii_diabetes_m)



# SII CLASE
sii_diabetes_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="overall")


sii_diabetes_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="men")

sii_diabetes_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=diabetes~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="diabetes", 
         sexo="women")

sii_diabetes_clase <- 
  sii_diabetes_overall %>%
  rbind(sii_diabetes_h) %>%
  rbind(sii_diabetes_m)




############################ HTA #################################################
# RII EDUCACIÓN

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
         sexo="overall")

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
         sexo="men")


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
         sexo="women")

rii_hta_education <- 
  rii_hta_overall %>%
  rbind(rii_hta_h) %>%
  rbind(rii_hta_m)



# SII EDUCACIÓN
sii_hta_overall <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="overall")


sii_hta_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="men")

sii_hta_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="women")

sii_hta_education <- 
  sii_hta_overall %>%
  rbind(sii_hta_h) %>%
  rbind(sii_hta_m)




# RII CLASE
dta_clase <- subset(dta, encuesta!=2009)

rii_hta_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="overall")

dta_clase_h<- subset(dta_clase, sexo==1)
rii_hta_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="men")


dta_clase_m<- subset(dta_clase, sexo==0)
rii_hta_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="women")

rii_hta_clase <- 
  rii_hta_overall %>%
  rbind(rii_hta_h) %>%
  rbind(rii_hta_m)



# SII CLASE
sii_hta_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="overall")


sii_hta_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="men")

sii_hta_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=hta~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="hta", 
         sexo="women")

sii_hta_clase <- 
  sii_hta_overall %>%
  rbind(sii_hta_h) %>%
  rbind(sii_hta_m)





############################ COL #################################################
# RII EDUCACIÓN

rii_col_overall <- dta_clase %>%
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

dta_clase_h<- subset(dta_clase, sexo==1)
rii_col_h <- dta_clase_h %>%
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


dta_clase_m<- subset(dta_clase, sexo==0)
rii_col_m <- dta_clase_m %>%
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

rii_col_education <- 
  rii_col_overall %>%
  rbind(rii_col_h) %>%
  rbind(rii_col_m)



# SII EDUCACIÓN
sii_col_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="overall")


sii_col_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="men")

sii_col_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~education_3_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="education_3_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="women")

sii_col_education <- 
  sii_col_overall %>%
  rbind(sii_col_h) %>%
  rbind(sii_col_m)




# RII CLASE
dta_clase <- subset(dta, encuesta!=2009)

rii_col_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="overall")

dta_clase_h<- subset(dta_clase, sexo==1)
rii_col_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="men")


dta_clase_m<- subset(dta_clase, sexo==0)
rii_col_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="women")

rii_col_clase <- 
  rii_col_overall %>%
  rbind(rii_col_h) %>%
  rbind(rii_col_m)



# SII CLASE
sii_col_overall <- dta_clase %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad+sexo, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="overall")


sii_col_h <- dta_clase_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="men")

sii_col_m <- dta_clase_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=col~clase_tr+edad, data=.x, 
                              family=poisson(link="identity"),start = c(1, 0.04, 0))), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci, encuesta) %>% 
  mutate(risk_factor="col", 
         sexo="women")

sii_col_clase <- 
  sii_col_overall %>%
  rbind(sii_col_h) %>%
  rbind(sii_col_m)


# Tablas

diabetes_education <- rii_diabetes_education %>%
  left_join(sii_diabetes_education)
hta_education <- rii_hta_education %>%
  left_join(sii_hta_education)
col_education <- rii_col_education %>%
  left_join(sii_col_education) 
education <- diabetes_education %>%
  rbind(hta_education) %>%
  rbind(col_education)
  
write.csv(education, "desigualdades_educacion.csv")


diabetes_clase <- rii_diabetes_clase %>%
  left_join(sii_diabetes_clase)
hta_clase <- rii_hta_clase %>%
  left_join(sii_hta_clase)
col_clase <- rii_col_clase %>%
  left_join(sii_col_clase) 
clase <- diabetes_clase %>%
  rbind(hta_clase) %>%
  rbind(col_clase)

write.csv(clase, "desigualdades_clase.csv")





