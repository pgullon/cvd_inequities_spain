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



ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)


# Invertimos alcohol 
dta <- dta %>%
  mutate(alcohol=(alcohol-1)*-1)


prevalencias_peso_ccaa_overall <- dta %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(ccaa, education_3, encuesta) %>%
  summarize(diabetes = survey_mean(diabetes, na.rm = T, vartype = "ci"), 
            hta = survey_mean(hta, na.rm = T, vartype = "ci"), 
            col = survey_mean(col, na.rm = T, vartype = "ci"), 
            obesity = survey_mean(obesity, na.rm = T, vartype = "ci"), 
            sobrepeso = survey_mean(sobrepeso, na.rm = T, vartype = "ci"),
            smoking = survey_mean(smoking, na.rm = T, vartype = "ci"), 
            alcohol = survey_mean(alcohol, na.rm = T, vartype = "ci"),
            sedentario = survey_mean(sedentario, na.rm = T, vartype = "ci"),
            fruta_verdura = survey_mean(fruta_verdura, na.rm = T, vartype = "ci")) %>%
  left_join(ccaas) %>%
  mutate(sexo="Overall")

prevalencias_peso_ccaa_sexo <- dta %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(ccaa, education_3, encuesta, sexo) %>%
  summarize(diabetes = survey_mean(diabetes, na.rm = T, vartype = "ci"), 
            hta = survey_mean(hta, na.rm = T, vartype = "ci"), 
            col = survey_mean(col, na.rm = T, vartype = "ci"), 
            obesity = survey_mean(obesity, na.rm = T, vartype = "ci"), 
            sobrepeso = survey_mean(sobrepeso, na.rm = T, vartype = "ci"),
            smoking = survey_mean(smoking, na.rm = T, vartype = "ci"), 
            alcohol = survey_mean(alcohol, na.rm = T, vartype = "ci"),
            sedentario = survey_mean(sedentario, na.rm = T, vartype = "ci"),
            fruta_verdura = survey_mean(fruta_verdura, na.rm = T, vartype = "ci")) %>%
  left_join(ccaas)


prevalencias_ccaa <- prevalencias_peso_ccaa_overall %>%
  rbind(prevalencias_peso_ccaa_sexo)

write.csv(prevalencias_ccaa, "prevalencias_ccaa.csv")


prevalencias_peso_spain_overall <- dta %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(education_3, encuesta) %>%
  summarize(diabetes = survey_mean(diabetes, na.rm = T, vartype = "ci"), 
            hta = survey_mean(hta, na.rm = T, vartype = "ci"), 
            col = survey_mean(col, na.rm = T, vartype = "ci"), 
            obesity = survey_mean(obesity, na.rm = T, vartype = "ci"), 
            sobrepeso = survey_mean(sobrepeso, na.rm = T, vartype = "ci"),
            smoking = survey_mean(smoking, na.rm = T, vartype = "ci"), 
            alcohol = survey_mean(alcohol, na.rm = T, vartype = "ci"),
            sedentario = survey_mean(sedentario, na.rm = T, vartype = "ci"),
            fruta_verdura = survey_mean(fruta_verdura, na.rm = T, vartype = "ci")) %>%
  mutate(sexo="Overall")



prevalencias_peso_spain_sexo <- dta %>%
  as_survey_design(weights = c(factor2)) %>%
  group_by(sexo, education_3, encuesta) %>%
  summarize(diabetes = survey_mean(diabetes, na.rm = T, vartype = "ci"), 
            hta = survey_mean(hta, na.rm = T, vartype = "ci"), 
            col = survey_mean(col, na.rm = T, vartype = "ci"), 
            obesity = survey_mean(obesity, na.rm = T, vartype = "ci"), 
            sobrepeso = survey_mean(sobrepeso, na.rm = T, vartype = "ci"),
            smoking = survey_mean(smoking, na.rm = T, vartype = "ci"), 
            alcohol = survey_mean(alcohol, na.rm = T, vartype = "ci"),
            sedentario = survey_mean(sedentario, na.rm = T, vartype = "ci"),
            fruta_verdura = survey_mean(fruta_verdura, na.rm = T, vartype = "ci"))


prevalencias_spain <- prevalencias_peso_spain_overall %>%
  rbind(prevalencias_peso_spain_sexo)

write.csv(prevalencias_spain, "prevalencias_spain.csv")


