library(plyr)
library(tidyverse)
library(scales)
library(broom)
library(survey)
library(lubridate)
library(lme4)
library(broom.mixed)
library(glmmTMB)
library(mixedup)


rm(list=ls())


load("joined_dta.RData")

#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

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

sii_sedentario_education <- sii_sedentario_overall %>%
  rbind(sii_sedentario_h) %>%
  rbind(sii_sedentario_m)

sii_sedentario_education_temp <- sii_sedentario_education %>% 
  mutate(exp="sii")
rii_sedentario_education_temp <- rii_sedentario_education %>% 
  mutate(exp="rii")
sii_sedentario_education_temp <- rename(sii_sedentario_education_temp,est = sii, infci=sii_infci, supci=sii_supci)
rii_sedentario_education_temp <- rename(rii_sedentario_education_temp,est = rii, infci=rii_infci, supci=rii_supci)
sedentario_educacion <- sii_sedentario_education_temp %>% 
  rbind(rii_sedentario_education_temp)

############################################################################################################
#####################################ANALISIS POR COMUNIDADES AUTÓNOMAS#####################################
############################################################################################################

#Primero el Relative Index of Inequality

#Estimamos con modelos multinivel que asumen que hay una desigualdad que cambia en el tiempo. Observaciones -> ccaa -> tiempo
rii_sedentario_CCAA <- glmmTMB(sedentario~education_3_tr+edad+sexo+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=dta,
                          family="poisson")

rii_sedentario_CCAA <- rii_sedentario_CCAA %>%
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

rii_sedentario_CCAA_h <- glmmTMB(sedentario~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_h, encuesta!=2001),
                            family="poisson")

rii_sedentario_CCAA_h <- rii_sedentario_CCAA_h %>%
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


rii_sedentario_CCAA_m <- glmmTMB(sedentario~education_3_tr+edad+(1+education_3_tr|encuesta) + (1+education_3_tr|encuesta: ccaa), data=subset(dta_m, encuesta!=2001),
                            family="poisson")

rii_sedentario_CCAA_m <- rii_sedentario_CCAA_m %>%
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

rii_sedentario_CCAA <- rii_sedentario_CCAA %>%
  rbind(rii_sedentario_CCAA_h) %>%
  rbind(rii_sedentario_CCAA_m)


############################################################################################################
####################################VISUALIZACIÓN DE TODOS LOS ANÁLISIS#####################################
############################################################################################################

theme_fis<-  theme(axis.text=element_text(size=10, color="black"),
                   axis.title=element_text(size=10, face="bold", color="black"),
                   strip.text = element_text(size=10, face="bold", color="black"),
                   legend.text=element_text(size=10, color="black"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(color="black", size=10),
                   axis.text.y = element_text(color="black", size=10),
                   legend.position="bottom")

####FIGURA RII (EDUCACIÓN) HOMBRES, MUJERES, OVERALL####

fig_rii_junto <-  ggplot(rii_sedentario_education, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
                  geom_hline(yintercept = 1, lty=2)+
                  geom_ribbon(alpha=0.3, aes(fill=sexo))+
                  geom_line(aes(color=sexo)) +
                  scale_y_continuous(trans="log",
                                     breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
                  labs(x="", y="RII (95% CI)")+
                  theme_bw()+
                  theme_fis+
                  theme()

fig_rii_junto


fig_rii_separado <-  ggplot(rii_sedentario_education, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  facet_grid(cols=vars(sexo), scales = "free_y") +
  scale_y_continuous(trans="log",
                     breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()

fig_rii_separado

####FIGURA SII (EDUCACIÓN) HOMBRES, MUJERES, OVERALL####

figura_sii_junto <-   ggplot(sii_sedentario_education, aes(x=encuesta, y=sii, ymin=sii_infci, ymax=sii_supci)) +
                      geom_hline(yintercept = 1, lty=2)+
                      geom_ribbon(alpha=0.3, aes(fill=sexo))+
                      geom_line(aes(color=sexo)) +
                      # scale_y_continuous(trans="log",
                      #                    breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
                      labs(x="", y="SII (95% CI)")+
                      theme_bw()+
                      theme_fis+
                      theme()

figura_sii_junto


figura_sii_separado <-  ggplot(sii_sedentario_education, aes(x=encuesta, y=sii, ymin=sii_infci, ymax=sii_supci)) +
                        geom_hline(yintercept = 1, lty=2)+
                        geom_ribbon(alpha=0.3, aes(fill=sexo))+
                        geom_line(aes(color=sexo)) +
                        facet_grid(cols=vars(sexo), scales = "free_y") +
                        # scale_y_continuous(trans="log",
                        #                    breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
                        labs(x="", y="SII (95% CI)")+
                        theme_bw()+
                        theme_fis+
                        theme()

figura_sii_separado

####FIGURA RII Y SII HOMBRES, MUJERES, OVERALL####

figura_sii_rii <-   ggplot(sedentario_educacion, aes(x=encuesta, y=est, ymin=infci, ymax=supci)) +
                    geom_hline(yintercept = 1, lty=2)+
                    geom_ribbon(alpha=0.3, aes(fill=sexo))+
                    geom_line(aes(color=sexo)) +
                    facet_grid(cols=vars(sexo), rows = vars(exp), scales = "free_y") +
                    # scale_y_continuous(trans="log",
                    #                    breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
                    labs(x="", y="SII (95% CI)")+
                    theme_bw()+
                    theme_fis+
                    theme()

figura_sii_rii

####FIGURA RII COMUNIDADES AUTÓNOMAS HOMBRES, MUJERES, OVERALL####

fig_CCAA <- ggplot(rii_sedentario_CCAA, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill="red"))+
  geom_line(aes(color="red")) +
  facet_grid(rows = vars(nombre_notilde), cols = vars(sexo), scales = "free") +
  scale_y_continuous(trans="log",
                     breaks=c(0.75, 1, 1.5, 2, 4, 8, 16, 32))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme(legend.position="none",
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black", size=10),
        axis.title = element_text(color="black", size=10),
        axis.title.y = element_text(color="black", size=10, margin = margin(r=23)),
        axis.title.x = element_text(color="black", size=10, margin = margin(t=23)),
        strip.text = element_text(color="black", size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fig_CCAA


############################################################################################################
##################################OBTENEMOS VARIABLES iPAQ PARA 2009-2020###################################
############################################################################################################
library(tidyverse)
library(openxlsx)
library(haven)

rm(list=ls())

###################################### 2009 #################################################################
#Cargamos encuesta adultos
campos <- read.xlsx("2009/Adul09.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2009 <- read_fwf("2009/ADULTO09.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

##Comprobamos que esté todo ok##
table(adulto2009$PE3)
table(adulto2009$PE4_1)
table(adulto2009$PE4_2)
table(adulto2009$PE1)
table(adulto2009$PE2_1)
table(adulto2009$PE2_2)
table(adulto2009$PE5)
table(adulto2009$PE6_1)
table(adulto2009$PE6_2)

#convertir a numérica 

#Obtenemos minutos/día de cada intensidad de actividad#
adulto2009 <- adulto2009 %>% 
  mutate(moderada=as.numeric(adulto2009$PE4_1))
  
  mutate(moderada_min_dia = as.numeric(adulto2009$PE4_1)* 60)

###################################### 2011 #################################################################
#Cargamos encuesta adultos
campos <- read.xlsx("2011/Adul11.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2011 <- read_fwf("2011/ADULTO11.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))



###################################### 2014 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2014/Adul14.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2014 <- read_fwf("2014/ADULTO14.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

###################################### 2017 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2017/Adul17.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2017 <- read_fwf("2017/ADULTO17.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

adulto2017 <- adulto2017 %>% 
  mutate(moderada=as.numeric(adulto2009$PE4_1))