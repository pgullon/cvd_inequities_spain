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
library(tidyverse)
library(openxlsx)
library(haven)
library(sf)
library(rgdal)
library(broom)
library(summarytools)
library(table1)
library(cowplot)
library(lpSolve)
library(irr)


rm(list=ls())

theme_fis<-  theme(axis.text=element_text(size=10, color="black"),
                   axis.title=element_text(size=10, face="bold", color="black"),
                   strip.text = element_text(size=10, face="bold", color="black"),
                   legend.text=element_text(size=10, color="black"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.text.x = element_text(color="black", size=10),
                   axis.text.y = element_text(color="black", size=10),
                   legend.position="bottom")

load("joined_dta.RData")

#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

#Duplicamos edad para tabla 1

dta <- dta %>% 
  mutate(edad_pura = edad)

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
  mutate(risk_factor="Sedentarismo", 
         sexo="Overall")

# RII EDUCACIÓN OVERALL - SEDENTARISMO

rii_sedentario_overall_weighted <- dta %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad+sexo, weights = factor, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Sedentarismo", 
         sexo="Overall")


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
  mutate(risk_factor="Sedentarismo", 
         sexo="Hombres")

dta_h_weighted<- subset(dta, sexo==1)
rii_sedentario_h_weighted <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, weights = factor, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Sedentarismo", 
         sexo="Hombres")

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
  mutate(risk_factor="Sedentarismo", 
         sexo="Mujeres")


dta_m_weighted<- subset(dta, sexo==0)
rii_sedentario_m_weighted <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=sedentario~education_3_tr+edad, weights = factor, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Sedentarismo", 
         sexo="Mujeres")


rii_sedentario_education <- 
  rii_sedentario_overall %>%
  rbind(rii_sedentario_h) %>%
  rbind(rii_sedentario_m)

rii_sedentario_education_weighted <- 
  rii_sedentario_overall_weighted %>%
  rbind(rii_sedentario_h_weighted) %>%
  rbind(rii_sedentario_m_weighted)


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
  mutate(risk_factor="Sedentarismo", 
         sexo="Overall")

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
  mutate(risk_factor="Sedentarismo", 
         sexo="Hombres")

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
  mutate(risk_factor="Sedentarismo", 
         sexo="Mujeres")

sii_sedentario_education <- sii_sedentario_overall %>%
  rbind(sii_sedentario_h) %>%
  rbind(sii_sedentario_m)

save(sii_sedentario_education, file="Physical Activity/sii_sedentario_education.RData")

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
  mutate(fr="Sedentarismo")

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
  mutate(fr="Sedentarismo")


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
  mutate(fr="Sedentarismo")

rii_sedentario_CCAA <- rii_sedentario_CCAA %>%
  rbind(rii_sedentario_CCAA_h) %>%
  rbind(rii_sedentario_CCAA_m)


##HALLAMOS ICC PARA CADA AÑO ENTRE LAS CCAAS##

#Creamos tabla

rii_ccaa_icc_2001 <- rii_sedentario_CCAA %>%
  filter(sexo=="Overall") %>% 
  filter(encuesta=="2001-01-01") %>% 
  select(rii, ccaa, encuesta) %>% 
  pivot_wider(names_from = encuesta, values_from = rii)

rii_ccaa_icc_2001 <- rii_ccaa_icc %>% 
  filter(encuesta=="01-01-2001")

results_icc_ccaa_2001 <- icc(rii_ccaa_icc, model = "twoway", type = "agreement", unit = "single", conf.level = 0.95)

results_icc_ccaa_2001

results_icc_ccaa <- results_icc_ccaa_2001 %>% 
  mutate(icc=value, 
         infci=lbound,
         supci=ubound,
         encuesta=2001)
  
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


#Hacemos figura de RII en sedentarismo comparando con y sin peso

fig_rii_sedentario_sinpeso <-  ggplot(rii_sedentario_education, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  labs(x="", y="RII (95% CI)")+
  scale_y_continuous(trans="log",
                     breaks=c(-1, 0, 1, 1.5, 2, 2.5, 3, 4, 5))+
  scale_x_continuous(breaks=c(2001, 2003, 2006, 2009, 2011, 2014, 2017, 2020))+
  ylim(-1, 5.5)+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Inequalities on sedentarism between 2001-2020",
        subtitle = "(Unweighted)")

fig_rii_sedentario_sinpeso


fig_rii_sedentario_weighted <-  ggplot(rii_sedentario_education_weighted, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  labs(x="", y="RII (95% CI)")+
  scale_y_continuous(trans="log",
                     breaks=c(-1, 0, 1, 1.5, 2, 2.5, 3, 4, 5))+
  scale_x_continuous(breaks=c(2001, 2003, 2006, 2009, 2011, 2014, 2017, 2020))+
  ylim(-1, 5.5)+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Inequalities on sedentarism between 2001-2020",
        subtitle = "(Weighted)")

fig_rii_sedentario_weighted

plot_weighted <- plot_grid(fig_rii_sedentario_sinpeso, fig_rii_sedentario_weighted, nrow = 2)

plot_weighted

save(rii_sedentario_education, file="Physical Activity/rii_sedentario_education.RData")


####FIGURA RII (EDUCACIÓN) HOMBRES, MUJERES, OVERALL####

fig_rii_junto <-  ggplot(rii_sedentario_education, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  labs(x="", y="RII (95% CI)")+
  scale_y_continuous(trans="log",
                     breaks=c(-1, 0, 1, 1.5, 2, 2.5, 3, 4, 5))+
  ylim(-1, 5.5)+
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
                      ylim(-20, 90)+
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

sedentario_educacion$exp <- factor(sedentario_educacion$exp, levels = c("sii", "rii"),
                                   labels = c("SII", "RII"))

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

#Comunidades Autónomas Mapa RII Sedentarismo#

shapefile_ccaa <- rgdal::readOGR("Physical Activity/Maps/Comunidades_Autonomas_ETRS89_30N.shp") # Leemos los datos de capa

data_ccaa <- broom::tidy(shapefile_ccaa) # Los convertimos en un dataframe

#Test de mapa de ejjjpañita

ggplot(data_ccaa, aes(x= long, y = lat, group = group)) + # Hacemos el mapa
  geom_polygon(fill = "violetred4", color = "white") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


data_ccaa$id<-as.character(data_ccaa$id)

rii_sedentario_map<-rii_sedentario_CCAA %>% 
  mutate(id=ccaa-1)

data_ccaa_map<-rii_sedentario_map %>% 
  mutate(id=as.character(id)) %>%
  right_join(data_ccaa, by= "id")%>% 
  filter(id!=17 & id !=18)

data_ccaa_map$encuesta <- factor(data_ccaa_map$encuesta, levels = c("2001-01-01", "2003-01-01", "2006-01-01", "2009-01-01", "2011-01-01", "2014-01-01", "2017-01-01", "2020-01-01"),
                                 labels=c("2001", "2003", "2006", "2009", "2011", "2014", "2017", "2020"))

save(data_ccaa_map, file="Physical Activity/data_map.RData")

sedentarismo_map <- ggplot(data_ccaa_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=rii), color= "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs( title = "Desigualdades en sedentarismo por Comunidades Autónomas en 2020",
        subtitle = "Unidades: Relative Index of Inequality",
        caption = "Fuente: Mis cojones",
        fill = "IRR Sedentarism") +
  facet_wrap(~encuesta, nrow = 2, ncol=4)+
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
sedentarismo_map  

## Creamos Tabla 1 ##


label(dta$edad_pura) <- "Age"
label(dta$sexo) <- "Sex"
label(dta$sedentario) <- "Sedentarismo"
dta$sedentario <- factor(dta$sedentario, levels = c(0, 1),
                         labels = c("No sedentario", "sedentario"))
label(dta$education_3_tr) <- "Nivel Educativo"
label(dta$education_3) <- "Nivel Educativo"
dta$education_3 <- factor(dta$education_3, levels = c(1, 2, 3),
                                          labels = c("Nivel Educativo Bajo", "Nivel Educativo Medio", "Nivel educativo Alto"))
dta$sexo <- factor(dta$sexo, levels = c(0, 1),
                   labels = c("Mujeres", "Hombres"))

table1(~ edad_pura + education_3 + sedentario | sexo,
       render.continuous=c(.="Median", "(IQR)"="(Q1, Q3)"),
       render.categorical=c(.="Freq (Pct%)"),
       data=dta)

#Tendencias descriptivas de sedentarismo comparando nivel educativo y clase social

desc_sedentario <- dta %>%
  group_by(education_3, encuesta) %>%
  summarize(
    n = sum(sedentario == 1), 
    n_t = sum(sedentario <2))%>%
  pivot_wider(
    id_cols = education_3,
    names_from = encuesta,
    values_from = c(n),
    names_sep = "_")

desc_sedentario_fig <- dta %>%
  group_by(education_3, encuesta) %>%
  summarize(
    Sedentario = sum(sedentario == "sedentario"),
    Sano = sum(sedentario=="No sedentario")) %>% 
  mutate(Sedentarismo = Sedentario / (Sedentario + Sano) * 100)

save(desc_sedentario_fig, file = "Physical Activity/desc_sedentario_fig.RData")

#Hacemos figura para el descriptivo de sedentarismo

fig_des_sedentario <-  ggplot(desc_sedentario_fig, aes(x=encuesta, y=Sedentarismo)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(aes(color=as.factor(education_3)))+
  scale_y_continuous(breaks = c(25, 50, 75, 100))+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2009, 2011, 2014, 2017, 2020))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Inequalities on sedentarism between 2001-2020",
        subtitle = "(Including 2009)")

fig_des_sedentario

fig_des_sedentario_sin2009 <-  ggplot(subset(desc_sedentario_fig, encuesta !=2009), aes(x=encuesta, y=Sedentarismo)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(aes(color=education_3))+
  scale_y_continuous(breaks = c(25, 50, 75, 100), limits = c(0, 100))+
    scale_x_continuous(breaks = c(2001, 2003, 2006, 2011, 2014, 2017, 2020))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()+
  labs( title = "Inequalities on sedentarism between 2001-2020",
        subtitle = "(Excluding 2009)")

fig_des_sedentario_sin2009

plot2009_sin2009 <- plot_grid(fig_des_sedentario, fig_des_sedentario_sin2009, nrow = 2)

plot2009_sin2009

ggsave(descriptivo_sedentarismo.png)

##############

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
  mutate(fr="Sedentarismo")
  
  
############################################################################################################
##################################OBTENEMOS VARIABLES iPAQ PARA 2009-2020###################################
############################################################################################################


###################################### 2009 #################################################################
#Cargamos encuesta adultos
load("2009/eese2009.RData")

##Comprobamos que esté todo ok##
table(eese_2009$PE3)
table(eese_2009$PE4_1)
table(eese_2009$PE4_2)
table(eese_2009$PE1)
table(eese_2009$PE2_1)
table(eese_2009$PE2_2)
table(eese_2009$PE5)
table(eese_2009$PE6_1)
table(eese_2009$PE6_2)

#convertir a numérica 

#Obtenemos minutos/día de cada intensidad de actividad#
eese_2009 <- eese_2009 %>% 
  mutate(moderada_dias=as.numeric(eese_2009$PE3)) %>% 
  mutate(moderada_horas=as.numeric(eese_2009$PE4_1)) %>% 
  mutate(moderada_min=as.numeric(eese_2009$PE4_2)) %>% 
  mutate(intensa_dias=as.numeric(eese_2009$PE1)) %>% 
  mutate(intensa_horas=as.numeric(eese_2009$PE2_1)) %>% 
  mutate(intensa_min=as.numeric(eese_2009$PE2_2)) %>% 
  mutate(caminar_dias=as.numeric(eese_2009$PE5)) %>% 
  mutate(caminar_horas=as.numeric(eese_2009$PE6_1)) %>% 
  mutate(caminar_min=as.numeric(eese_2009$PE6_2))

#Corregimos nulos en minutos y/o horas si el día se recoge como "0"

eese_2009 <- eese_2009 %>% 
  mutate(moderada_horas = ifelse(moderada_dias == 0, 0, moderada_horas),
         moderada_min = ifelse(moderada_dias == 0, 0, moderada_min),
         intensa_horas = ifelse(intensa_dias == 0, 0, intensa_horas),
         intensa_min = ifelse(intensa_dias == 0, 0, intensa_min),
         caminar_horas = ifelse(caminar_dias == 0, 0, caminar_horas),
         caminar_min = ifelse(caminar_dias == 0, 0, caminar_min))

sum(is.na(eese_2009$moderada_horas) | is.na(eese_2009$moderada_min) | is.na(eese_2009$intensa_horas) | is.na(eese_2009$intensa_min) | is.na(eese_2009$caminar_horas) | is.na(eese_2009$caminar_min))

#Pasamos a NA los no sabe no contesta
eese_2009 <- eese_2009 %>% 
  mutate(moderada_dias = ifelse(moderada_dias>7, NA, moderada_dias),
         moderada_horas = ifelse(moderada_horas>90, NA, moderada_horas),
         moderada_min = ifelse(moderada_min>90, NA, moderada_min),
         intensa_dias = ifelse(intensa_dias>7, NA, intensa_dias),
         intensa_horas = ifelse(intensa_horas>90, NA, intensa_horas),
         intensa_min = ifelse(intensa_min>90, NA, intensa_min),
         caminar_dias = ifelse(caminar_dias>7, NA, caminar_dias),
         caminar_horas = ifelse(caminar_horas>90, NA, caminar_horas),
         caminar_min=ifelse(caminar_min>90, NA, caminar_min))


#Cuantificamos actividad física  
eese_2009 <- eese_2009 %>%
  mutate(moderada_min_dia = (moderada_horas*60)+moderada_min,
         intensa_min_dia = (intensa_horas*60)+intensa_min,
         caminar_min_dia = (caminar_horas*60)+caminar_min)

#Aplicamos truncation rules
eese_2009 <- eese_2009 %>%
  mutate(moderada_min_dia = ifelse(moderada_min_dia>180, 180, moderada_min_dia),
         intensa_min_dia = ifelse(intensa_min_dia>180, 180, intensa_min_dia),
         caminar_min_dia = ifelse(caminar_min_dia>180, 180, caminar_min_dia))

#Calculamos minutos de actividad física a la semana por intensidad
eese_2009 <- eese_2009 %>%
  mutate(moderada_semana = moderada_min_dia*moderada_dias,
         intensa_semana = intensa_min_dia*intensa_dias,
         caminar_semana = caminar_min_dia*caminar_dias,
         moderosa_semana = moderada_semana+intensa_semana)

#Calculamos METs/min/semana
eese_2009 <- eese_2009 %>%
  mutate(mets_semana = (moderada_semana*4)+(intensa_semana*8)+(caminar_semana*3.3))

#Calculamos cumple/no cumple recomendaciones OMS

eese_2009$recomendaciones <- 0
eese_2009$recomendaciones[eese_2009$intensa_semana>74 | eese_2009$moderada_semana>149 | eese_2009$moderosa_semana>224] <- 1
eese_2009$recomendaciones[eese_2009$moderada_semana>299 | eese_2009$intensa_semana>149 | eese_2009$moderosa_semana>449] <- 2

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
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura,
         recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, 
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          imc, obesity, sobrepeso, smoking, sedentario) %>%
  mutate(encuesta=2009)

save(eese_2009, file = "Physical Activity/ense2009_pa.RData")

###################################### 2011 #################################################################
#Cargamos encuesta adultos
load("2011/ense2011.RData")

##Comprobamos que esté todo ok##
table(ense_2011$U130_3)
table(ense_2011$U130_4a)
table(ense_2011$U130_4b)
table(ense_2011$U130_1)
table(ense_2011$U130_2a)
table(ense_2011$U130_2b)
table(ense_2011$U130_5)
table(ense_2011$U130_6a)
table(ense_2011$U130_6b)

#convertimos a numérica y obtenemos minutos/día de cada intensidad de actividad#
ense_2011 <- ense_2011 %>% 
  mutate(moderada_dias=as.numeric(ense_2011$U130_3)) %>% 
  mutate(moderada_horas=as.numeric(ense_2011$U130_4a)) %>% 
  mutate(moderada_min=as.numeric(ense_2011$U130_4b)) %>% 
  mutate(intensa_dias=as.numeric(ense_2011$U130_1)) %>% 
  mutate(intensa_horas=as.numeric(ense_2011$U130_2a)) %>% 
  mutate(intensa_min=as.numeric(ense_2011$U130_2b)) %>% 
  mutate(caminar_dias=as.numeric(ense_2011$U130_5)) %>% 
  mutate(caminar_horas=as.numeric(ense_2011$U130_6a)) %>% 
  mutate(caminar_min=as.numeric(ense_2011$U130_6b))

#Corregimos nulos en minutos y/o horas si el día se recoge como "0"

ense_2011 <- ense_2011 %>% 
  mutate(moderada_horas = ifelse(moderada_dias == 0, 0, moderada_horas),
         moderada_min = ifelse(moderada_dias == 0, 0, moderada_min),
         intensa_horas = ifelse(intensa_dias == 0, 0, intensa_horas),
         intensa_min = ifelse(intensa_dias == 0, 0, intensa_min),
         caminar_horas = ifelse(caminar_dias == 0, 0, caminar_horas),
         caminar_min = ifelse(caminar_dias == 0, 0, caminar_min))

sum(is.na(ense_2011$moderada_horas) | is.na(ense_2011$moderada_min) | is.na(ense_2011$intensa_horas) | is.na(ense_2011$intensa_min) | is.na(ense_2011$caminar_horas) | is.na(ense_2011$caminar_min))


#Pasamos a NA los no sabe no contesta
ense_2011 <- ense_2011 %>% 
  mutate(moderada_dias = ifelse(moderada_dias>7, NA, moderada_dias),
         moderada_horas = ifelse(moderada_horas>90, NA, moderada_horas),
         moderada_min = ifelse(moderada_min>90, NA, moderada_min),
         intensa_dias = ifelse(intensa_dias>7, NA, intensa_dias),
         intensa_horas = ifelse(intensa_horas>90, NA, intensa_horas),
         intensa_min = ifelse(intensa_min>90, NA, intensa_min),
         caminar_dias = ifelse(caminar_dias>7, NA, caminar_dias),
         caminar_horas = ifelse(caminar_horas>90, NA, caminar_horas),
         caminar_min=ifelse(caminar_min>90, NA, caminar_min))

table(ense_2011$moderada_dias, useNA = 'always')
table(ense_2011$moderada_horas, useNA = 'always')
table(ense_2011$moderada_min, useNA = 'always')

table(ense_2011$U130_3, useNA = 'always')
table(ense_2011$U130_4a, useNA = 'always')
table(ense_2011$U130_4b, useNA = 'always')

#Cuantificamos actividad física  
ense_2011 <- ense_2011 %>% 
  mutate(moderada_min_dia = (moderada_horas*60)+moderada_min,
         intensa_min_dia = (intensa_horas*60)+intensa_min,
         caminar_min_dia = (caminar_horas*60)+caminar_min)

#Aplicamos truncation rules
ense_2011 <- ense_2011 %>% 
  mutate(moderada_min_dia = ifelse(moderada_min_dia>180, 180, moderada_min_dia),
         intensa_min_dia = ifelse(intensa_min_dia>180, 180, intensa_min_dia),
         caminar_min_dia = ifelse(caminar_min_dia>180, 180, caminar_min_dia))

#Calculamos minutos de actividad física a la semana por intensidad
ense_2011 <- ense_2011 %>% 
  mutate(moderada_semana = moderada_min_dia*moderada_dias,
         intensa_semana = intensa_min_dia*intensa_dias,
         caminar_semana = caminar_min_dia*caminar_dias,
         moderosa_semana = moderada_semana+intensa_semana)

#Calculamos METs/min/semana
ense_2011 <- ense_2011 %>% 
  mutate(mets_semana = (moderada_semana*4)+(intensa_semana*8)+(caminar_semana*3.3))

#Calculamos cumple/no cumple recomendaciones OMS

ense_2011$recomendaciones <- 0
ense_2011$recomendaciones[ense_2011$intensa_semana>74 | ense_2011$moderada_semana>149 | ense_2011$moderosa_semana>224] <- 1
ense_2011$recomendaciones[ense_2011$moderada_semana>299 | ense_2011$intensa_semana>149 | ense_2011$moderosa_semana>449] <- 2

#Comprobaciones
table((ense_2011$moderada_semana!='NA' & ense_2011$intensa_semana != 'NA' & ense_2011$moderosa_semana != 'NA') & ense_2011$recomendaciones=='NA')
table(ense_2011$recomendaciones, useNA = 'always')
table(ense_2011$intensa_semana>74 | ense_2011$moderada_semana>149 | ense_2011$moderosa_semana>224, useNA= 'always')
table(ense_2011$moderada_semana>299 | ense_2011$intensa_semana>149 | ense_2011$moderosa_semana>449, useNA= 'always')
table((ense_2011$moderada_semana=='NA' | ense_2011$intensa_semana == 'NA' | ense_2011$moderosa_semana == 'NA'))

table(ense_2011$moderada_semana, useNA='always')
table(ense_2011$intensa_semana, useNA='always')
table(ense_2011$moderosa_semana, useNA='always')

#Creo variable sedentario y lo añado al resto de variables del joint

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
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura,
         recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, sedentario, fruta_verdura) %>%
  mutate(encuesta=2011)

save(ense_2011, file = "Physical Activity/ense2011_pa.RData")

###################################### 2017 #################################################################
#Cargamos encuesta adultos
load("2017/ense2017.RData")

##Comprobamos que esté todo ok##
table(ense_2017$T115)
table(ense_2017$T116_1)
table(ense_2017$T116_2)
table(ense_2017$T113)
table(ense_2017$T114_1)
table(ense_2017$T114_2)
table(ense_2017$T117)
table(ense_2017$T118_1)
table(ense_2017$T118_2)

#convertimos a numérica y obtenemos minutos/día de cada intensidad de actividad#
ense_2017 <- ense_2017 %>% 
  mutate(moderada_dias=as.numeric(ense_2017$T115)) %>% 
  mutate(moderada_horas=as.numeric(ense_2017$T116_1)) %>% 
  mutate(moderada_min=as.numeric(ense_2017$T116_2)) %>% 
  mutate(intensa_dias=as.numeric(ense_2017$T113)) %>% 
  mutate(intensa_horas=as.numeric(ense_2017$T114_1)) %>% 
  mutate(intensa_min=as.numeric(ense_2017$T114_2)) %>% 
  mutate(caminar_dias=as.numeric(ense_2017$T117)) %>% 
  mutate(caminar_horas=as.numeric(ense_2017$T118_1)) %>% 
  mutate(caminar_min=as.numeric(ense_2017$T118_2))

#Corregimos nulos en minutos y/o horas si el día se recoge como "0"

ense_2017 <- ense_2017 %>% 
  mutate(moderada_horas = ifelse(moderada_dias == 0, 0, moderada_horas),
         moderada_min = ifelse(moderada_dias == 0, 0, moderada_min),
         intensa_horas = ifelse(intensa_dias == 0, 0, intensa_horas),
         intensa_min = ifelse(intensa_dias == 0, 0, intensa_min),
         caminar_horas = ifelse(caminar_dias == 0, 0, caminar_horas),
         caminar_min = ifelse(caminar_dias == 0, 0, caminar_min))

sum(is.na(ense_2017$moderada_horas) | is.na(ense_2017$moderada_min) | is.na(ense_2017$intensa_horas) | is.na(ense_2017$intensa_min) | is.na(ense_2017$caminar_horas) | is.na(ense_2017$caminar_min))


#Pasamos a NA los no sabe no contesta
ense_2017 <- ense_2017 %>% 
  mutate(moderada_dias = ifelse(moderada_dias>7, NA, moderada_dias),
         moderada_horas = ifelse(moderada_horas>90, NA, moderada_horas),
         moderada_min = ifelse(moderada_min>90, NA, moderada_min),
         intensa_dias = ifelse(intensa_dias>7, NA, intensa_dias),
         intensa_horas = ifelse(intensa_horas>90, NA, intensa_horas),
         intensa_min = ifelse(intensa_min>90, NA, intensa_min),
         caminar_dias = ifelse(caminar_dias>7, NA, caminar_dias),
         caminar_horas = ifelse(caminar_horas>90, NA, caminar_horas),
         caminar_min=ifelse(caminar_min>90, NA, caminar_min))

table(ense_2017$moderada_dias, useNA = 'always')
table(ense_2017$moderada_horas, useNA = 'always')
table(ense_2017$moderada_min, useNA = 'always')

table(ense_2017$T115, useNA = 'always')
table(ense_2017$T116_1, useNA = 'always')
table(ense_2017$T116_2, useNA = 'always')

#Cuantificamos actividad física  
ense_2017 <- ense_2017 %>% 
  mutate(moderada_min_dia = (moderada_horas*60)+moderada_min,
         intensa_min_dia = (intensa_horas*60)+intensa_min,
         caminar_min_dia = (caminar_horas*60)+caminar_min)

#Aplicamos truncation rules
ense_2017 <- ense_2017 %>% 
  mutate(moderada_min_dia = ifelse(moderada_min_dia>180, 180, moderada_min_dia),
         intensa_min_dia = ifelse(intensa_min_dia>180, 180, intensa_min_dia),
         caminar_min_dia = ifelse(caminar_min_dia>180, 180, caminar_min_dia))

#Calculamos minutos de actividad física a la semana por intensidad
ense_2017 <- ense_2017 %>% 
  mutate(moderada_semana = moderada_min_dia*moderada_dias,
         intensa_semana = intensa_min_dia*intensa_dias,
         caminar_semana = caminar_min_dia*caminar_dias,
         moderosa_semana = moderada_semana+intensa_semana)

#Calculamos METs/min/semana
ense_2017 <- ense_2017 %>% 
  mutate(mets_semana = (moderada_semana*4)+(intensa_semana*8)+(caminar_semana*3.3))

#Calculamos cumple/no cumple recomendaciones OMS

ense_2017$recomendaciones <- 0
ense_2017$recomendaciones[ense_2017$intensa_semana>74 | ense_2017$moderada_semana>149 | ense_2017$moderosa_semana>224] <- 1
ense_2017$recomendaciones[ense_2017$moderada_semana>299 | ense_2017$intensa_semana>149 | ense_2017$moderosa_semana>449] <- 2

#Comprobaciones
table((ense_2017$moderada_semana!='NA' & ense_2017$intensa_semana != 'NA' & ense_2017$moderosa_semana != 'NA') & ense_2017$recomendaciones=='NA')
table(ense_2017$recomendaciones, useNA = 'always')
table(ense_2017$intensa_semana>74 | ense_2017$moderada_semana>149 | ense_2017$moderosa_semana>224, useNA= 'always')
table(ense_2017$moderada_semana>299 | ense_2017$intensa_semana>149 | ense_2017$moderosa_semana>449, useNA= 'always')
table((ense_2017$moderada_semana=='NA' | ense_2017$intensa_semana == 'NA' | ense_2017$moderosa_semana == 'NA'))

table(ense_2017$moderada_semana, useNA='always')
table(ense_2017$intensa_semana, useNA='always')
table(ense_2017$moderosa_semana, useNA='always')

#Creo variable sedentario y lo añado al resto de variables del joint

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
         col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta, verdura, fruta_verdura, 
         recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana) %>%
  filter(edad>17) %>%
  distinct() %>%
  drop_na(id, factor, edad, sexo, ccaa, nacionalidad, migration, clase, clase_tr,
          education_3, education_3_tr, education_5, education_5_tr, diabetes, hta, 
          col, imc, obesity, sobrepeso, smoking, alcohol, sedentario, fruta_verdura) %>%
  mutate(encuesta=2017)

save(ense_2017, file = "Physical Activity/ense2017_pa.RData")

#Unimos y generamos base de datos de PA de 2009, 2011, 2017

dta_pa <- eese_2009 %>%
  rbind(ense_2011) %>%
  rbind(ense_2017)

#Paso a NA en recomendaciones las observaciones que tienen NA en alguna de sus predictoras

dta_pa <- dta_pa %>% 
  mutate(recomendaciones = ifelse(is.na(moderada_semana) | is.na(moderosa_semana) | is.na(intensa_semana), NA, recomendaciones))

save(dta_pa, file = "Physical Activity/joined_PA_dta.RData")


#eese_2009 <-  eese_2009 %>% 
#  drop_na(recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana)

#ense_2011 <-  ense_2011 %>% 
#  drop_na(recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana)

#ense_2017 <-  ense_2017 %>% 
#  drop_na(recomendaciones, caminar_semana, moderada_semana, intensa_semana, moderosa_semana, mets_semana)


##Probamos gráficos RRI Educación - Cumple/No Cumple Recomendaciones

#Centramos edad
dta_pa <- dta_pa %>%
  mutate(edad=scale(edad, center=T, scale=F))

#RII METs AF en el Tiempo Libre

rii_mets_overall <- dta_pa %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=mets_semana~education_3_tr+edad+sexo, data=.x)), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="METs semanales", 
         sexo="Overall")


# RII EDUCACIÓN HOMBRE - RECOMENDACIONES

dta_h<- subset(dta_pa, sexo==1)
rii_mets_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=mets_semana~education_3_tr+edad, data=.x)), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="METs semanales", 
         sexo="Hombres")

# RII EDUCACIÓN MUJER - RECOMENDACIONES

dta_m<- subset(dta_pa, sexo==0)
rii_mets_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=mets_semana~education_3_tr+edad, data=.x)), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="METs semanales", 
         sexo="Mujeres")

rii_mets_educacion <- rii_mets_overall %>% 
  rbind(rii_mets_h) %>% 
  rbind(rii_mets_m)

#Dicotomizo recomendaciones OMS e invierto (0= cumple, 1=no cumple) para igualar a sedentario

dta_pa$recomendaciones[dta_pa$recomendaciones>0] <- 2
dta_pa$recomendaciones[dta_pa$recomendaciones<2] <- 1
dta_pa$recomendaciones[dta_pa$recomendaciones>1] <- 0

# RII EDUCACIÓN OVERALL - RECOMENDACIONES

rii_recomendaciones_overall <- dta_pa %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=recomendaciones~education_3_tr+edad+sexo, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Recomendaciones OMS", 
         sexo="Overall")


# RII EDUCACIÓN HOMBRE - RECOMENDACIONES

dta_h<- subset(dta_pa, sexo==1)
rii_recomendaciones_h <- dta_h %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=recomendaciones~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Recomendaciones OMS", 
         sexo="Hombres")

# RII EDUCACIÓN MUJER - RECOMENDACIONES

dta_m<- subset(dta_pa, sexo==0)
rii_recomendaciones_m <- dta_m %>%
  nest(data=-encuesta) %>%
  mutate(model=map(data, ~glm(formula=recomendaciones~education_3_tr+edad, data=.x, 
                              family="poisson")), 
         tidied=map(model, tidy)) %>%
  unnest(tidied) %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="education_3_tr") %>% 
  select(rii, rii_infci, rii_supci, encuesta) %>% 
  mutate(risk_factor="Recomendaciones OMS", 
         sexo="Mujeres")

rii_recomendaciones_educacion <- rii_recomendaciones_overall %>% 
  rbind(rii_recomendaciones_h) %>% 
  rbind(rii_recomendaciones_m)



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

fig_rii_WHO_junto <-  ggplot(rii_recomendaciones_educacion, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
                      geom_hline(yintercept = 1, lty=2)+
                      geom_ribbon(alpha=0.3, aes(fill=sexo))+
                      geom_line(aes(color=sexo)) +
                      scale_y_continuous(trans="log")+
                      scale_x_continuous(breaks = c(2009, 2011, 2017))+
                      labs(x="", y="RII (95% CI)")+
                      theme_bw()+
                      theme_fis+
                      theme()

fig_rii_WHO_junto


fig_rii_WHO_separado <-  ggplot(rii_recomendaciones_educacion, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  facet_grid(cols=vars(sexo), scales = "free_y") +
  scale_y_continuous(trans="log")+
  scale_x_continuous(breaks = c(2009, 2011, 2017))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()

fig_rii_WHO_separado



fig_rii_mets_separado <-  ggplot(rii_mets_educacion, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  facet_grid(cols=vars(sexo), scales = "free_y") +
  scale_y_continuous(trans="log")+
  scale_x_continuous(breaks = c(2009, 2011, 2017))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()

fig_rii_mets_separado

## TODO JUNTO AHORA (SEDENTARISMO, METs Y RECOMENDACIONES)

rii_pa_educacion <- rii_recomendaciones_educacion %>%
  rbind(rii_sedentario_education)

save(rii_pa_educacion, file="Physical Activity/rii_pa_educacion.RData")

fig_rii_PA <-  ggplot(rii_pa_educacion, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  facet_grid(cols=vars(sexo), rows = vars(risk_factor), scales = "free_y") +
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2009, 2011, 2014, 2017, 2020))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()


fig_rii_PA

fig_rii_PA <-  ggplot(rii_pa_educacion, aes(x=encuesta, y=rii, ymin=rii_infci, ymax=rii_supci)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_ribbon(alpha=0.3, aes(fill=sexo))+
  geom_line(aes(color=sexo)) +
  facet_grid(cols=vars(sexo), rows = vars(risk_factor), scales = "free_y") +
  scale_y_continuous(trans="log")+
  scale_x_continuous(breaks = c(2001, 2003, 2006, 2009, 2011, 2014, 2017, 2020))+
  labs(x="", y="RII (95% CI)")+
  theme_bw()+
  theme_fis+
  theme()


