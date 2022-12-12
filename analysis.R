library(plyr)
library(tidyverse)
library(scales)
library(broom)
library(survey)
library(lubridate)
library(lme4)
library(broom.mixed)
library(mixedup)


rm(list=ls())


load("joined_dta.RData")

load("2003/ense2003_clean.RData")
load("2006/ense2006_clean.RData")
load("2011/ense2011_clean.RData")
load("2014/eese2014_clean.RData")
load("2017/ense2017_clean.RData")
load("2020/eese2020_clean.RData")

#Lista CCAA
ccaas <- read_delim("ccaas.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)



#asumimos que hay una desigualdad que cambia en el tiempo. Observaciones -> ccaa -> tiempo
# Random slopes la variable de clase social
# No olvidar weights



prueba <- glmer (diabetes~clase_tr+edad+sexo+(1|ccaa)+(1|encuesta), data=dta,
                 family="poisson")



test <- prueba %>%
  tidy(effects = "fixed", exponentiate=TRUE, conf.int=TRUE)



coefs_encuesta <- glmer(diabetes~clase_tr+edad+sexo+(1+ clase_tr|encuesta), data=dta,
                            family="poisson")


coefs_encuesta <- coefs_encuesta %>%
  extract_random_coefs(re="encuesta") %>%
  mutate(rii=exp(value), 
         infci=exp(value-1.96*se),
         supci=exp(value+1.96*se)) %>% 
  filter(effect=="clase_tr") %>% 
  select(rii, infci, supci, group) %>% 
  mutate(sexo="Overall")



coefs_encuesta_ccaa <- glmer(diabetes~clase_tr+edad+sexo+(1+clase_tr|ccaa) + (1+clase_tr|ccaa: encuesta), data=dta,
                        family="poisson")


coefs_encuesta_ccaa <- coefs_encuesta_ccaa %>%
  extract_random_coefs(re="ccaa:encuesta") %>%
  mutate(rii=exp(value), 
         infci=exp(value-1.96*se),
         supci=exp(value+1.96*se)) %>% 
  filter(effect=="clase_tr") %>% 
  select(rii, infci, supci, group) %>% 
  mutate(sexo="Overall") %>%
  separate(group, c('ccaa', 'encuesta')) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L), 
         ccaa=as.numeric(ccaa)) %>%
  left_join(ccaas)



ccaa_overal <- ggplot(coefs_encuesta_ccaa, aes(encuesta, rii)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(alpha=0.4)+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(5), limits = c(0.5, 2.8))+
  scale_x_date(breaks = as.Date(c("2003-01-01", "2006-01-01", "2011-01-01", 
                                  "2014-01-01", "2017-01-01", "2020-01-01")), 
               date_labels = "%y")+
  geom_point(size=2, position=position_dodge(width = 0.75), alpha=0.6) +
  geom_ribbon(aes(ymin=infci, ymax=supci), fill="darkolivegreen4", alpha=0.3) +
  ylab("Relative Index of Inequality (RII)")+
  xlab("Año de la encuesta")+
  theme_bw()+
  facet_wrap(~nombre_notilde, ncol=4)+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15),
        axis.title = element_text(size = 13))+
  ggtitle("Desigualdades en diabetes por clase social 2003-2020")
ccaa_overal





######################################################################################
# Relative Index of Inequality in diabetes
svy <- svydesign(ids=~1,weights=~as.numeric(factor),data=ense_2003)
rii_2003 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
              family=poisson(link="log"))
rii_2003 <- rii_2003  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2003)
rii_2003


rii_2003_ccaa <- ense_2003 %>% 
  nest_by (ccaa) %>%
  mutate(design = map(., ~svydesign(ids=~1, data = .x, weights = ~ factor)),
         model = map(.x = design,
                     .f = ~ svyglm(diabetes ~ clase_tr+edad+sexo,
                                   family = poisson(link="log")),
                                   design = .x)) %>% 
  mutate(model_tidy = map(model, tidy)) %>% 
  select(ccaa, model_tidy)%>%
  ungroup()%>%
  unnest(model_tidy)

rii_2003_ccaa <-ddply(ense_2003, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2003_ccaa <- rii_2003_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2003) %>%
  left_join(ccaas)



svy <- svydesign(ids=~1,weights=~as.numeric(factor),data=subset(ense_2003,sexo==1))
rii_2003_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                  family=poisson(link="log"))
rii_2003_hombres <- rii_2003_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2003)
rii_2003_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2003,sexo==6))
rii_2003_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2003_mujeres <- rii_2003_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2003)
rii_2003_mujeres




ense_2006$factor=as.numeric(ense_2006$FACTOR.x)
svy <- svydesign(ids=~1,weights=~as.numeric(factor),data=ense_2006)
rii_2006 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2006 <- rii_2006  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2006)
rii_2006

rii_2006_ccaa <-ddply(ense_2006, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2006_ccaa <- rii_2006_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2006) %>%
  left_join(ccaas)


svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2006,sexo==1))
rii_2006_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2006_hombres <- rii_2006_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2006)
rii_2006_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2006,sexo==6))
rii_2006_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2006_mujeres <- rii_2006_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2006)
rii_2006_mujeres




ense_2011$factor=as.numeric(ense_2011$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2011)
rii_2011 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2011 <- rii_2011  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2011)
rii_2011


rii_2011_ccaa <-ddply(ense_2011, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2011_ccaa <- rii_2011_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2011) %>%
  left_join(ccaas)


svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2011,sexo==1))
rii_2011_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2011_hombres <- rii_2011_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2011)
rii_2011_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2011,sexo==2))
rii_2011_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2011_mujeres <- rii_2011_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2011)
rii_2011_mujeres



eese_2014$factor=as.numeric(eese_2014$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=eese_2014)
rii_2014 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2014 <- rii_2014  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2014)
rii_2014


rii_2014_ccaa <-ddply(eese_2014, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2014_ccaa <- rii_2014_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2014) %>%
  left_join(ccaas)



svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2014,sexo==1))
rii_2014_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2014_hombres <- rii_2014_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2014)
rii_2014_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2014,sexo==2))
rii_2014_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2014_mujeres <- rii_2014_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2014)
rii_2014_mujeres



ense_2017$factor=as.numeric(ense_2017$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2017)
rii_2017 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2017 <- rii_2017  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2017)
rii_2017


rii_2017_ccaa <-ddply(ense_2017, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2017_ccaa <- rii_2017_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2017) %>%
  left_join(ccaas)



svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2017,sexo==1))
rii_2017_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2017_hombres <- rii_2017_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2017)
rii_2017_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2017,sexo==2))
rii_2017_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2017_mujeres <- rii_2017_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2017)
rii_2017_mujeres





eese_2020$factor=as.numeric(eese_2020$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=eese_2020)
rii_2020 <-svyglm(diabetes~clase_tr+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2020 <- rii_2020  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2020)
rii_2020

rii_2020_ccaa <-ddply(eese_2020, .(ccaa), function(temp){
  #temp<-dta[dta$ccaa=="01",]
  svy <- svydesign(ids=~1,weights=~factor,data=temp)
  f<-as.formula(diabetes~clase_tr+edad+sexo)
  model<-svyglm(formula=f, design=svy,
                family=poisson(link="log"))
  coefs<-summary(model)$coefficients
  beta<-coefs["clase_tr", 1]
  se<-coefs["clase_tr", 2]
  data.frame(estimate=beta,std.error=se, stringsAsFactors = F)
})

rii_2020_ccaa <- rii_2020_ccaa %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  select(ccaa, rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2020) %>%
  left_join(ccaas)



svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2020,sexo==1))
rii_2020_hombres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2020_hombres <- rii_2020_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2020)
rii_2020_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2020,sexo==2))
rii_2020_mujeres <-svyglm(diabetes~clase_tr+edad, design=svy,
                          family=poisson(link="log"))
rii_2020_mujeres <- rii_2020_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2020)
rii_2020_mujeres



rii_diabetes= rii_2003 %>%
  rbind(rii_2003_hombres) %>%
  rbind(rii_2003_mujeres) %>%
  rbind(rii_2006) %>%
  rbind(rii_2006_hombres) %>%
  rbind(rii_2006_mujeres) %>%
  rbind(rii_2011) %>%
  rbind(rii_2011_hombres) %>%
  rbind(rii_2011_mujeres) %>%
  rbind(rii_2014) %>%
  rbind(rii_2014_hombres) %>%
  rbind(rii_2014_mujeres) %>%
  rbind(rii_2017) %>%
  rbind(rii_2017_hombres) %>%
  rbind(rii_2017_mujeres) %>%
  rbind(rii_2020) %>%
  rbind(rii_2020_hombres) %>%
  rbind(rii_2020_mujeres) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L))


rii_diabetes_ccaa <- rii_2003_ccaa %>%
  rbind(rii_2006_ccaa) %>%
  rbind(rii_2011_ccaa) %>%
  rbind(rii_2014_ccaa) %>%
  rbind(rii_2017_ccaa) %>%
  rbind(rii_2020_ccaa) %>%
  mutate(encuesta=ymd(encuesta, truncated = 2L))


base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

rii_diabetes_fig <- ggplot(rii_diabetes, aes(encuesta, rii)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(alpha=0.4)+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(5))+
  scale_x_date(breaks = as.Date(c("2003-01-01", "2006-01-01", "2011-01-01", 
                                  "2014-01-01", "2017-01-01", "2020-01-01")), 
               date_labels = "%y")+
  geom_point(size=2, position=position_dodge(width = 0.75), alpha=0.6) +
  geom_ribbon(aes(ymin=rii_infci, ymax=rii_supci), fill="darkolivegreen4", alpha=0.3) +
  ylab("Relative Index of Inequality (RII)")+
  xlab("Año de la encuesta")+
  theme_bw()+
  facet_grid(~sexo)+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15),
        axis.title = element_text(size = 13))+
  ggtitle("Desigualdades en diabetes por clase social 2003-2020")
rii_diabetes_fig  
  
  
  

rii_diabetes_fig_ccaa <- ggplot(rii_diabetes_ccaa, aes(encuesta, rii)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(alpha=0.4)+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(5))+
  scale_x_date(breaks = as.Date(c("2003-01-01", "2006-01-01", "2011-01-01", 
                                  "2014-01-01", "2017-01-01", "2020-01-01")), 
               date_labels = "%y")+
  geom_point(size=2, position=position_dodge(width = 0.75), alpha=0.6) +
  #geom_ribbon(aes(ymin=rii_infci, ymax=rii_supci), fill="darkolivegreen4", alpha=0.3) +
  ylab("Relative Index of Inequality (RII)")+
  xlab("Año de la encuesta")+
  theme_bw()+
  facet_wrap(~nombre_notilde, ncol=4)+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15),
        axis.title = element_text(size = 13))+
  ggtitle("Desigualdades en diabetes por clase social 2003-2020")
rii_diabetes_fig_ccaa



