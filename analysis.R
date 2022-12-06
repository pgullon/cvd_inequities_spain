library(tidyverse)
library(scales)
library(broom)
library(survey)
library(lubridate)

rm(list=ls())

load("2003/ense2003_clean.RData")
load("2006/ense2006_clean.RData")
load("2011/ense2011_clean.RData")
load("2014/eese2014_clean.RData")
load("2017/ense2017_clean.RData")
load("2020/eese2020_clean.RData")



######################################################################################
# Relative Index of Inequality in diabetes con Poisson y clase en 6 categorías 0 a 1
ense_2003$factor=as.numeric(ense_2003$FACTOR.x)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2003)
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

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2003,sexo==1))
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
svy <- svydesign(ids=~1,weights=~factor,data=ense_2006)
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



base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

rii_diabetes_fig <- ggplot(rii_diabetes, aes(encuesta, rii)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(alpha=0.4)+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(5))+
  scale_x_date(breaks = date_breaks("year"), date_labels = "%y")+
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
  
  
  
  





######################################################################################
# Relative Index of Inequality in diabetes con Poisson y clase en 2 categorías
ense_2003$factor=as.numeric(ense_2003$FACTOR.x)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2003)
rii_2003 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2003 <- rii_2003  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2003)
rii_2003

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2003,sexo==1))
rii_2003_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2003_hombres <- rii_2003_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2003)
rii_2003_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2003,sexo==6))
rii_2003_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2003_mujeres <- rii_2003_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2003)
rii_2003_mujeres




ense_2006$factor=as.numeric(ense_2006$FACTOR.x)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2006)
rii_2006 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2006 <- rii_2006  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2006)
rii_2006

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2006,sexo==1))
rii_2006_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2006_hombres <- rii_2006_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2006)
rii_2006_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2006,sexo==6))
rii_2006_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2006_mujeres <- rii_2006_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2006)
rii_2006_mujeres




ense_2011$factor=as.numeric(ense_2011$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2011)
rii_2011 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2011 <- rii_2011  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2011)
rii_2011

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2011,sexo==1))
rii_2011_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2011_hombres <- rii_2011_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2011)
rii_2011_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2011,sexo==2))
rii_2011_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2011_mujeres <- rii_2011_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2011)
rii_2011_mujeres



eese_2014$factor=as.numeric(eese_2014$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=eese_2014)
rii_2014 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2014 <- rii_2014  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2014)
rii_2014

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2014,sexo==1))
rii_2014_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2014_hombres <- rii_2014_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2014)
rii_2014_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2014,sexo==2))
rii_2014_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2014_mujeres <- rii_2014_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2014)
rii_2014_mujeres



ense_2017$factor=as.numeric(ense_2017$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=ense_2017)
rii_2017 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2017 <- rii_2017  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2017)
rii_2017

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2017,sexo==1))
rii_2017_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2017_hombres <- rii_2017_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2017)
rii_2017_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(ense_2017,sexo==2))
rii_2017_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2017_mujeres <- rii_2017_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2017)
rii_2017_mujeres





eese_2020$factor=as.numeric(eese_2020$FACTORADULTO)
svy <- svydesign(ids=~1,weights=~factor,data=eese_2020)
rii_2020 <-svyglm(diabetes~clase2+edad+sexo, design=svy,
                  family=poisson(link="log"))
rii_2020 <- rii_2020  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Overall") %>%
  mutate(encuesta=2020)
rii_2020

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2020,sexo==1))
rii_2020_hombres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2020_hombres <- rii_2020_hombres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Hombres") %>%
  mutate(encuesta=2020)
rii_2020_hombres

svy <- svydesign(ids=~1,weights=~factor,data=subset(eese_2020,sexo==2))
rii_2020_mujeres <-svyglm(diabetes~clase2+edad, design=svy,
                          family=poisson(link="log"))
rii_2020_mujeres <- rii_2020_mujeres  %>% tidy %>%
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase2") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(sexo="Mujeres") %>%
  mutate(encuesta=2020)
rii_2020_mujeres



rii_diabetes2= rii_2003 %>%
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



base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

rii_diabetes_fig2 <- ggplot(rii_diabetes2, aes(encuesta, rii)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_line(alpha=0.4)+
  scale_y_continuous(trans = log_trans(), breaks = base_breaks(5))+
  scale_x_date(breaks = date_breaks("year"), date_labels = "%y")+
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
rii_diabetes_fig2  


