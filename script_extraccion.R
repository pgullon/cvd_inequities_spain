# This file is to get the data for all national surveys
library(tidyverse)
library(openxlsx)
library(haven)

rm(list=ls())



###################################### 2001 ###########################
#Cargamos encuesta adultos



###################################### 2003 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2003/Adul03.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD")
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2003 <- read_fwf("2003/ADULTO03.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

# Cargamos encuesta hogar
campos <- read.xlsx("2003/Hogar03.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD")
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
hogar2003 <- read_fwf("2003/HOGAR03.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
ense_2003 <- adulto2003 %>%
  left_join(hogar2003, by="NIDENTIF")
save(ense2003, file = "2003/ense2003.RData")




###################################### 2006 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2006/Adul06.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD")
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2006 <- read_fwf("2006/ADULTO06.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

# Cargamos encuesta hogar
campos <- read.xlsx("2006/Hogar06.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD")
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
hogar2006 <- read_fwf("2006/HOGAR06.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
ense_2006 <- adulto2006 %>%
  left_join(hogar2006, by="NIDENTIF")
save(ense2006, file = "2006/ense2006.RData")




###################################### 2009 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2009/Adul09.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2009 <- read_fwf("2009/ADULTO09.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("2009/Hogar09.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
hogar2009 <- read_fwf("2009/HOGAR09.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
eese_2009 <- adulto2009 %>%
  left_join(hogar2009, by="IDENTHOGAR")
save(eese2009, file = "2009/eese2009.RData")



###################################### 2011 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2011/Adul11.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2011 <- read_fwf("2011/ADULTO11.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("2011/Hogar11.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
hogar2011 <- read_fwf("2011/HOGAR11.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
ense_2011 <- adulto2011 %>%
  left_join(hogar2011, by="IDENTHOGAR")
save(ense2011, file = "2011/ense2011.RData")


###################################### 2014 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2014/Adul14.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2014 <- read_fwf("2014/ADULTO14.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("2014/Hogar14.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
hogar2014 <- read_fwf("2014/HOGAR14.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
eese_2014 <- adulto2014 %>%
  left_join(hogar2014, by="IDENTHOGAR")
save(eese_2014, file = "2014/eese2014.RData")




###################################### 2017 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2017/Adul17.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
adulto2017 <- read_fwf("2017/ADULTO17.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("2017/Hogar17.xlsx", colNames = FALSE) %>%
  filter(is.na(X2) == FALSE & X2 != "LONGITUD") 
nombres <- campos$X1
anchos  <- campos$X2 %>% as.numeric
hogar2017 <- read_fwf("2017/HOGAR17.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
ense_2017 <- adulto2017 %>%
  left_join(hogar2017, by="IDENTHOGAR")
save(ense_2017, file = "2017/ense2017.RData")



###################################### 2020 ###########################
#Cargamos encuesta adultos
campos <- read.xlsx("2020/Adul20.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2020 <- read_fwf("2020/ADULTO20.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("2020/Hogar20.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
hogar2020 <- read_fwf("2020/HOGAR20.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
eese_2020 <- adulto2020 %>%
  left_join(hogar2020, by="IDENTHOGAR")
save(eese_2020, file = "2020/eese2020.RData")



