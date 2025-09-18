rm(list = ls())

# LIBRERIE

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(slider)
library(readr)

#CARICAMENTO DATI 

dfrawFF5 <- as.data.frame(read.csv("C:\\Users\\leoan\\OneDrive\\Desktop\\thomas\\F-F_Research_Data_5_Factors_2x3.csv",nrows= 744))


dfFF5 <- dfrawFF5 %>%
  mutate(
    # conversione in stringa
    X_str = as.character(X),
    # estrazione anno e mese
    year = substr(X_str, 1, 4),
    month = substr(X_str, 5, 6),
    # creazione data come Date
    date = my(paste0(month, year)))%>%
  filter(year>=1990)%>%
  select(-X_str,-year,-month,-X)