rm(list = ls())

# LIBRERIE

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(slider)
library(readr)
library(quantmod)
library(tidyquant)


# Yahoo è la sorgente di default
getSymbols("RACE", src = "yahoo", from = "2010-01-01", to = "2025-09-17")
RACE_df <- data.frame(
  date = index(RACE),          # prendi l'indice (le date)
  coredata(RACE),              # prendi i dati
  row.names = NULL             # evita che le date restino come rownames
)

