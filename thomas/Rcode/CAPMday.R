rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(broom)
library(readr)
library(lubridate)

#carico dati sui prezzi BBG

df <- read.delim("https://raw.githubusercontent.com/LeoLeo101/TESIthomas/refs/heads/main/thomas/DATA/ferrari_prices.txt", sep = "\t", header = TRUE)
df$Date<- as.Date(df$Date, origin="1899-12-30")
df$Date <- format(df$Date, "%d-%m-%Y")
df$Date <- as.Date(df$Date, format = "%d-%m-%Y")

#trasformo in dollari i prezzi

dfUSD<- df%>%
  mutate(NOKIA.FH.Equity = NOKIA.FH.Equity * EURUSD.Curncy)%>%
  select(-RACE.IM.Equity,-EURUSD.Curncy)%>%
  filter(Date > "2016-01-01")

#cambio i nomi alle variabili e mi assicuro che siano state lette come numeri

dfUSD <- dfUSD %>%
  rename(
    SPX  = SPX.Index,
    RACE = RACE.US.Equity,
    F    = F.US.Equity,
    AAPL = AAPL.US.Equity,
    NOK  = NOKIA.FH.Equity
  ) %>%
  mutate(across(-Date, as.numeric))


#calcolo i rendimenti giornalieri

dfRet <- dfUSD %>%
  arrange(Date) %>%
  mutate(across(c(AAPL, NOK, RACE, F, SPX),
                ~ .x / lag(.x) - 1)) %>%
  drop_na()


# importo i fattori FF
FF2x3 <- read.csv("https://raw.githubusercontent.com/LeoLeo101/TESIthomas/refs/heads/main/thomas/DATA/F-F_Research_Data_5_Factors_2x3.csv",nrows = 744)

FF2x3 <- FF2x3 %>%
  mutate(across(everything(), ~ ifelse(. == -99.99, NA, .))) %>%
  mutate(
    # conversione in stringa
    X_str = as.character(X),
    # estrazione anno e mese
    year = substr(X_str, 1, 4),
    month = substr(X_str, 5, 6),
    # creazione data come Date
    Date = my(paste0(month, year))   # formattazione mese-anno
  )%>%
  filter(year>2015)%>%
  select(-X,-X_str,-year,-month)%>%
  mutate(across(1:6, ~ .x / 100))


# FF2x3: colonne Date (primo giorno del mese) e RF (tasso mensile in decimali)

rf_day <- FF2x3 %>%
  mutate(
    month_start   = floor_date(Date, "month"),
    month_end     = ceiling_date(Date, "month") - days(1),
    ndays         = as.integer(month_end - month_start + 1L),
    rf_daily_rate = (1 + RF)^(1 / ndays) - 1     # tasso giornaliero equivalente
  ) %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(month_start, month_end, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, RF_daily = rf_daily_rate)

# rf_daily ora ha una riga per ogni giorno con il corrispondente RF_daily

dfret_RF <- dfRet %>%
  left_join(rf_day, by = "Date") %>%
  mutate(
    across(
      !c(Date, RF_daily),
      ~ .x - RF_daily
    )
  )%>%
  select(-RF_daily)



# Lista di formule
formule <- list(
  RACE = RACE ~ SPX,
  F    = F ~ SPX
)

# Funzione helper per stimare modello ed estrarre intercetta (alpha) + pvalue
estrai_alpha <- function(formula, data) {
  model <- lm(formula, data = data)
  coeffs <- summary(model)$coefficients
  c(
    alpha   = coeffs["(Intercept)", "Estimate"],
    p_value = coeffs["(Intercept)", "Pr(>|t|)"]
  )
}

# Applico a tutte le formule
risultati <- lapply(formule, estrai_alpha, data = dfret_RF)

# Converto in data frame presentabile
df_risultati <- do.call(rbind, risultati) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Modello")

df_risultati




