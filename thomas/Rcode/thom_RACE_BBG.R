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

df <- read.delim("ferrari_prices.txt", sep = "\t", header = TRUE)
df$Date<- as.Date(df$Date, origin="1899-12-30")
df$Date <- format(df$Date, "%d-%m-%Y")
df$Date <- as.Date(df$Date, format = "%d-%m-%Y")

#trasformo in dollari i prezzi

dfUSD<- df%>%
  mutate(NOKIA.FH.Equity = NOKIA.FH.Equity * EURUSD.Curncy)%>%
  select(-RACE.IM.Equity,-EURUSD.Curncy)

#seleziono nokia e apple per la prima analisi

dftech<- dfUSD%>%
  rename(RACE = RACE.US.Equity,F=F.US.Equity,SPX = SPX.Index)%>%
  select(Date,RACE,F,SPX)%>%
  filter(Date > "2016-01-01")

dftech <- dftech %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(across(c(RACE, F,SPX), ~ parse_number(as.character(.))))

#calolo i ritorni mensili dai prezzi

dftechRet <- dftech %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    across(c(RACE,F,SPX),
           list(ret = ~ last(.x) / first(.x) - 1),
           .names = "{.col}_ret"),
    .groups = "drop"
  )

#importo rf da darmouth
FF2x3 <- read.csv("F-F_Research_Data_5_Factors_2x3.csv",nrows = 744)

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
  filter(year>2016)%>%
  select(-X,-X_str,-year,-month)%>%
  mutate(across(1:6, ~ .x / 100))


#toglio il RF ai rendimeniti dei titoli

dftechRet.RF <- dftechRet%>%
  rename(Date = YearMonth) %>%
  left_join(FF2x3, by = "Date")%>%
  mutate(across(c(RACE_ret,F_ret), ~ .x - RF, .names = "{.col}_adj"))%>%
  select(-RACE_ret,-F_ret)

#CAPM

summary(lm(RACE_ret_adj ~ Mkt.RF,data = dftechRet.RF))
summary(lm(F_ret_adj ~ Mkt.RF,data = dftechRet.RF))

#FF3F

summary(lm(RACE_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))
summary(lm(F_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))

#estraggo i beta

B_RACE<-coef(lm(RACE_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["Mkt.RF"]
B_F<-coef(lm(F_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["Mkt.RF"]

#estraggo gli alpha

A_RACE<-coef(lm(RACE_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["(Intercept)"]
A_F<-coef(lm(F_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["(Intercept)"]

#calcolo i rendimenti cumulati beta adjusted

dftechRetBAD<- dftechRet%>%
  mutate(RACE_ret = RACE_ret/B_RACE)%>%
  mutate(F_ret = F_ret/B_F)%>%
  mutate(across(ends_with("_ret"),
                ~ 100 * cumprod(1 + .x) / (1 + first(.x)),
                .names = "{.col}_index"))

# <<< AGGIUNTA SPX: costruisco il long e il grafico includendo SPX non aggiustato
df_long <- dftechRetBAD %>%
  select(YearMonth, RACE = RACE_ret_index, F = F_ret_index, SPX = SPX_ret_index) %>%
  pivot_longer(-YearMonth, names_to = "Ticker", values_to = "Index100")

ggplot(df_long, aes(x = YearMonth, y = Index100, color = Ticker)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  scale_color_manual(
    values = c(
      RACE = "#10de47",  # verde
      SPX  = "#1010de",  # blu
      F  = "#de1010"   # rosso
    ),
    breaks = c("RACE", "F", "SPX"),  # ordine in legenda
    name = "Ticker"
  ) +
  scale_x_date(
    breaks = seq(min(df_long$YearMonth), max(df_long$YearMonth), by = "1 years"),
    date_labels = "%Y"
  ) +
  labs(
    title    = expression("Rendimenti Race vs F " * beta ~ "adjusted"),
    subtitle = "Rendimenti cumulati aritmetici normalizzati in base 100",
    x = NULL, y = "Indice (base=100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 12),
    legend.title     = element_text(size = 13),
    plot.title       = element_text(size = 18, face = "bold", hjust = 0), # <-- sinistra + grassetto
    plot.subtitle    = element_text(size = 13, hjust = 0),                # anche sottotitolo a sinistra
    axis.title.y     = element_text(margin = margin(r = 8))
  )
