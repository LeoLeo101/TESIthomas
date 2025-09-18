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

df <- read.delim("C:\\Users\\leoan\\OneDrive\\Desktop\\thomas\\ferrari_prices.txt", sep = "\t", header = TRUE)
df$Date<- as.Date(df$Date, origin="1899-12-30")
df$Date <- format(df$Date, "%d-%m-%Y")
df$Date <- as.Date(df$Date, format = "%d-%m-%Y")

#trasformo in dollari i prezzi

dfUSD<- df%>%
  mutate(NOKIA.FH.Equity = NOKIA.FH.Equity * EURUSD.Curncy)%>%
  select(-RACE.IM.Equity,-EURUSD.Curncy)

#seleziono nokia e apple per la prima analisi

dftech<- dfUSD%>%
  rename(AAPL = AAPL.US.Equity,NOK=NOKIA.FH.Equity,SPX = SPX.Index)%>%
  select(Date,NOK,AAPL)

#calolo i ritorni mensili dai prezzi

dftechRet <- dftech %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    across(c(AAPL, NOK),
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
  filter(year>=2000)%>%
  select(-X,-X_str,-year,-month)%>%
  mutate(across(1:6, ~ .x / 100))


#toglio il RF ai rendimeniti dei titoli

dftechRet.RF <- dftechRet%>%
  rename(Date = YearMonth) %>%
  left_join(FF2x3, by = "Date")%>%
  mutate(across(2:3, ~ .x - RF, .names = "{.col}_adj"))%>%
  select(-AAPL_ret,-NOK_ret)
  
#CAPM

summary(lm(AAPL_ret_adj ~ Mkt.RF,data = dftechRet.RF))
summary(lm(NOK_ret_adj ~ Mkt.RF,data = dftechRet.RF))

#FF3F

summary(lm(AAPL_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))
summary(lm(NOK_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))

#estraggo i beta

B_AAPL<-coef(lm(AAPL_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["Mkt.RF"]
B_NOK<-coef(lm(NOK_ret_adj ~ Mkt.RF + SMB + HML,data = dftechRet.RF))["Mkt.RF"]


#calcolo i rendimenti cumulati beta adjusted


dftechRetBAD<- dftechRet%>%
  mutate(AAPL_ret = AAPL_ret/B_AAPL)%>%
  mutate(NOK_ret = NOK_ret/B_NOK)%>%
  mutate(across(ends_with("_ret"),
                ~ 100 * cumprod(1 + .x) / (1 + first(.x)),
                .names = "{.col}_index"))
  
  
df_long <- dftechRetBAD %>%
  select(YearMonth, AAPL_ret_index, NOK_ret_index) %>%
  pivot_longer(-YearMonth, names_to = "Ticker", values_to = "Index100")

ggplot(df_long, aes(x = YearMonth, y = Index100, color = Ticker)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Performance cumulata (base 100)",
    subtitle = "Rendimenti cumulati aritmetici da 100",
    x = NULL, y = "Indice (base=100)",
    color = "Ticker"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 8))
  )
  
  
  