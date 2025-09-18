rm(list = ls())

library(quantmod)
library(dplyr)
library(purrr)
library(tidyr)

# ---- Parametri ----
tickers <- c("SPY", "MSFT","KO")
from <- "2010-01-01"
to   <- "2020-12-31"
ret_type <- "log"   # "log" oppure "arithmetic"

# ---- Funzione: daily returns per ticker (usa Adjusted se c'è) ----
get_returns <- function(ticker, from, to, type = c("log","arithmetic")) {
  type <- match.arg(type)
  x <- getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  px <- if (any(grepl("Adjusted", colnames(x)))) Ad(x) else Cl(x)
  r_xts <- dailyReturn(px, type = type)
  df <- data.frame(date = index(r_xts), coredata(r_xts), row.names = NULL)
  names(df)[2] <- ticker
  df
}

# ---- 1) Rendimenti giornalieri allineati sulle date comuni ----
returns_df <- reduce(
  map(tickers, ~ get_returns(.x, from, to, type = ret_type)),
  inner_join, by = "date"        # solo le date presenti per tutti
) |> arrange(date)

# ---- 2) Rendimenti cumulati che partono da 0 ----
cum_returns_df <- returns_df |>
  mutate(
    across(-date, ~ if (ret_type == "log") {
      # log: R_t^cum = exp(cumsum(r_t)) - 1
      exp(cumsum(.x)) - 1
    } else {
      # arithmetic: R_t^cum = prod(1+r_t) - 1
      cumprod(1 + .x) - 1
    })
  )

# (Opzionale) 3) Indice base-100 (comodo per i grafici)
index_100_df <- cum_returns_df |>
  mutate(across(-date, ~ 100 * (1 + .x)))

# ---- 4) (Opzionale) formato long per ggplot ----
cum_returns_long <- cum_returns_df |>
  pivot_longer(-date, names_to = "ticker", values_to = "cum_ret")

index_100_long <- index_100_df |>
  pivot_longer(-date, names_to = "ticker", values_to = "index100")

# Esempio rapido di grafico (cumulative returns)
# library(ggplot2)
# ggplot(cum_returns_long, aes(x = date, y = cum_ret, color = ticker)) +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(title = "Rendimenti cumulati (base 0)", x = "Data", y = "Rendimento cumulato")

 #Esempio grafico base-100
ggplot(index_100_long, aes(x = date, y = index100, color = ticker)) +
   geom_line() +
   labs(title = "Indice base 100", x = "Data", y = "Indice (base=100)")
