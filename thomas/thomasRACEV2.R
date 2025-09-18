rm(list = ls())

library(quantmod)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(broom)

# ---------- Parametri ----------
tickers <- c("RACE", "F", "SPY")
from <- "2010-01-01"
to   <- "2020-01-01"
ret_type <- "log"

# ---------- Helper: rendimenti giornalieri ----------
get_returns <- function(ticker, from, to, type = c("log", "arithmetic")) {
  type <- match.arg(type)
  x <- getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  px <- if (any(grepl("Adjusted", colnames(x)))) Ad(x) else Cl(x)
  r_xts <- dailyReturn(px, type = type)
  df <- data.frame(date = index(r_xts), coredata(r_xts), row.names = NULL)
  names(df)[2] <- ticker
  df
}

# ---------- 1) Rendimenti giornalieri ----------
returns_df <- reduce(
  map(tickers, ~ get_returns(.x, from, to, type = ret_type)),
  inner_join, by = "date"
) |> arrange(date)

# ---------- 2) Calcolo beta rispetto a SPY ----------
market <- "SPY"
betas <- map_dfr(setdiff(tickers, market), function(tkr) {
  fit <- lm(as.formula(paste(tkr, "~", market)), data = returns_df)
  tidy(fit) |> filter(term == market) |> 
    mutate(ticker = tkr) |> select(ticker, beta = estimate)
})

print(betas)

# ---------- 3) Normalizza rendimenti (beta-equivalent) ----------
returns_beta_eq <- returns_df |> 
  mutate(across(all_of(betas$ticker), 
                ~ .x / betas$beta[betas$ticker == cur_column()]))

# Nota: SPY non viene normalizzato (beta = 1)

# ---------- 4) Cumulata e indice base-100 ----------
cum_returns_df <- returns_beta_eq |>
  mutate(across(-date, ~ if (ret_type == "log") exp(cumsum(.x)) - 1 else cumprod(1 + .x) - 1))

index_100_df <- cum_returns_df |>
  mutate(across(-date, ~ 100 * (1 + .x)))

# ---------- 5) Long per plotting ----------
index_100_long <- index_100_df |> 
  pivot_longer(-date, names_to = "ticker", values_to = "index100")

# Ultimo valore per etichette finali
last_pts <- index_100_long |> 
  group_by(ticker) |> slice_tail(n = 1) |> ungroup() |>
  mutate(label = paste0(ticker, "  ", sprintf("%.1f", index100)))

# ---------- 6) Grafico ----------
p <- ggplot(index_100_long, aes(x = date, y = index100, color = ticker)) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  geom_line(linewidth = 1) +
  geom_point(data = last_pts, size = 2) +
  geom_label_repel(data = last_pts, aes(label = label),
                   size = 3.2, label.padding = unit(0.15, "lines"),
                   label.size = 0.2, min.segment.length = 0,
                   max.overlaps = Inf, show.legend = FALSE) +
  scale_y_continuous(labels = function(x) sprintf("%.0f", x)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_viridis_d(option = "D", end = 0.9) +
  labs(title = "Performance a livelli Beta-equivalenti (base 100)",
       subtitle = paste0("Rendimenti aggiustati per beta vs ", market),
       x = NULL, y = "Indice (base=100)",
       color = "Ticker") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(r = 8)))

print(p)

