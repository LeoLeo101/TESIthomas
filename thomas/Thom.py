# -*- coding: utf-8 -*-
# Thom.py — Conversione R -> Python con fix robusti per yfinance/Pandas

import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import yfinance as yf
import statsmodels.api as sm
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime

try:
    from adjustText import adjust_text  # opzionale (per evitare sovrapposizioni delle etichette finali)
    _HAS_ADJUSTTEXT = True
except Exception:
    _HAS_ADJUSTTEXT = False

# ---------- Parametri ----------
TICKERS = ["F", "AAPL", "SPY"]
MARKET  = "SPY"
START   = "2020-01-01"
END     = "2025-01-01"
RET_TYPE = "log"      # "log" oppure "arithmetic"

# ---------- Helper: scelta prezzo (gestione MultiIndex/flat) ----------
def _pick_price_column(df: pd.DataFrame) -> pd.Series:
    """
    Restituisce una Series di prezzi, preferendo 'Adj Close' se disponibile/non tutta NaN,
    altrimenti 'Close'. Gestisce sia colonne flat che MultiIndex (yfinance a volte le usa).
    """
    if df.empty:
        raise ValueError("DataFrame vuoto (nessun dato scaricato)")

    # Caso MultiIndex (es. livello 0: ['Open','High','Low','Close','Adj Close','Volume'])
    if isinstance(df.columns, pd.MultiIndex):
        lvl0 = df.columns.get_level_values(0)
        px = None
        if "Adj Close" in set(lvl0):
            sub = df.xs("Adj Close", axis=1, level=0)
            # se per qualche motivo ci sono più colonne, prendo la prima
            px = sub.iloc[:, 0] if isinstance(sub, pd.DataFrame) else sub
            if not pd.isna(px).all():
                return px
        if "Close" in set(lvl0):
            sub = df.xs("Close", axis=1, level=0)
            px = sub.iloc[:, 0] if isinstance(sub, pd.DataFrame) else sub
            return px
        raise ValueError(f"Colonne di prezzo mancanti: livelli disponibili={sorted(set(lvl0))}")

    # Caso colonne flat standard
    if "Adj Close" in df.columns:
        px = df["Adj Close"]
        if not px.isna().all():
            return px
    if "Close" in df.columns:
        return df["Close"]
    raise ValueError(f"Colonne di prezzo mancanti: {list(df.columns)}")

# ---------- Helper: rendimenti giornalieri ----------
def get_returns(ticker: str, start: str, end: str, ret_type: str = "log") -> pd.DataFrame:
    """
    Scarica prezzi (preferendo Adj Close) e calcola rendimenti giornalieri.
    ret_type: 'log' (log-diff) oppure 'arithmetic' (pct_change).
    Ritorna DataFrame con indice Date e singola colonna = ticker.
    """
    df = yf.download(ticker, start=start, end=end, progress=False, auto_adjust=False, group_by="column")
    if df.empty:
        raise ValueError(f"Nessun dato per {ticker} tra {start} e {end}")

    px = _pick_price_column(df).sort_index()

    if ret_type.lower() == "log":
        r = np.log(px).diff()
    elif ret_type.lower() == "arithmetic":
        r = px.pct_change()
    else:
        raise ValueError("ret_type deve essere 'log' o 'arithmetic'")

    r = r.dropna()
    return r.to_frame(name=ticker)

# ---------- 1) Rendimenti giornalieri (inner join su date comuni) ----------
ret_list = [get_returns(t, START, END, RET_TYPE) for t in TICKERS]
returns_df = pd.concat(ret_list, axis=1).dropna().sort_index()

# ---------- 2) Calcolo beta rispetto al mercato (MARKET) ----------
betas = []
for tkr in TICKERS:
    if tkr == MARKET:
        continue
    X = sm.add_constant(returns_df[[MARKET]])
    y = returns_df[tkr]
    fit = sm.OLS(y, X, missing='drop').fit()
    beta = fit.params.get(MARKET, np.nan)
    betas.append({"ticker": tkr, "beta": float(beta)})

betas_df = pd.DataFrame(betas).sort_values("ticker").reset_index(drop=True)
print("Betas vs", MARKET)
print(betas_df.to_string(index=False))

# ---------- 3) Normalizza rendimenti (beta-equivalent) ----------
returns_beta_eq = returns_df.copy()
for _, row in betas_df.iterrows():
    tkr, beta = row["ticker"], row["beta"]
    if pd.isna(beta) or np.isclose(beta, 0.0):
        warnings.warn(f"Beta non valido (~0/NaN) per {tkr}; normalizzazione saltata (NaN).")
        returns_beta_eq[tkr] = np.nan
    else:
        returns_beta_eq[tkr] = returns_beta_eq[tkr] / beta
# La colonna del MARKET rimane invariata (come nello script R)

# ---------- 4) Cumulata e indice base-100 ----------
def cumulate(series: pd.Series, ret_type: str) -> pd.Series:
    if ret_type.lower() == "log":
        return np.exp(series.cumsum()) - 1.0
    else:
        return (1.0 + series).cumprod() - 1.0

cum_returns_df = returns_beta_eq.apply(lambda s: cumulate(s, RET_TYPE))
index_100_df = 100.0 * (1.0 + cum_returns_df)
index_100_df = index_100_df.sort_index()

# ---------- 5) Long + curva dei massimi storici ----------
index_100_long = (
    index_100_df
    .reset_index()
    .rename(columns={"index": "date"})
    .melt(id_vars="Date" if "Date" in index_100_df.reset_index().columns else "date",
          var_name="ticker", value_name="index100")
    .rename(columns={"Date": "date"})
    .dropna(subset=["index100"])
    .sort_values(["ticker", "date"])
)

index_100_long["peak100"] = index_100_long.groupby("ticker")["index100"].cummax()
index_100_long["drawdown"] = index_100_long["index100"] / index_100_long["peak100"] - 1.0

# Ultimo valore per etichette
last_idx = index_100_long.groupby("ticker")["date"].idxmax()
last_pts = index_100_long.loc[last_idx].copy()
last_pts["label"] = last_pts.apply(lambda r: f"{r['ticker']}  {r['index100']:.1f}", axis=1)

# ---------- 6) Grafico ----------
plt.figure(figsize=(11, 6.5))
ax = plt.gca()

# linea base 100
ax.axhline(100.0, linestyle="--", linewidth=0.8, alpha=0.7)

# palette viridis con end=0.9
cmap = plt.cm.get_cmap("viridis")
tickers_sorted = sorted(index_100_long["ticker"].unique().tolist())
colors = {t: cmap(i) for t, i in zip(tickers_sorted, np.linspace(0.0, 0.9, len(tickers_sorted)))}

# linee principali + peak line
for tkr in tickers_sorted:
    dft = index_100_long[index_100_long["ticker"] == tkr]
    ax.plot(dft["date"], dft["index100"], label=tkr, linewidth=1.5, color=colors[tkr])
    # peak line stile "dotdash" approssimato
    ax.plot(dft["date"], dft["peak100"], linewidth=0.8, alpha=0.4, color=colors[tkr], dashes=(3, 5, 1, 5))

# punti finali
for _, r in last_pts.iterrows():
    ax.scatter(r["date"], r["index100"], s=26, color=colors[r["ticker"]], zorder=3)

# label finali
texts = []
for _, r in last_pts.iterrows():
    txt = ax.annotate(
        r["label"],
        xy=(r["date"], r["index100"]),
        xytext=(6, 0),
        textcoords="offset points",
        ha="left", va="center",
        fontsize=9,
        bbox=dict(boxstyle="round,pad=0.2", fc="white", ec="0.6", lw=0.6, alpha=0.8)
    )
    texts.append(txt)

if _HAS_ADJUSTTEXT:
    adjust_text(texts, ax=ax, only_move={'points': 'y', 'texts': 'y', 'objects': 'y'})

# assi e stile
ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, pos: f"{x:.0f}"))
ax.xaxis.set_major_locator(mdates.YearLocator(base=1))
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))

ax.set_title(
    f"Performance a livelli Beta-equivalenti (base 100)\n"
    f"Con curva dei massimi storici (peak line) vs {MARKET}",
    fontweight="bold"
)
ax.set_ylabel("Indice (base=100)")
ax.grid(axis="y", alpha=0.25)
ax.margins(x=0.01)

ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.08), ncol=min(5, len(tickers_sorted)))
plt.tight_layout()
plt.show()

# Stampa betas (come nello script R)
print("\n--- Betas (riepilogo) ---")
print(betas_df.to_string(index=False))
