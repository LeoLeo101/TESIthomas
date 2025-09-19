# -*- coding: utf-8 -*-
# Pipeline: prezzi BBG -> USD -> rendimenti mensili -> join FF -> CAPM/FF3F -> beta & cumulate -> grafico

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from pathlib import Path

# --- percorsi robusti, relativi al file .py ---
BASE_DIR = Path(__file__).resolve().parent
DATA_DIR = BASE_DIR.parent / "DATA"

PRICES_TXT = DATA_DIR / "ferrari_prices.txt"
FF_CSV     = DATA_DIR / "F-F_Research_Data_5_Factors_2x3.csv"

if not PRICES_TXT.exists():
    raise FileNotFoundError(f"File prezzi non trovato: {PRICES_TXT}")
if not FF_CSV.exists():
    raise FileNotFoundError(f"File Fama-French non trovato: {FF_CSV}")

# ------------------------- Helper -------------------------
def read_prices_txt(path: Path, start_date: str | None = None) -> pd.DataFrame:
    """
    Legge il file prezzi Bloomberg (txt con tabulazioni).
    Converte la colonna Date in datetime.
    Se start_date è passato (es. '2016-01-01'), elimina tutte le osservazioni precedenti.
    """
    df = pd.read_csv(path, sep="\t", header=0)
    if "Date" not in df.columns:
        raise KeyError(f"Manca la colonna 'Date' nel TXT. Colonne: {list(df.columns)}")

    # prova Excel-seriale, poi dd-mm-YYYY, poi parse generico
    d = pd.to_datetime(df["Date"], unit="D", origin="1899-12-30", errors="coerce")
    if d.isna().all():
        d = pd.to_datetime(df["Date"], format="%d-%m-%Y", errors="coerce")
    if d.isna().all():
        d = pd.to_datetime(df["Date"], errors="coerce")
    if d.isna().all():
        raise ValueError("Impossibile interpretare le date dalla colonna 'Date'.")
    df["Date"] = d

    # se specificata, applica il filtro
    if start_date is not None:
        start = pd.to_datetime(start_date)
        df = df.loc[df["Date"] >= start]

    return df.sort_values("Date").reset_index(drop=True)


def norm(s: str) -> str:
    # normalizza un nome colonna (spazi/punti/trattini e case)
    return "".join(ch for ch in s.lower() if ch.isalnum())

def standardize_price_columns(df: pd.DataFrame) -> pd.DataFrame:
    """
    Accetta sia notazione Bloomberg con punti (AAPL.US.Equity) sia con spazi (AAPL US Equity).
    Ritorna colonne canoniche: Date, AAPL, NOK, SPX, EURUSD (se presente).
    """
    col_map = {norm(c): c for c in df.columns}
    want = {
        "aaplusequity": "AAPL",
        "raceusequity": "RACE",
        "fusequity": "F",
        "nokiafhequity": "NOK",
        "spxindex": "SPX",
        "eurusdcurncy": "EURUSD",
    }
    missing = []
    rename = {}
    for key_norm, canon in want.items():
        if key_norm in col_map:
            rename[col_map[key_norm]] = canon
        else:
            # EURUSD può anche non servire se non usiamo NOK in EUR -> USD,
            # ma lo segnaliamo come "morbido"
            if canon != "EURUSD":
                missing.append(canon)
    if missing:
        raise KeyError(f"Colonne mancanti nel TXT: {missing}\nColonne presenti: {list(df.columns)}")

    out = df.rename(columns=rename).copy()
    return out

def month_last_first_ret(g: pd.DataFrame, col: str) -> float:
    s = g[col].dropna()
    if s.empty:
        return np.nan
    return s.iloc[-1] / s.iloc[0] - 1.0

def read_ff_monthly(path: Path) -> pd.DataFrame:
    """
    Legge i 5 fattori di Ken French (mensili) e restituisce Date, Mkt.RF, SMB, HML, RF in decimali.
    Robusto a intestazioni e varianti “Mkt-RF”/“Mkt.RF”.
    """
    raw = pd.read_csv(path, header=0)
    raw.columns = [str(c).strip() for c in raw.columns]

    first_col = raw.columns[0]
    mask_ym = raw[first_col].astype(str).str.fullmatch(r"\d{6}")
    df = raw.loc[mask_ym].copy()
    if df.empty:
        # fallback: alcuni CSV hanno parte annuale sotto; limitiamo le righe se serve
        df = pd.read_csv(path, nrows=900)
        df.columns = [str(c).strip() for c in df.columns]
        first_col = df.columns[0]
        mask_ym = df[first_col].astype(str).str.fullmatch(r"\d{6}")
        df = df.loc[mask_ym].copy()
        if df.empty:
            raise ValueError("Non trovo il blocco mensile YYYYMM nel file dei fattori.")

    # Rename robusto (case/segni)
    rename_map = {}
    for c in df.columns:
        c_norm = norm(c)  # es. "mktrf", "smb", "hml", "rf"
        if c_norm == "mktrf":
            rename_map[c] = "Mkt.RF"
        elif c_norm == "smb":
            rename_map[c] = "SMB"
        elif c_norm == "hml":
            rename_map[c] = "HML"
        elif c_norm == "rf":
            rename_map[c] = "RF"
        elif c_norm == "rmw":
            rename_map[c] = "RMW"
        elif c_norm == "cma":
            rename_map[c] = "CMA"

    df = df.rename(columns=rename_map)

    ym_col = df.columns[0]
    df["year"] = df[ym_col].astype(str).str.slice(0, 4).astype(int)
    df["month"] = df[ym_col].astype(str).str.slice(4, 6).astype(int)
    df["Date"] = pd.to_datetime(dict(year=df["year"], month=df["month"], day=1))

    # Dal 2000 in poi (adatta se vuoi)
    df = df.loc[df["year"] >= 2016].copy()

    # Converti -99.99 in NaN e percentuali -> decimali
    for c in ["Mkt.RF", "SMB", "HML", "RF", "RMW", "CMA"]:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce").replace(-99.99, np.nan) / 100.0

    keep = [c for c in ["Date", "Mkt.RF", "SMB", "HML", "RF"] if c in df.columns]
    if "Mkt.RF" not in keep:
        # se ancora non lo vediamo, stampiamo diagnostica
        raise KeyError(f"Nel file FF non trovo 'Mkt.RF'. Colonne viste: {list(df.columns)}")
    return df[keep].reset_index(drop=True)

def base100_from_returns(r: pd.Series) -> pd.Series:
    r = r.copy()
    if r.dropna().empty:
        return pd.Series(np.nan, index=r.index)
    cp = (1.0 + r).cumprod()
    first = 1.0 + r.iloc[0]
    return 100.0 * cp / first

def run_ols(y: pd.Series, X: pd.DataFrame, add_const=True):
    if add_const:
        X = sm.add_constant(X, has_constant='add')
    mask = y.notna() & X.notna().all(axis=1)
    return sm.OLS(y[mask], X[mask]).fit()

# ------------------- 1) Prezzi & colonne -------------------
prices_raw = read_prices_txt(PRICES_TXT, start_date="2016-01-01")
prices = standardize_price_columns(prices_raw)

# Se abbiamo EURUSD, convertiamo NOK in USD
if "EURUSD" in prices.columns and "NOK" in prices.columns:
    prices["NOK"] = prices["NOK"] * prices["EURUSD"]

# Teniamo solo ciò che serve
keep_cols = ["Date", "RACE", "F", "SPX"]
keep_cols = [c for c in keep_cols if c in prices.columns]
dftech = prices[keep_cols].copy()

# ------------------- 2) Rendimenti mensili -----------------
# Usa Period->Timestamp (inizio mese) per avere dtype datetime64[ns] consistente
dftech["YearMonth"] = dftech["Date"].dt.to_period("M").dt.to_timestamp()

rets = []
for ym, g in dftech.groupby("YearMonth", sort=True):
    rets.append({
        "YearMonth": ym,
        "RACE_ret": month_last_first_ret(g, "RACE") if "RACE" in g else np.nan,
        "F_ret":  month_last_first_ret(g, "F")  if "F"  in g else np.nan,
        "SPX_ret":  month_last_first_ret(g, "SPX")  if "SPX"  in g else np.nan,
    })
dftechRet = pd.DataFrame(rets).sort_values("YearMonth").reset_index(drop=True)

# ------------------- 3) Fattori Fama-French ----------------
ff = read_ff_monthly(FF_CSV)

df = dftechRet.rename(columns={"YearMonth": "Date"}).merge(ff, on="Date", how="left")

required = ["Mkt.RF", "SMB", "HML", "RF"]
missing = [c for c in required if c not in df.columns]
if missing:
    raise KeyError(f"Mancano colonne FF dopo il merge: {missing}\nColonne disponibili: {list(df.columns)}")

# Excess returns
if "RACE_ret" in df: df["RACE_ret_adj"] = df["RACE_ret"] - df["RF"]
if "F_ret"  in df: df["F_ret_adj"]  = df["F_ret"]  - df["RF"]

# ------------------- 4) Regressioni CAPM / FF3F ------------
def safe_series(name): 
    return df[name] if name in df else pd.Series(index=df.index, dtype=float)

capm_RACE = run_ols(safe_series("RACE_ret_adj"), df[["Mkt.RF"]])
capm_F  = run_ols(safe_series("F_ret_adj"),  df[["Mkt.RF"]])

print("\n=== CAPM RACE (RACE-RF ~ Mkt.RF) ===")
print(capm_RACE.summary())
print("\n=== CAPM F (F-RF ~ Mkt.RF) ===")
print(capm_F.summary())

ff3_RACE = run_ols(safe_series("RACE_ret_adj"), df[["Mkt.RF", "SMB", "HML"]])
ff3_F  = run_ols(safe_series("F_ret_adj"),  df[["Mkt.RF", "SMB", "HML"]])

print("\n=== FF3F RACE (RACE-RF ~ Mkt.RF + SMB + HML) ===")
print(ff3_RACE.summary())
print("\n=== FF3F F (F-RF ~ Mkt.RF + SMB + HML) ===")
print(ff3_F.summary())

B_RACE = float(ff3_RACE.params.get("Mkt.RF", np.nan))
B_F  = float(ff3_F.params.get("Mkt.RF",  np.nan))

A_RACE = float(ff3_RACE.params["const"]) if "const" in ff3_RACE.params else np.nan
A_F  = float(ff3_F.params["const"]) if "const" in ff3_F.params else np.nan

print(f"\nBeta (Mkt.RF) da FF3F -> RACE: {B_RACE:.4f}, F: {B_F:.4f}")
print(f"\nAlpha (const) da FF3F -> RACE: {A_RACE:.4f}, F: {A_F:.4f}")

# ------------------- 5) Cumulate beta-adjusted -------------
df_bad = dftechRet.copy()

if np.isfinite(B_RACE) and B_RACE != 0 and "RACE_ret" in df_bad:
    df_bad["RACE_ret"] = df_bad["RACE_ret"] / B_RACE
else:
    print("Attenzione: beta RACE non valido; salto l'aggiustamento.")

if np.isfinite(B_F) and B_F != 0 and "F_ret" in df_bad:
    df_bad["F_ret"] = df_bad["F_ret"] / B_F
else:
    print("Attenzione: beta F non valido; salto l'aggiustamento.")

if "RACE_ret" in df_bad: df_bad["RACE_ret_index"] = base100_from_returns(df_bad["RACE_ret"])
if "F_ret"  in df_bad: df_bad["F_ret_index"]  = base100_from_returns(df_bad["F_ret"])
if "SPX_ret"  in df_bad: df_bad["SPX_ret_index"]  = base100_from_returns(df_bad["SPX_ret"])

# ------------------- 6) Grafico ----------------------------
plot_cols = [c for c in ["RACE_ret_index", "F_ret_index", "SPX_ret_index"] if c in df_bad.columns]
labels = {"RACE_ret_index":"RACE", "F_ret_index":"F", "SPX_ret_index":"SPX"}

plt.figure(figsize=(11, 6))
for c in plot_cols:
    plt.plot(df_bad["YearMonth"], df_bad[c], label=labels.get(c, c), linewidth=1.5)

plt.axhline(100, linestyle="--", linewidth=0.8, alpha=0.7)
plt.title("Rendimenti RACE vs F $\\beta$ adjusted\n " \
"Rendimenti cumulati aritmetici normalizzati in base 100")
plt.ylabel("Indice (base = 100)")
plt.xlabel("")
plt.legend(loc="lower center", ncol=len(plot_cols))
plt.grid(True, axis="y", alpha=0.3)
plt.tight_layout()
plt.show()
