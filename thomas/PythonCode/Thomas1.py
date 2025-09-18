# -*- coding: utf-8 -*-
# Porting R -> Python del workflow su FF 3x3, SMB/HML, RFR, MB e regressioni
# Autore: Andrea (conversione assistita)

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from pathlib import Path

# ------------------------------------------------------------
# File paths (aggiorna se necessario)
# ------------------------------------------------------------
FF_CSV = r"C:\Users\leoan\OneDrive\Desktop\thomas\DATA\FFdataEWV2.csv"
RFR_CSV = r"C:\Users\leoan\OneDrive\Desktop\thomas\DATA\RFR.csv"      # con colonne: Date (mm/dd/YYYY), Value (tasso annuo)
START_YR = 1990

# ------------------------------------------------------------
# Helper
# ------------------------------------------------------------
def yyyymm_to_date(yyyymm: pd.Series) -> pd.Series:
    """Converte un intero/stringa YYYYMM in datetime primo del mese."""
    s = yyyymm.astype(str).str.zfill(6)
    year = s.str[:4].astype(int)
    month = s.str[4:6].astype(int)
    return pd.to_datetime(dict(year=year, month=month, day=1))

def row_mean_by_positions(df: pd.DataFrame, positions_1based: list) -> pd.Series:
    """Media riga su colonne selezionate per POSIZIONE (1-based) all'interno di df."""
    idx = [i - 1 for i in positions_1based]  # converti a 0-based
    return df.iloc[:, idx].mean(axis=1, skipna=True)

def rolling12_grouped(df_long, value_col="Value", by_col="Factor", date_col="date"):
    """Aggiunge media mobile 12 mesi per fattore."""
    df_long = df_long.sort_values([by_col, date_col]).copy()
    df_long["roll12"] = (
        df_long.groupby(by_col)[value_col]
        .transform(lambda s: s.rolling(12, min_periods=12).mean())
    )
    return df_long

def adj_r2(y, X):
    """Restituisce l'Adjusted R^2 di un OLS con costante."""
    Xc = sm.add_constant(X, has_constant="add")
    model = sm.OLS(y, Xc, missing="drop").fit()
    return model.rsquared_adj

# ------------------------------------------------------------
# 1) Caricamento dati FF 100 portafogli
#    Nota: in R usavi skip=1207, nrows=1188 -> riproduciamo uguale.
# ------------------------------------------------------------
dfraw = pd.read_csv(FF_CSV, skiprows=1207, nrows=1188)

if "X" not in dfraw.columns:
    dfraw = dfraw.rename(columns={dfraw.columns[0]: "X"})
# Rimpiazza -99.99 con NaN
dfraw = dfraw.replace(-99.99, np.nan)

# La colonna 'X' è il YYYYMM (come nello script R)
if "X" not in dfraw.columns:
    raise KeyError("Nel CSV mi aspetto una colonna 'X' (YYYYMM).")

# Costruisci year, month, date
df = dfraw.copy()
df["X_str"] = df["X"].astype(str)
df["year"] = df["X_str"].str[:4].astype(int)
df["month"] = df["X_str"].str[4:6].astype(int)
df["date"] = yyyymm_to_date(df["X"])

# Filtro dal 1990 in poi e drop della colonna X (come R: select(-X))
df = df[df["year"] >= START_YR].copy()
df = df.drop(columns=["X"])

# Isoliamo i 100 portafogli per POSIZIONE come nello script R:
# Dopo il drop di X, in R usi across(c(1..100), ecc.). Emuliamo:
# Togliamo eventuali colonne helper per ottenere una vista con i 100 portafogli in testa
helper_cols = ["X_str", "year", "month", "date"]
portfolios_only = df.drop(columns=[c for c in helper_cols if c in df.columns], errors="ignore").copy()

# Se il file ha più di 100 colonne numeriche (es. extra), tagliamo a 100 come in R (1..100)
if portfolios_only.shape[1] < 100:
    raise ValueError(f"Mi aspettavo almeno 100 colonne di portafogli, trovate {portfolios_only.shape[1]}")
portfolios_only = portfolios_only.iloc[:, :100]

# ------------------------------------------------------------
# 2) Costruzione matrice 3x3 e SMB/HML (stesse posizioni 1-based del tuo R)
# ------------------------------------------------------------
pos_SizeSmall_BMlow  = [1,2,3, 11,12,13, 21,22,23]
pos_SizeSmall_BMMed  = [4,5,6,7, 14,15,16,17, 24,25,26,27]
pos_SizeSmall_BMHigh = [8,9,10, 18,19,20, 28,29,30]

pos_SizeMed_BMlow    = [31,32,33, 41,42,43, 51,52,53, 61,62,63]
pos_SizeMed_BMMed    = [34,35,36,37, 44,45,46,47, 54,55,56,57, 64,65,66,67]
pos_SizeMed_BMHigh   = [38,39,40, 48,49,50, 58,59,60, 68,69,70]

pos_SizeBig_BMlow    = [71,72,73, 81,82,83, 91,92,93]
pos_SizeBig_BMMed    = [74,75,76,77, 84,85,86,87, 94,95,96,97]
pos_SizeBig_BMHigh   = [78,79,80, 88,89,90, 98,99,100]

matrix = pd.DataFrame({
    "date": df["date"].values,
    "SizeSmall_BMlow":  row_mean_by_positions(portfolios_only, pos_SizeSmall_BMlow),
    "SizeSmall_BMMed":  row_mean_by_positions(portfolios_only, pos_SizeSmall_BMMed),
    "SizeSmall_BMHigh": row_mean_by_positions(portfolios_only, pos_SizeSmall_BMHigh),
    "SizeMed_BMlow":    row_mean_by_positions(portfolios_only, pos_SizeMed_BMlow),
    "SizeMed_BMMed":    row_mean_by_positions(portfolios_only, pos_SizeMed_BMMed),
    "SizeMed_BMHigh":   row_mean_by_positions(portfolios_only, pos_SizeMed_BMHigh),
    "SizeBig_BMlow":    row_mean_by_positions(portfolios_only, pos_SizeBig_BMlow),
    "SizeBig_BMMed":    row_mean_by_positions(portfolios_only, pos_SizeBig_BMMed),
    "SizeBig_BMHigh":   row_mean_by_positions(portfolios_only, pos_SizeBig_BMHigh),
})

# SMB & HML come in R
matrix["SMB"] = (
    (matrix["SizeSmall_BMlow"] + matrix["SizeSmall_BMMed"] + matrix["SizeSmall_BMHigh"]) / 3
    - (matrix["SizeBig_BMlow"]  + matrix["SizeBig_BMMed"]  + matrix["SizeBig_BMHigh"]) / 3
)
matrix["HML"] = (
    (matrix["SizeSmall_BMHigh"] + matrix["SizeMed_BMHigh"] + matrix["SizeBig_BMHigh"]) / 3
    - (matrix["SizeSmall_BMlow"] + matrix["SizeMed_BMlow"] + matrix["SizeBig_BMlow"]) / 3
)


# ------------------------------------------------------------
# 4) Correlazione Spearman tra SMB e HML
# ------------------------------------------------------------
corr_spearman = matrix[["SMB", "HML"]].corr(method="spearman")
print("Spearman corr (SMB vs HML):")
print(corr_spearman)

# ------------------------------------------------------------
# 5) Market Beta (MB = media dei 9 portafogli 3x3)
# ------------------------------------------------------------
group_cols = [
    "SizeSmall_BMlow","SizeSmall_BMMed","SizeSmall_BMHigh",
    "SizeMed_BMlow","SizeMed_BMMed","SizeMed_BMHigh",
    "SizeBig_BMlow","SizeBig_BMMed","SizeBig_BMHigh"
]
index_df = matrix[["date"] + group_cols + ["SMB", "HML"]].copy()
index_df["MB"] = index_df[group_cols].mean(axis=1, skipna=True)
index_df = index_df[["date", "MB", "SMB", "HML"]]

# ------------------------------------------------------------
# 6) Import RFR e calcolo tasso mensile
# ------------------------------------------------------------
rfr_raw = pd.read_csv(RFR_CSV)
# Date in formato %m/%d/%Y
rfr = rfr_raw.copy()
rfr["Date"] = pd.to_datetime(rfr["Date"], format="%m/%d/%Y")
rfr["ym"] = rfr["Date"].dt.to_period("M").astype(str)  # YYYY-MM
rfr = (
    rfr.groupby("ym", as_index=False)
       .agg(media_value=("Value", "mean"))
)
# R mensile da R annuo medio del mese
rfr["monthlyRFR"] = (1 + rfr["media_value"])**(1/12) - 1
# Filtro range come in R
rfr = rfr[(rfr["ym"] >= "1990-01") & (rfr["ym"] <= "2025-06")].copy()
rfr["date"] = pd.to_datetime(rfr["ym"] + "-01")

# ------------------------------------------------------------
# 7) Sottrazione RFR da MB (excess market return)
# ------------------------------------------------------------
indexMBadj = index_df.copy()
indexMBadj["ym"] = indexMBadj["date"].dt.to_period("M").astype(str)
indexMBadj = indexMBadj.merge(rfr[["date", "monthlyRFR"]], left_on="date", right_on="date", how="left")
indexMBadj["MB"] = indexMBadj["MB"] - indexMBadj["monthlyRFR"]
indexMBadj = indexMBadj[["date", "SMB", "HML", "MB"]].rename(columns={"date": "ym_date"})
indexMBadj["ym"] = indexMBadj["ym_date"].dt.to_period("M").astype(str)

# Grafico SMB, HML, MB con media mobile
# Definisci mappa colori globale
# Palette coerente per tutti i grafici
# Palette coerente
colors = {
    "MB":  "#FF7F0E",   # arancio-rosso
    "SMB": "#1F77B4",   # blu
    "HML": "#2CA02C"    # verde
}

# Reshape + rolling mean
plot_dataMB = (
    indexMBadj[["ym_date", "SMB", "HML", "MB"]]
    .rename(columns={"ym_date": "date"})
    .melt(id_vars="date", var_name="Factor", value_name="Value")
)
plot_dataMB = rolling12_grouped(
    plot_dataMB, value_col="Value", by_col="Factor", date_col="date"
)

title_map = {"MB": "MarketBeta", "SMB": "SMB", "HML": "HML"}

# --- Grafici singoli ---
for factor in ["MB", "SMB", "HML"]:
    sub = plot_dataMB[plot_dataMB["Factor"] == factor].copy()
    plt.figure()
    plt.plot(
        sub["date"], sub["Value"],
        alpha=0.4, linewidth=1,
        color=colors[factor], label=f"{factor} (serie)"
    )
    plt.plot(
        sub["date"], sub["roll12"],
        linewidth=2,
        color=colors[factor], label=f"{factor} (MM12)"
    )
    plt.title(f"Fattore {title_map[factor]} con media mobile (12 mesi)")
    plt.xlabel("Data"); plt.ylabel("Rendimento")
    plt.legend(); plt.tight_layout()

# --- Grafico SMB e HML insieme ---
plt.figure()
for factor in ["SMB", "HML"]:
    sub = plot_dataMB[plot_dataMB["Factor"] == factor].copy()
    plt.plot(
        sub["date"], sub["Value"],
        alpha=0.4, linewidth=1,
        color=colors[factor], label=f"{factor} (serie)"
    )
    plt.plot(
        sub["date"], sub["roll12"],
        linewidth=2,
        color=colors[factor], label=f"{factor} (MM12)"
    )
plt.title("Fattori SMB e HML con media mobile (12 mesi)")
plt.xlabel("Data"); plt.ylabel("Rendimento")
plt.legend(); plt.tight_layout()


# Correlazioni Spearman
print("\nSpearman corr (SMB, HML, MB):")
print(indexMBadj[["SMB","HML","MB"]].corr(method="spearman"))

# ------------------------------------------------------------
# 8) Sottraggo RFR anche ai singoli portafogli 3x3 e preparo regressioni
# ------------------------------------------------------------
matrixFinal = matrix.merge(index_df[["date","MB"]], on="date", how="left") \
                   .merge(rfr[["date","monthlyRFR"]], on="date", how="left")

# Colonne dei 9 portafogli (come in R: 2..10 su 'matrix' che qui sono group_cols)
for c in group_cols:
    matrixFinal[c] = matrixFinal[c] - matrixFinal["monthlyRFR"]

# ------------------------------------------------------------
# 9) Regressioni e media degli Adjusted R^2
#    FF3F: y ~ MB + SMB + HML ; CAPM: y ~ MB
# ------------------------------------------------------------
colsFF = group_cols[:]  # i 9 portafogli 3x3
adjR2_FF = []
adjR2_CAPM = []

# Regressori (att.ne: già MB è excess market; SMB, HML sono già excess factors)
X_capm = matrixFinal[["MB"]]
X_ff   = matrixFinal[["MB","SMB","HML"]]

for col in colsFF:
    y = matrixFinal[col]
    adjR2_FF.append(adj_r2(y, X_ff))
    adjR2_CAPM.append(adj_r2(y, X_capm))

mean_adjR2FF   = float(np.nanmean(adjR2_FF))
mean_adjR2CAPM = float(np.nanmean(adjR2_CAPM))

print(f"\nMean Adjusted R² (FF3F):  {mean_adjR2FF:.4f}")
print(f"Mean Adjusted R² (CAPM):  {mean_adjR2CAPM:.4f}")

# ------------------------------------------------------------
# 10) Mostra i grafici (se stai eseguendo in locale)
# ------------------------------------------------------------
plt.show()
