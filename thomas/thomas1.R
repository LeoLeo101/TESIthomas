rm(list = ls())

# LIBRERIE

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(slider)
library(readr)

#CARICAMENTO DATI 

dfraw <- as.data.frame(read.csv("C:\\Users\\leoan\\OneDrive\\Desktop\\thomas\\FFdataEWV2.csv",skip = 1207, nrows = 1188))

#CALCOLO DATE E SELEZIONE DATE DI INTERESSE

df <- dfraw %>%
  mutate(across(everything(), ~ ifelse(. == -99.99, NA, .))) %>%
  mutate(
    # conversione in stringa
    X_str = as.character(X),
    # estrazione anno e mese
    year = substr(X_str, 1, 4),
    month = substr(X_str, 5, 6),
    # creazione data come Date
    date = my(paste0(month, year))   # formattazione mese-anno
  )%>%
  filter(year>=1990)%>%
  select(-X)

#CREAZIONE MATRICE 3X3 IN BASE A SIZE E BOOK TO MARKET

matrix <- df %>%
  
  mutate(SizeSmall_BMlow = rowMeans(across(c(1,2,3,11,12,13,21,22,23)), na.rm = TRUE))%>%
  mutate(SizeSmall_BMMed = rowMeans(across(c(4,5,6,7,14,15,16,17,24,25,26,27)), na.rm = TRUE))%>%
  mutate(SizeSmall_BMHigh = rowMeans(across(c(8,9,10,18,19,20,28,29,30)), na.rm = TRUE))%>%
  mutate(SizeMed_BMlow = rowMeans(across(c(31,32,33,41,42,43,51,52,53,61,62,63)), na.rm = TRUE))%>%
  mutate(SizeMed_BMMed = rowMeans(across(c(34,35,36,37,44,45,46,47,54,55,56,57,64,65,66,67)), na.rm = TRUE))%>%
  mutate(SizeMed_BMHigh = rowMeans(across(c(38,39,40,48,49,50,58,59,60,68,69,70)), na.rm = TRUE))%>%
  mutate(SizeBig_BMlow = rowMeans(across(c(71,72,73,81,82,83,91,92,93)), na.rm = TRUE))%>%
  mutate(SizeBig_BMMed = rowMeans(across(c(74,75,76,77,84,85,86,87,94,95,96,97)), na.rm = TRUE))%>%
  mutate(SizeBig_BMHigh = rowMeans(across(c(78,79,80,88,89,90,98,99,100)), na.rm = TRUE))%>%
  select(date,SizeSmall_BMlow,SizeSmall_BMMed,SizeSmall_BMHigh,
         SizeMed_BMlow,SizeMed_BMMed,SizeMed_BMHigh,
         SizeBig_BMlow,SizeBig_BMMed,SizeBig_BMHigh)%>%
  
  mutate(SMB = (SizeSmall_BMlow + SizeSmall_BMMed + SizeSmall_BMHigh)/3 -  (SizeBig_BMlow + SizeBig_BMMed + SizeBig_BMHigh)/3) %>%
  mutate(HML = (SizeSmall_BMHigh + SizeMed_BMHigh +SizeBig_BMHigh)/3 - (SizeSmall_BMlow + SizeMed_BMlow + SizeBig_BMlow)/3)


#CREAZIONE GRAFICI DI SMB E HML 

plot_data <- matrix %>%
  pivot_longer(cols = c(SMB,HML), names_to = "Factor", values_to = "Value") %>%
  arrange(date) %>%
  group_by(Factor) %>%
  mutate(roll12 = slide_dbl(Value, mean, .before = 11, .complete = TRUE)) %>%
  ungroup()

ggplot(plot_data, aes(x = date, color = Factor)) +
  geom_line(aes(y = Value), alpha = 0.5) +          # serie originale (più trasparente)
  geom_line(aes(y = roll12), linewidth = 1) +       # media mobile 12 periodi
  labs(title = "Fattori SMB e HML con media mobile (12 mesi)",
       x = "Data", y = "Rendimento", color = "Fattore") +
  theme_minimal()+ 
  scale_y_continuous(n.breaks = 20)+
  scale_x_date(
    breaks = seq(min(plot_data$date), max(plot_data$date), by = "5 years"),
    date_labels = "%Y"
  ) + facet_wrap(~ Factor, ncol = 1, scales = "free_y")


ggplot(plot_data, aes(x = date, color = Factor)) +
  geom_line(aes(y = Value), alpha = 0.5) +          # serie originale (più trasparente)
  geom_line(aes(y = roll12), linewidth = 1) +       # media mobile 12 periodi
  labs(title = "Fattori SMB e HML con media mobile (12 mesi)",
       x = "Data", y = "Rendimento", color = "Fattore") +
  theme_minimal()+ 
  scale_y_continuous(n.breaks = 20)+
  scale_x_date(
    breaks = seq(min(plot_data$date), max(plot_data$date), by = "5 years"),
    date_labels = "%Y"
  )

#CORRELAZIONE TRA SMB E HML

cor(matrix$SMB, matrix$HML, method = "spearman")


#CALCOLO MARKET BETA

index<- matrix %>%
  mutate(MB = rowMeans(across(c(2:10))))%>%
  select(date,MB,SMB,HML)

#IMPORT DATI RFR
dfrawMB <- as.data.frame(read_csv("C:\\Users\\leoan\\OneDrive\\Desktop\\thomas\\RFR.csv"))

dfRFR <- dfrawMB %>%
  mutate(
    Date     = as.Date(Date, format = "%m/%d/%Y"),
    mese_num = month(Date),
    ym       = format(Date, "%Y-%m")
  ) %>%
  group_by(ym) %>%
  summarise(
    media_value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(monthlyRFR = ((1+media_value)^(1/12))-1)%>%
  filter(ym>= "1990-01") %>%
  filter(ym<= "2025-06")

#SOTTRAZIONE TRA MB E RFR
indexMBadj <- index %>%
  mutate(
    ym = as.Date(paste0(format(as.Date(date), "%Y-%m"), "-01"))
  ) %>%
  select(ym, SMB, HML, MB) %>%
  left_join(dfRFR %>% mutate(ym = as.Date(paste0(ym, "-01"))), by = "ym") %>%
  mutate(MB = MB - monthlyRFR) %>%
  select(ym, SMB, HML, MB)


#PLOT DATA DI MB
plot_dataMB <- indexMBadj %>%
  pivot_longer(cols = c(SMB, HML, MB), names_to = "Factor", values_to = "Value") %>%
  arrange(ym) %>%
  group_by(Factor) %>%
  mutate(roll12 = slide_dbl(Value, mean, .before = 11, .complete = TRUE)) %>%
  ungroup()

ggplot(plot_dataMB, aes(x = ym, color = Factor)) +
  geom_line(aes(y = Value), alpha = 0.5) +        # serie originale
  geom_line(aes(y = roll12), linewidth = 1) +     # media mobile
  labs(title = "Fattori SMB, HML e MarketBeta con media mobile (12 mesi)",
       x = "Data", y = "Rendimento", color = "Fattore") +
  theme_minimal() +
  scale_y_continuous(n.breaks = 5) +
  scale_x_date(
    breaks = seq(min(plot_dataMB$ym), max(plot_dataMB$ym), by = "5 years"),
    date_labels = "%Y"
  ) +
  facet_wrap(
    ~ Factor,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(MB = "MarketBeta", SMB = "SMB", HML = "HML"))
  )+ theme(legend.position = "none")

#CORRELZIONE TRA INDICI
cor(indexMBadj[, c("SMB", "HML", "MB")], method = "spearman", use = "complete.obs")


#TOLGO IL RFR AI RENDIEMNTI DEI SINGOLI PORTAFOGLI

matrixFinal <- matrix %>%
  left_join(index %>% select(date, MB), by = "date")%>%
  left_join(dfRFR %>% mutate(date = as.Date(paste0(ym, "-01")))%>% select(date,monthlyRFR), by = "date") %>%
  mutate(across(2:10, ~ .x - monthlyRFR))

#FACCIO LE REGRESSIONI e calcolo l' adj R2 medio dei due

colsFF <- names(matrixFinal)[2:10]

adjR2FF <- sapply(colsFF, function(col) {
  model <- lm(as.formula(paste(col, "~ MB + SMB + HML")), data = matrixFinal)
  summary(model)$adj.r.squared
})

mean_adjR2FF <- mean(adjR2FF)


colsCAPM <- names(matrixFinal)[2:10]

adjR2CAPM <- sapply(colsCAPM, function(col) {
  model <- lm(as.formula(paste(col, "~ MB")), data = matrixFinal)
  summary(model)$adj.r.squared
})

mean_adjR2CAPM <- mean(adjR2CAPM)

print(mean_adjR2FF)
print(mean_adjR2CAPM)



