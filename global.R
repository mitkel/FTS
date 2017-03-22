################################
# WCZYTYWANIE BIBLIOTEK
################################
library(foreign)
library(zoo)
library(stats)
library(reshape)
library(forecast)
library(directlabels)
library(data.table)
library(lubridate)
library(chron)
library(timeDate)

options(scipen=999) # Odpowiedni format liczb

################################
# PARAMETRY POCZATKOWE
################################

ileMcy <- 1
ileDni <- ileMcy * 31
coef <- 0
scale <- 10^6
uzup_dzien <- 13

################################
# WCZYTYWANIE DANYCH
################################

source("Functions/data_upload.R")

Dane <- dane(scale=scale)
Dane_Uzupelnione <- dane_uzupelnione(Dane, uzup_dzien = uzup_dzien)
obs_number <- length(Dane_Uzupelnione[,1])

starting_day <- Dane_Uzupelnione[obs_number,"date"]
ending_day <- add_month(starting_day)

################################
# DEKOMPOZYCJA I PROGNOZA
################################

source("Functions/prediction.R")
source("Functions/casting_back.R")

Dekompozycja <- stl(ts(Dane_Uzupelnione$bal_amt, frequency = 31), "periodic")
trend_szereg <- as.data.frame(Dekompozycja$time.series[,"trend"])
Holt_Winters_Forecast <- holt_winters_prognoza(trend_szereg, liczba_dni = 12*31)
predykcja <- prediction(Dane_Uzupelnione, trend_szereg, liczba_dni = 12*31)
predykcja[,"Corrected"] <- predykcja$x
predykcjaFinal <- cast_2_true_dates(predykcja)

casted_obs_number <- length(predykcjaFinal[,1])
last_day_available <- predykcjaFinal$date[casted_obs_number]

################################
# BACKTESTING
################################

predykcjaBacktesting <- predykcjaFinal
backtesting_start <- as.Date(predykcjaBacktesting$date[1])
backtesting_end <- as.Date(last(predykcjaBacktesting$date))