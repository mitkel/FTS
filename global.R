################################
# WCZYTYWANIE BIBLIOTEK
################################
library(foreign)
library(zoo)
library(scales)
library(stats)
library(reshape)
library(forecast)
library(directlabels)
library(data.table)
library(lubridate)
library(data.table)
library(chron)
library(timeDate)

options(scipen=999) # Odpowiedni format liczb
source("myFunctions.R") # wczytywanie funkcji

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

Dane <- dane(scale=scale)
Dane_Uzupelnione <- dane_uzupelnione(Dane, uzup_dzien = uzup_dzien)
obs_number <- length(Dane_Uzupelnione[,1])

starting_day <- Dane_Uzupelnione[obs_number,"date"]
ending_day <- add_month(starting_day)
Idx <- Dane_Uzupelnione$date <= starting_day

################################
# DEKOMPOZYCJA I PROGNOZA
################################

Dekompozycja <- stl(ts(Dane_Uzupelnione$bal_amt[Idx], frequency = 31), "periodic")
trend_szereg <- as.data.frame(Dekompozycja$time.series[,"trend"])
Holt_Winters_Forecast <- holt_winters_prognoza(trend_szereg, liczba_dni = 12*31)
predykcja <- prediction(Dane_Uzupelnione[Idx,], trend_szereg, liczba_dni = 12*31)
predykcja[,"poKorekcie"] <- predykcja$x
predykcjaFinal <- cast_prediction(predykcja)
PredykcjaTmp <- predykcjaFinal[predykcjaFinal$date <= ending_day,]

casted_obs_number <- length(predykcjaFinal[,1])
last_day <- predykcjaFinal$date[casted_obs_number]
