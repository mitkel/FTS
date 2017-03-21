#######################################################################################################################################################
#           Funkcje Bartek
#######################################################################################################################################################

ffill <- function(A) {
  sapply(A, function(x) { 
    current <<- ifelse(is.na(x), current, x); current })
}

mean_fill <- function(v){
  NA_index <- which(is.na(v), arr.ind = TRUE)
  for(i in NA_index){
    if(!is.na(v[i])) next
    j = i
    while(is.na(v[j+1])) {
      j<-j+1
    }
    if(j==length(v)){
      v<-ffill(v)
    }
    else
    {
      start = v[i-1]
      end = v[j+1]
      for(k in seq(from=i, to=j)){
        v[k]=v[i-1]+(k-(i-1))*(v[j+1]-v[i-1])/(j+1-(i-1))
      }
    }
  }
  return(v)
}

# dodaje brakuj?ce dni (weekendy)
df_missing_update <- function(df_ts, day_added = 13, basis = "day") {
  pocz <- df_ts[1,1]
  konc <- tail(df_ts[,1], n=1)
  cName <- colnames(df_ts)[1]
  colNo <- ncol(df_ts)
  months_30 <- c(11,6,9,4)
  t1 <- as.data.frame(seq.Date(as.Date(pocz, "%Y-%m-%d"), as.Date(konc, "%Y-%m-%d"), by = basis)); colnames(t1) <- cName

  df_missing_update <- merge(df_ts, t1, by = cName, all = TRUE)
  ile_dodane <- 0
  
  for(i in 1:length(df_missing_update[,1])){
    j <- i + ile_dodane      
    
    row_new <- t(as.data.frame(c(df_missing_update[j,1], rep(NA, colNo-1))))
    colnames(row_new) <- colnames(df_missing_update)
    dni <- 0    
    
    if(month(df_missing_update[j,1]) %in% months_30 && day(df_missing_update[j,1])==day_added)
    { 
      dni <- 1    } 
    else if(month(df_missing_update[j,1]) == 2 && day(df_missing_update[j,1])==day_added)
    {
      if(year(df_missing_update[j,1]) %% 4 == 0) 
        {dni <- 2}
      else {dni <- 3}
    }
      
      if(dni>0){
        for(a in 1:dni){
          df_missing_update <- rbind(df_missing_update[1:j,], row_new, df_missing_update[-(1:j),])
        }}
      
      ile_dodane <- ile_dodane + dni
    }
  return(df_missing_update)
}

rm_excess <- function(A,freq){
  if(length(A[,1]) %% freq == 0){end <- 0}
  else{
    end <- length(A[,1]) %% freq
  }
  return(A[(1:(length(A[,1])-end)),])
}

###############################################################
#       WYB?R MIESI?CY W ZAL?ZNO?CI OD PIKU
###############################################################

##### Analiza ze wzgl?du na pik 10. dnia miesi?ca
extract_wypl <- function(df, dzien){
  df <- check_wypl(df)
  day <- df$pik[as.Date(dzien) == as.Date(df$date)]
  Idx <- (df$pik == day)
  return(df[Idx,])
}

#sprawdza, czy w danym miesiacu 10 jest dniem roboczym. Dzia?a dla wektor?w
check_wypl <- function(df){
  res <- logical()
  i <- 1
  for( d in df$date ){
    if(  wday(as.Date(paste(format(as.Date(d), "%Y-%m"), "10", sep="-"))) == 7 ) 
      df$pik[i] <- 9
    else if(  wday(as.Date(paste(format(as.Date(d), "%Y-%m"), "10", sep="-"))) == 1 ) 
      df$pik[i] <- 8
    else if(wday(as.Date(paste(format(as.Date(d), "%Y-%m"), "10", sep="-"))) == 2 )
      df$pik[i] <- 7
    else 
      df$pik[i] <- 10
    i <- i+1
  }
  return(df)
}

kiedy_pik <- function(data){
  miesiac <- as.data.frame(seq(as.Date(data), as.Date(timeLastDayInMonth(as.Date(data))), by = "day"))
  if(wday(as.Date(paste(format(as.Date(data), "%Y-%m"), "10", sep="-"))) == 7 ) 
    pik <- 9
  else if(  wday(as.Date(paste(format(as.Date(data), "%Y-%m"), "10", sep="-"))) == 1 ) 
    pik <- 8
  else if(wday(as.Date(paste(format(as.Date(data), "%Y-%m"), "10", sep="-"))) == 2 )
    pik <- 7
  else 
    pik <- 10
  return(pik)
}

wybierz_miesiace_pik <- function(pik, df){
  df <- check_wypl(df)
  Idx <- (df$pik == pik)
  return(df[Idx,])
}
  
##### Analiza ze wzgl?du na ostatni dzie? miesi?ca
# funkcja zwracaj?ca ostatni dzie? miesi?ca jako dat?
eom <- function(dzien){
  
  if(month(as.Date(dzien)) == 12){
    eom <- as.Date(paste(year(as.Date(dzien)), "12", "31", sep="-"))
  }
  else{
    eom <- as.Date(paste(year(as.Date(dzien)), month(as.Date(dzien))+1, "01", sep="-"))-1
  }
  return(eom)
}

idx_of_months_ending_with <- function(days_ts, day)
{
  Idx <- logical()
  for(d in days_ts){
    if(wday(eom(as.Date(d))) == day){
      Idx <- c(Idx, TRUE)
    }
    else{
      Idx <- c(Idx, FALSE)
    }
  }
  return(Idx)
}


# zwraca indeksy dat, kt?re s? prawdziwymi datami
retreive_true_dates <- function(dlugosc_szeregu, start, uzup_dzien)
{
  # start jest pierwsz? dat? poprzedzaj?c? szereg
  day <- as.Date(start)
  Idx <- logical()
  licz <- 0
  
  # check na sytuacj? kiedy poczatek prognozy jest uzupe?nionym dniem
  if(day(as.Date(day)) == uzup_dzien){
    excess <- 31 - day(eom(day))
    licz <- licz + excess + 1
    day <- day + 1
    
    while(excess > 0){
      Idx <- c(Idx, FALSE)
      excess <- excess - 1
    }}
  
  while(licz < dlugosc_szeregu){
    if(day(as.Date(day)) == uzup_dzien){
      Idx <- c(Idx, TRUE)
      day <- day + 1
      excess <- 31 - day(eom(day))
      licz <- licz + excess + 1
      
      while(excess > 0){
        Idx <- c(Idx, FALSE)
        excess <- excess - 1
      }
    }
    else{
      Idx <- c(Idx, TRUE)
      licz <- licz + 1
      day <- day + 1
    }
  }
  return(Idx)
}

add_month <- function(dat){
  newD <- eom(dat) + 1
  newD <- as.Date(paste(year(newD),month(newD),day(dat), sep="-"))
  return(newD)
}

cast_prediction <- function(predykcja, start_day = starting_day){
  predykcjaFinal <- predykcja[retreive_true_dates(length(predykcja[,1]), start = as.Date(start_day), uzup_dzien = uzup_dzien),]
  predykcjaFinal[,"date"] <- seq.Date(from = as.Date(start_day), by ="day", along.with = predykcjaFinal[,1])
  return(predykcjaFinal)
}


#######################################################################################################################################################
#           Obr?bka danych
#######################################################################################################################################################


dane <- function(scale=1){
  x <- read.csv(file = "data/hist RB KO ON.csv", header = TRUE, sep = ";", dec = ".") # wczytywanie danych
  Y <- x[c("Data", "Segment", "tenor_org", "Iso_crncy_cde", "bal_amt")] # wyci?ganie odpowiednich kolumn
  colnames(Y) <- c("date", "segment", "prod", "ccy", "bal_amt") # nadanie odpowiednich nazw
  
  Y_filled <- Y[c("date", "bal_amt")][Y$ccy == "PLN" & Y$segment == "IND" & Y$prod == "NYD", ] # filtrowanie danych
  # Y_filled <- Y[c("date", "bal_amt")][Y$ccy == "PLN" & (Y$segment == "SME") & (Y$prod == "NYD" | Y$prod == "YD"),] # filtrowanie danych
  
  Y_filled <- aggregate(Y_filled$bal_amt, list(Y_filled$date), sum) # agregacja danych po dacie
  colnames(Y_filled) <- c("date", "bal_amt") # zmiana nazwa dw?ch pozosta?ych kolumn
  Y_filled$date <- as.Date( Y_filled$date, '%Y-%m-%d') # ustalanie formatu daty
  
  Y_filled$bal_amt <- Y_filled$bal_amt/scale
  return(Y_filled)
}

dane_uzupelnione <- function(Y,uzup_dzien=13){
  Y_filled <- Y
  Y_filled$date <- as.Date( Y_filled$date, '%Y-%m-%d') # ustalanie formatu daty
  Y_filled <- df_missing_update_brakujace_daty(Y_filled) # uzupe?nianie dat
  Y_filled <- df_missing_update(Y_filled, uzup_dzien) # dodawanie sztucznych dat do miesiecy, ktore nie maja 31 dni
  Y_filled$bal_amt[1] <- Y_filled$bal_amt[2] # uzupe?nienie wartosci dla pierwszego stycznia 2015
  Y_filled$bal_amt <- as.numeric(Y_filled$bal_amt) 
  Y_filled$bal_amt <- mean_fill(Y_filled$bal_amt) # uzupelnianie brakujacych wartosci srednia
  Y_filled[,"dzien"] <- seq(1:length(Y_filled[,1]))
  return(Y_filled)
}

tworzenie_szeregu <- function(df, czestosc = 31){
  series <- ts(df$bal_amt, frequency = czestosc)
  series <- as.data.frame(series)
  series["dzien"] <- seq(1:length(df[,1]))
  return(series)
}

holt_winters_prognoza <- function(szereg, colname = "x", liczba_dni = 31, czestosc = 372, add_czy_multi = "additive"){
  Filter <- HoltWinters(ts(szereg[1:(length(szereg[,1])-1),colname], frequency = czestosc), seasonal = add_czy_multi)
  Forecast <- as.data.frame(forecast.HoltWinters(Filter, h=liczba_dni))
  return(Forecast)
}

prediction <- function(dane, Szereg, liczba_dni = 31){
  trend <- holt_winters_prognoza(Szereg, liczba_dni = liczba_dni)
  ostatni_dzien <- as.Date(timeLastDayInMonth(last(dane$date)))
  pik <- extract_wypl(dane, as.Date(timeFirstDayInMonth(last(dane$date))))
  pik <- sezonowosc(scalaj(pik, dane))
  ostatni_dzien <- weekdays(ostatni_dzien)
  czynnik_sezonowy <- filtruj_miesiace2(dane, ostatni_dzien)
  czynnik_sezonowy <- sezonowosc(scalaj(czynnik_sezonowy, dane))
  dzien_miesiaca <- day(dane$date[length(dane$date)])
  if(ostatni_dzien == "poniedziałek" || ostatni_dzien == "pi?tek" || ostatni_dzien == "sobota" || ostatni_dzien == "niedziela"){
    czynnik_sezonowy <- zmiana_czynnika_sezon(czynnik_sezonowy, pik, ostatni_dzien)
  }  
  prognoza <- dane$bal_amt[length(dane$bal_amt)] + trend$`Point Forecast`[(1:liczba_dni)] - trend$`Point Forecast`[1]
  prognoza <- as.data.frame(prognoza)
  prognoza$daty <- seq(1:liczba_dni)
  colnames(prognoza) <- c("x", "daty")
  for(i in 1:min(liczba_dni,(31-dzien_miesiaca+1))){
    prognoza$x[i] <- prognoza$x[i] + czynnik_sezonowy$x[(dzien_miesiaca+i-1)] - czynnik_sezonowy$x[dzien_miesiaca]
  }
  j <- 1
  while(liczba_dni > (31*j - dzien_miesiaca)){
    prognoza <- monthly_prediction(prognoza, dane, j, dzien_miesiaca, liczba_dni)
    j <- j+1
  }
  return(prognoza)
}

monthly_prediction <- function(prognoza, dane, j, dzien_miesiaca, liczba_dni){
  ostatni_dzien <- as.Date(timeLastDayInMonth(as.Date(timeLastDayInMonth(last(dane$date))) %m+% months(j)))
  pik <- kiedy_pik(as.Date(timeFirstDayInMonth(last(dane$date))) %m+% months(j))
  pik <- wybierz_miesiace_pik(pik, dane)
  pik <- sezonowosc(scalaj(pik, dane))
  ostatni_dzien <- weekdays(ostatni_dzien)
  ostatni_dzien_pop_mies <- as.Date(timeLastDayInMonth(as.Date(timeLastDayInMonth(last(dane$date))) %m+% months(j-1)))
  ostatni_dzien_pop_mies <- weekdays(ostatni_dzien_pop_mies)
  czynnik_sezonowy <- filtruj_miesiace2(dane, ostatni_dzien)
  czynnik_sezonowy <- scalaj(czynnik_sezonowy, dane)
  czynnik_sezonowy <- sezonowosc(czynnik_sezonowy)
  czynnik_sezonowy <- zmiana_czynnika_sezon(czynnik_sezonowy, pik, ostatni_dzien)
  hist <- historyczne_roznice(dane, ostatni_dzien_pop_mies)
  for(i in (j*31 - dzien_miesiaca + 2):min(liczba_dni, ((j+1)*31 - dzien_miesiaca + 1))){
    prognoza$x[i] <- prognoza$x[i] + czynnik_sezonowy$x[(i - (j*31 - dzien_miesiaca + 1))] - czynnik_sezonowy$x[1] - prognoza$x[j*31 - dzien_miesiaca + 2] + prognoza$x[j*31 - dzien_miesiaca + 1] + hist
  }
  return(prognoza)
}

zmiana_czynnika_sezon <- function(czynnik, pik, ostatni_dzien){
  dzien_pik <- pik$dzien[pik$x == max(pik$x)]
  if(dzien_pik == 7){
    for(i in 6:10){
      wzrost <- pik$x[i+1] - pik$x[i]
      czynnik$x[i+1] <- wzrost + czynnik$x[i]
    }
  }
  if(dzien_pik == 8){
    for(i in 7:10){
      wzrost <- pik$x[i+1] - pik$x[i]
      czynnik$x[i+1] <- wzrost + czynnik$x[i]
    }
  }
  if(dzien_pik == 9){
    for(i in 8:10){
      wzrost <- pik$x[i+1] - pik$x[i]
      czynnik$x[i+1] <- wzrost + czynnik$x[i]
    }
  }
  if(dzien_pik == 10){
    if(ostatni_dzien == "poniedzia?ek"){
      for(i in 7:10){
        wzrost <- pik$x[i+1] - pik$x[i]
        czynnik$x[i+1] <- wzrost + czynnik$x[i]
      }
    }
    if(ostatni_dzien == "pi?tek"){
      for(i in 8:10){
        wzrost <- pik$x[i+1] - pik$x[i]
        czynnik$x[i+1] <- wzrost + czynnik$x[i]
      }
    }
  }
  return(czynnik)
}

historyczne_roznice <- function(dane, ostatni_dzien){
  colnames(dane) <- c("date", "x")
  suma <- 0
  licznik <- 0
  data <- as.Date(dane$date[1])
  for(i in 1:floor(length(dane$x)/31)){
    data <- as.Date(timeLastDayInMonth(data))
    if(weekdays(data) == ostatni_dzien){
      suma <- suma + dane$x[as.Date(dane$date) == (data + 1)] - dane$x[as.Date(dane$date) == (data)]  
      licznik <- licznik + 1
    }
    data <- as.Date(data + 1)
  }
  return(suma/licznik)
}

rysuj_prognoze <- function(prognoza){
  return(ggplot(prognoza, aes(seq(1:(length(prognoza$x))),prognoza$x[1:(length(prognoza$x))])) + geom_line())
}

HWplot<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  hw_object<-HoltWinters(ts_object)
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  p <- ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) 
  #+ geom_vline(x=max(actual_values$time),  lty=2) + xlab('Time') + ylab('Value') + scale_colour_hue('')
  return(p)
}

df_missing_update_brakujace_daty <- function(df_ts){
  # just in case these aren't Dates. 
  df_ts$date <- as.Date(df_ts$date)
  # all desired dates.
  alldates <- data.table(date=seq.Date(as.Date("2015/1/1"), max(df_ts$date), by="day"))
  # merge
  dt <- merge(df_ts, alldates, by="date", all=TRUE)
  return(dt)
}


# Funkcja do tworzenia szeregu czasowego na podstawie wybranych miesi?cy
# UWAGA! - trzeba zadba? o odpowiednie nazwy kolumn
filtruj_miesiace2 <- function(df, dzien_tygodnia){
  X <- data.frame(date = numeric(), bal_amt = numeric(), stringsAsFactors = FALSE)
  df$bool <- NA
  licznik <- 0
  for(i in 1:length(df$date)){
    if(month(df$date[i]) == 1 || month(df$date[i]) == 3 || month(df$date[i]) == 5 || month(df$date[i]) == 7 || month(df$date[i]) == 8 || month(df$date[i]) == 10 || month(df$date[i]) == 12){
      df$bool[i] <- (weekdays(df$date[i]) == dzien_tygodnia && day(df$date[i]) == 31)
      if(df$bool[i] == TRUE){
        daty <- seq(1 + licznik*31, 31 + licznik*31)
        daty <- as.data.frame(daty)
        colnames(daty) <- c("date")
        licznik <- licznik + 1
        daty$bal_amt <- df$bal_amt[(i-30):i]
        X <- merge(X, daty, by = c("date","bal_amt"), all = TRUE)
      }
    }
    if(month(df$date[i]) == 4 || month(df$date[i]) == 6 || month(df$date[i]) == 9 || month(df$date[i]) == 11){
      df$bool[i] <- (weekdays(df$date[i]) == dzien_tygodnia && day(df$date[i]) == 30)
      if(df$bool[i] == TRUE){
        daty <- seq(1 + licznik*31, 31 + licznik*31)
        daty <- as.data.frame(daty)
        colnames(daty) <- c("date")
        licznik <- licznik + 1
        daty$bal_amt <- df$bal_amt[(i-30):i]
        X <- merge(X, daty, by = c("date","bal_amt"), all = TRUE)
      }
    }
    if(month(df$date[i]) == 2){
      if(year(df$date[i]) %% 4 == 0){
        df$bool[i] <- (weekdays(df$date[i]) == dzien_tygodnia && day(df$date[i]) == 29)
        if(df$bool[i] == TRUE){
          daty <- seq(1 + licznik*31, 31 + licznik*31)
          daty <- as.data.frame(daty)
          colnames(daty) <- c("date")
          licznik <- licznik + 1
          daty$bal_amt <- df$bal_amt[(i-30):i]
          X <- merge(X, daty, by = c("date","bal_amt"), all = TRUE)
        }
      }else{
        df$bool[i] <- (weekdays(df$date[i]) == dzien_tygodnia && day(df$date[i]) == 28)
        if(df$bool[i] == TRUE){
          daty <- seq(1 + licznik*31, 31 + licznik*31)
          daty <- as.data.frame(daty)
          colnames(daty) <- c("date")
          licznik <- licznik + 1
          daty$bal_amt <- df$bal_amt[(i-30):i]
          X <- merge(X, daty, by = c("date","bal_amt"), all = TRUE)
        }
      }
    }
  }
  return(X)
}

scalaj <- function(df, Y){
  ilosc_miesiecy <- length(df$bal_amt) / 31
  for(i in 1:(ilosc_miesiecy-1)){
    df$bal_amt[-(1:(31 + 31*(i-1)))] <- df$bal_amt[-(1:(31 + 31*(i-1)))] - df$bal_amt[i*31 + 1] + df$bal_amt[i*31] + Y$bal_amt[(i*31 + 1)] - Y$bal_amt[(i*31)]
  }
  return(df)
}

sezonowosc <- function(df){
  first_row <- df[length(df[,1]),]
  df <- rbind(df, first_row)
  df_szereg <- ts(df$bal_amt, frequency = 31)
  df_szereg <- stl(df_szereg, "periodic")
  df_szereg_seasonal <- df_szereg$time.series[,1]
  df_szereg_seasonal <- as.data.frame(df_szereg_seasonal)
  df_szereg_seasonal["dzien"] <- seq(1:length(df$bal_amt))
  return(df_szereg_seasonal[(1:31),])
}

df_missing_update_brakujace_daty <- function(df_ts){
  df_ts$date <- as.Date(df_ts$date)
  alldates <- data.table(date=seq.Date(as.Date("2015/1/1"), max(df_ts$date), by="day"))
  dt <- merge(df_ts, alldates, by="date", all=TRUE)
  return(dt)
}
