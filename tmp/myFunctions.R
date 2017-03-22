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

# returns boolean vector indicating if month of a day ends with a specific weekday
idx_of_months_ending_with <- function(days_ts, day)
{
  Idx <- logical()
  for(d in days_ts){
    if(wday(timeLastDayInMonth(as.Date(d))) == day){
      Idx <- c(Idx, TRUE)
    }
    else{
      Idx <- c(Idx, FALSE)
    }
  }
  return(Idx)
}

###############################################################
#       PREDICTION
###############################################################

holt_winters_prognoza <- function(szereg, colname = "x", liczba_dni = 31, czestosc = 372, add_czy_multi = "additive"){
  Filter <- HoltWinters(ts(szereg[1:(length(szereg[,1])-1),colname], frequency = czestosc), seasonal = add_czy_multi)
  Forecast <- as.data.frame(forecast.HoltWinters(Filter, h=liczba_dni))
  return(Forecast)
}

prediction <- function(dane, Szereg, liczba_dni = 31){
  trend <- holt_winters_prognoza(Szereg, liczba_dni = liczba_dni)

  #kolumna z numereami pikow dla danych mcy
  pik <- extract_wypl(dane, as.Date(timeFirstDayInMonth(last(dane$date))))
  pik <- sezonowosc(scalaj(pik, dane))
  ostatni_dzien <- wday(as.Date(timeLastDayInMonth(last(dane$date))))
  
  czynnik_sezonowy <- dane[idx_of_months_ending_with(dane$date, ostatni_dzien),]
  czynnik_sezonowy$daty <- seq(1:length(czynnik_sezonowy[,1]))
  czynnik_sezonowy <- sezonowosc(scalaj(czynnik_sezonowy, dane))
  
  dzien_miesiaca <- day(dane$date[length(dane$date)])
  if(ostatni_dzien %in%  c(1,2,6,7)){
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
  pik <- kiedy_pik(as.Date(timeFirstDayInMonth(last(dane$date))) %m+% months(j))
  pik <- wybierz_miesiace_pik(pik, dane)
  pik <- sezonowosc(scalaj(pik, dane))
  
  ostatni_dzien <- as.Date(timeLastDayInMonth(as.Date(timeLastDayInMonth(last(dane$date))) %m+% months(j)))
  ostatni_dzien <- wday(ostatni_dzien)
  ostatni_dzien_pop_mies <- as.Date(timeLastDayInMonth(as.Date(timeLastDayInMonth(last(dane$date))) %m+% months(j-1)))
  ostatni_dzien_pop_mies <- wday(ostatni_dzien_pop_mies)
  
  czynnik_sezonowy <- dane[idx_of_months_ending_with(dane$date, ostatni_dzien),]
  czynnik_sezonowy$daty <- seq(1:length(czynnik_sezonowy[,1]))
  
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
    if(ostatni_dzien == 2){
      for(i in 7:10){
        wzrost <- pik$x[i+1] - pik$x[i]
        czynnik$x[i+1] <- wzrost + czynnik$x[i]
      }
    }
    if(ostatni_dzien == 6){
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
    if(wday(data) == ostatni_dzien){
      suma <- suma + dane$x[as.Date(dane$date) == (data + 1)] - dane$x[as.Date(dane$date) == (data)]  
      licznik <- licznik + 1
    }
    data <- as.Date(data + 1)
  }
  return(suma/licznik)
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

#######################################################
#           OTHER
#######################################################

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