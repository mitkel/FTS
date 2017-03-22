###############################################################
#       Plotting Functions
###############################################################
library(scales)
source("Functions/prediction.R")

plotPrediction <- function(starting_day, ending_day, coef, scale, PredykcjaFinal){
  PredykcjaTmp <- predykcjaFinal[predykcjaFinal$date <= ending_day,]
  
  korekta <- c(0, (1:(ending_day-starting_day))*coef*10^6/scale)
  PredykcjaTmp$Corrected <- PredykcjaTmp$x + korekta
  
  p <- ggplot(PredykcjaTmp, aes(x = PredykcjaTmp$date)) + 
          geom_line(aes(y = PredykcjaTmp$Corrected, color="Corrected")) +
          geom_line(aes(y = PredykcjaTmp$x, color="Vanilla")) +
          ggtitle("Forecast") +
          labs(x = "Days ahead", y = "Deposits level (MM)") +
          scale_colour_manual("",
                              breaks = c("Corrected", "Vanilla"),
                              values = c("darkblue", "orange")) +
          scale_y_continuous(breaks = round(seq(min(PredykcjaTmp$x, PredykcjaTmp$Corrected),
                                                max(PredykcjaTmp$x, PredykcjaTmp$Corrected),
                                                by = 100), -2))
  return(p)
}

plotBacktesting <- function(starting, ending, act_start, act_end,
                            coef, scale, 
                            Y, predykcjaBacktesting)
{
  if( starting != act_start | ending != act_end ){
    act_start <<- starting
    act_end <<- ending
    Z <- Y[Y$date <= starting,]
    Decomposition <- stl(ts(Z$bal_amt, frequency = 31), "periodic")
    trend <- as.data.frame(Decomposition$time.series[,"trend"])
    HW_Forecast <- holt_winters_prognoza(trend, liczba_dni = as.numeric(floor((ending-starting+1)*31/28)))
    predykcja <- prediction(Z, trend, liczba_dni = as.numeric(floor((ending-starting+1)*31/28)))
    
    predykcjaBacktesting <<- cast_2_true_dates(predykcja, start_day = starting)
  }

  PredykcjaTmp <- predykcjaBacktesting[predykcjaBacktesting$date <= ending,]
  
  DaneTmp <- cast_2_true_dates(Y[Y$date>=starting,], start_day = starting)
  DaneTmp <- DaneTmp[DaneTmp$date <= ending,]
  
  q <- ggplot(PredykcjaTmp, aes(x = PredykcjaTmp$date)) + 
          geom_line(aes(y = PredykcjaTmp$x, color="Vanilla")) +
          geom_line(aes(y = DaneTmp$bal_amt, color="True")) +
          ggtitle("Backtesting") +
          labs(x = "Days ahead", y = "Deposits level (MM)") +
          scale_colour_manual("",
                              breaks = c("Vanilla", "True"),
                              values = c("Vanilla"  ="orange", 
                                         "True"     = "red")) +
          scale_y_continuous(breaks = round(seq(min(PredykcjaTmp$x, DaneTmp$bal_amt),
                                                max(PredykcjaTmp$x, DaneTmp$bal_amt),
                                                by = 100), -2))
  return(q)
}
  
plotHistory <- function(Dane_Uzupelnione, trend){
  p <- ggplot(Dane_Uzupelnione, aes(x=Dane_Uzupelnione$date)) + 
          geom_line(aes(y = Dane_Uzupelnione$bal_amt, color = "Historic level")) + 
          geom_line(aes(y = trend, color = "Trend estimated")) +
          labs(x = "date", y = "Deposits level (MM)") +
          scale_colour_manual("",
                              breaks = c("Historic level", "Trend estimated"),
                              values = c("black", "blue"))
  return(p)
}