###############################################################
#       Plotting Functions
###############################################################
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
                              values = c("darkblue", "orange"))
  return(p)
}

plotBacktesting <- function(starting, ending, starting_day, ending_day, coef, scale, 
                            Y, predykcjaBacktesting){
  if(starting != starting_day | ending != ending_day){
    Z <- Y[Y$date <= starting,]
    Decomposition <- stl(ts(Z$bal_amt, frequency = 31), "periodic")
    trend <- as.data.frame(Decomposition$time.series[,"trend"])
    HW_Forecast <- holt_winters_prognoza(trend, liczba_dni = as.numeric(floor((ending-starting+1)*31/28)))
    predykcja <- prediction(Z, trend, liczba_dni = as.numeric(floor((ending-starting+1)*31/28)))
    predykcja[,"Corrected"] <- predykcja$x
    
    predykcjaBacktesting <<- cast_2_true_dates(predykcja, start_day = starting)
  }

  PredykcjaTmp <- predykcjaBacktesting[predykcjaBacktesting$date <= ending,]
  korekta <- c(0, (1:(length(PredykcjaTmp[,1])-1))*coef*10^6/scale)
  PredykcjaTmp$Corrected <- PredykcjaTmp$x + korekta
  
  DaneTmp <- cast_2_true_dates(Y[Y$date>=starting,], start_day = starting)
  DaneTmp <- DaneTmp[DaneTmp$date <= ending,]
  
  q <- ggplot(PredykcjaTmp, aes(x = PredykcjaTmp$date)) + 
          geom_line(aes(y = PredykcjaTmp$Corrected, color="Corrected")) +
          geom_line(aes(y = PredykcjaTmp$x, color="Vanilla")) +
          geom_line(aes(y = DaneTmp$bal_amt, color="True")) +
          ggtitle("Forecast") +
          labs(x = "Days ahead", y = "Deposits level (MM)") +
          scale_colour_manual("",
                              breaks = c("Corrected", "Vanilla", "True"),
                              values = c("darkblue", "orange", "pink"))
  
  return(q)
}
    