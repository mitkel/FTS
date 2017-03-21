###############################################################
#       Plotting Functions
###############################################################

plotPrediction <- function(starting_day, ending_day, coef, scale, PredykcjaFinal){
  PredykcjaTmp <- predykcjaFinal[predykcjaFinal$date <= ending_day,]
  
  korekta <- c(0, (1:(ending_day-starting_day))*coef*10^6/scale)
  PredykcjaTmp$poKorekcie <- PredykcjaTmp$x + korekta
  
  ggplot(PredykcjaTmp, aes(x = PredykcjaTmp$date)) + 
    geom_line(aes(y = PredykcjaTmp$poKorekcie, color="Corrected")) +
    geom_line(aes(y = PredykcjaTmp$x, color="Vanilla")) +
    ggtitle("Forecast") +
    labs(x = "Days ahead", y = "Deposits level (MM)") +
    scale_colour_manual("",
                        breaks = c("Corrected", "Vanilla"),
                        values = c("darkblue", "orange"))
}

plotBacktesting <- function(){
  
}
    