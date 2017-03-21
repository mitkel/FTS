library(shiny)
library(ggplot2)

source("plotting.R")

shinyServer(function(input, output, session) {
  
  output$depo_forecast <- renderPlot({
    
    coef <<- input$trend_mod
    ending_day <- input$ending_date
    plotPrediction(starting_day, ending_day, coef, scale, predykcjaFinal)
    
  })
  
  output$backtesting <- renderPlot({
    
    coef <- input$trend_mod_testing
    starting <- input$dates_backtesting[1]
    ending <- input$dates_backtesting[2]
    
    Idx1 <- Dane_Uzupelnione$date <= starting
    Idx2 <- Dane_Uzupelnione$date <= ending
    
    Dekompozycja <- stl(ts(Dane_Uzupelnione$bal_amt[Idx1], frequency = 31), "periodic")
    trend_szereg <- as.data.frame(Dekompozycja$time.series[,"trend"])
    Holt_Winters_Forecast <- holt_winters_prognoza(trend_szereg, liczba_dni = 12*31)
    predykcja <- prediction(Dane_Uzupelnione[Idx1,], trend_szereg, liczba_dni = 12*31)
    predykcja[,"poKorekcie"] <- predykcja$x
    
    predykcjaFinal <- cast_prediction(predykcja, start_day = starting)
    PredykcjaTmp <- predykcjaFinal[predykcjaFinal$date <= ending,]
    
    korekta <- c(0, (1:(ending-starting-1))*coef*10^6/scale)
    DaneTmp <- Dane_Uzupelnione[PredykcjaTmp$daty,]
    DaneTmp <- aggregate(DaneTmp$bal_amt, list(DaneTmp$date), sum)
    colnames(DaneTmp) <- c("date","bal_amt")
    # PredykcjaTmp$poKorekcie <<- PredykcjaTmp$poKorekcie + korekta
    
    ggplot(PredykcjaTmp, aes(x = PredykcjaTmp$date)) + 
      geom_line(aes(y = PredykcjaTmp$poKorekcie, color="Corrected")) +
      geom_line(aes(y = PredykcjaTmp$x, color="Vanilla")) +
      geom_line(aes(y = DaneTmp$bal_amt, color="original")) +
      ggtitle("Backtesting") +
      labs(x = "Days ahead", y = "Deposits level (MM)") +
      scale_colour_manual("",
                          breaks = c("Corrected", "Vanilla", "original"),
                          values = c("darkblue", "orange", "purple"))
    
  })
  
  output$Historia_danych_trend <- renderPlot({
    
    ggplot(Dane_Uzupelnione, aes(x=Dane_Uzupelnione$date)) + 
      geom_line(aes(y = Dane_Uzupelnione$bal_amt, color = "Historic level")) + 
      geom_line(aes(y = Dekompozycja$time.series[,"trend"], color = "Trend estimated")) +
      labs(x = "date", y = "Deposits level (MM)") +
      scale_colour_manual("",
        breaks = c("Historic level", "Trend estimated"),
        values = c("black", "blue"))
  })
  
  output$Decomposition <- renderPlot({
    acf(Dekompozycja$time.series)
  })
  
  observeEvent(input$save, {
    write.table(PredykcjaTmp[,c("date","x","poKorekcie")], file = paste0("OUTPUT/",input$CSV_name, sep=""), sep=";", row.names = FALSE)
  })
  
  observe({
    coef <- input$trend_mod
    init_name <- paste("depo_fcst_", format(Sys.time(), "%y%m%d"), sep="")
    if(coef==0){
      nameProp <- paste(init_name, ".csv", sep="")
    }
    else if(coef<0){
      nameProp <- paste(init_name,"_", abs(coef), "D.csv", sep="")
    }
    else{
      nameProp <- paste(init_name,"_", abs(coef), "U.csv", sep="")
    }
    updateSelectInput(session, "CSV_name", selected = nameProp)
  })
})