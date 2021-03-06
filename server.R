library(shiny)
library(ggplot2)

source("Functions/plotting.R")

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
    plotBacktesting(starting, ending, backtesting_start, backtesting_end,
                    coef, scale, 
                    Dane_Uzupelnione, predykcjaBacktesting)
    
  })
  
  output$Historia_danych_trend <- renderPlot({
    
    plotHistory(Dane_Uzupelnione, Dekompozycja$time.series[,"trend"])
    
  })
  
  output$Decomposition <- renderPlot({
    acf(Dekompozycja$time.series)
  })
  
  observeEvent(input$save, {
    write.table(PredykcjaTmp[,c("date","x","Corrected")], file = paste0("OUTPUT/",input$CSV_name, sep=""), sep=";", row.names = FALSE)
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