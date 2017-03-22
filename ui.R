library(shiny)

shinyUI(fluidPage(
  titlePanel("DepoForecast"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Settings"),
      
    sliderInput("trend_mod", label=h5("Trend modification (MM daily runoff/inflow)"), 
      min = -50, max = 50, value = 0),
    
    dateInput("ending_date", label=h5("Ending date"),
              value=as.Date(ending_day), 
              min = as.Date(starting_day + 1), max=as.Date(last_day_available)),
    
    textInput("CSV_name", label=h5("Filename:"), 
              value = paste("depo_forecast_", format(Sys.time(), "%y%m%d"), ".csv", sep="")),
    
    actionButton("save", label = "Save", position = "middle", icon("glass"), 
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    mainPanel(
      plotOutput("depo_forecast"),
      uiOutput("Change_nameProp")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Backtesting"),
    
    sliderInput("trend_mod_testing", label=h5("Trend modification (MM daily runoff/inflow)"), 
                min = -50, max = 50, value = 0),
    
    dateRangeInput("dates_backtesting", 
              label = h5("Backtesting range"),
              start = as.Date("2017-01-01"), 
              end   = as.Date(starting_day))
    ),
    
    mainPanel(
      plotOutput("backtesting")
    )
  ),
  
  fluidRow(
    h4("Obs. history"),
    plotOutput("Historia_danych_trend")
  ),
  
  fluidRow(
    tableOutput("Monthly_history")
  ),
  
  fluidRow(
    h4("Trend decomposition analysis"),
    plotOutput("Decomposition")
  )
  
))