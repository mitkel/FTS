###############################################################
#       CASTING BACK TO TRUE DATES
###############################################################

cast_2_true_dates <- function(predykcja, start_day = starting_day)
{
  predykcjaFinal <- predykcja[retreive_true_dates(length(predykcja[,1]), start = as.Date(start_day), uzup_dzien = uzup_dzien),]
  predykcjaFinal[,"date"] <- seq.Date(from = as.Date(start_day), by ="day", along.with = predykcjaFinal[,1])
  return(predykcjaFinal)
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
    excess <- 31 - day(timeLastDayInMonth(day))
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
      excess <- 31 - day(timeLastDayInMonth(day))
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