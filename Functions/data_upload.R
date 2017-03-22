###############################################################
#           DATA UPLOAD
###############################################################

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
  #Y_filled <- df_missing_update_brakujace_daty(Y_filled) # uzupe?nianie dat
  Y_filled <- df_missing_update(Y_filled, uzup_dzien) # dodawanie sztucznych dat do miesiecy, ktore nie maja 31 dni
  #Y_filled$bal_amt[1] <- Y_filled$bal_amt[2] # uzupe?nienie wartosci dla pierwszego stycznia 2015
  Y_filled$bal_amt <- as.numeric(Y_filled$bal_amt) 
  Y_filled$bal_amt <- mean_fill(Y_filled$bal_amt) # uzupelnianie brakujacych wartosci srednia
  Y_filled[,"dzien"] <- seq(1:length(Y_filled[,1]))
  return(Y_filled)
}

# for time series data - we want to fill it with weakend, holidays and simply missing dates
# assigning NA by default
df_missing_update <- function(df_ts, day_added = 13, basis = "day", poczMca = TRUE) {
  if(poczMca){
    pocz <- as.Date(timeFirstDayInMonth(as.Date(df_ts[1,1])))
  }
  else{
    pocz <- df_ts[1,1]
  }
  
  konc <- tail(df_ts[,1], n=1)
  cName <- colnames(df_ts)[1]
  colNo <- ncol(df_ts)
  months_30 <- c(11,6,9,4)
  t1 <- as.data.frame(seq.Date(as.Date(pocz), as.Date(konc), by = basis)); colnames(t1) <- cName
  
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

# linear interpolation for NAs
mean_fill <- function(v){
  NA_index <- which(is.na(v), arr.ind = TRUE)
  for(i in NA_index){
    if(!is.na(v[i])) next
    
    j = i
    while(is.na(v[j+1])) {
      j<-j+1
    }
    if(i==1){
      v[1:j] <- v[j+1]
    }
    else if(j==length(v)){
      v <-ffill(v)
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

# fill NAs with the previous values
ffill <- function(A) {
  sapply(A, function(x) { 
    current <<- ifelse(is.na(x), current, x); current })
}

add_month <- function(dat){
  newD <- as.Date(timeLastDayInMonth(dat)) + 1
  if(day(timeLastDayInMonth(newD)) > day(dat)){
    newD <- as.Date(paste(year(newD),month(newD),day(dat), sep="-"))
  }
  else{
    newD <- as.Date(timeLastDayInMonth(newD))
  }
  return(newD)
}