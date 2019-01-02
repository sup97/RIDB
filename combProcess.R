combProcess <- function(data, N){  
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,7))
  }
  
  SARIMA <- forecast(auto.arima(train, 
                                trace = FALSE, 
                                lambda="auto", 
                                max.P=0, max.Q=0, 
                                seasonal = TRUE, 
                                ic = "bic", 
                                test = "adf",
                                approximation= FALSE,
                                allowmean = TRUE,
                                allowdrift = TRUE), h=N)
  
  ETS <- forecast(ets(train), h=N)
  NNAR <- forecast(nnetar(train, P=8, lambda = "auto", repeats = 100), h=N)
  
  Combination <- (ETS[["mean"]] + SARIMA[["mean"]] +
                    NNAR[["mean"]])/3
  
  
  fitted <- Combination
  n <- length(train)+1
  m <- n+N-1
  
  if(N<10){
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2017,9-N))
  }
  else {
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2016,8)) 
  }
  
  e <- abs((compare-fitted)/compare) * 100
  e <- e[!is.na(e)]
  e <- e[e!=Inf]
  
  print(forecast::accuracy(Combination, occupancy))
  print(mean(e))
  
  autoplot(compare) +
    autolayer(Combination, series="Combination", alpha=0.5) +
    xlab("") + ylab("Occupancy (%)")  + 
    theme(panel.background = element_blank()) + 
    scale_y_continuous(breaks=seq(0,100,25))
}
