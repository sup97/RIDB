etsProcess <- function(data, N){  
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,7))
  }

  ETS <- forecast(ets(train), h=N)
  
  fitted <- ETS$mean
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
  
  print(forecast::accuracy(ETS$mean, occupancy))
  print(mean(e))
  
  autoplot(compare) +
    autolayer(ETS, series="ETS", alpha=0.5) +
    xlab("") + ylab("Occupancy (%)")  + 
    theme(panel.background = element_blank()) + 
    scale_y_continuous(breaks=seq(0,100,25))
}
