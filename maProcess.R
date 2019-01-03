maProcess <- function(data, N){
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,7))
  }
  
  MA <- forecast(ma(train, 3), h=N)
  
  fitted <- MA$mean
  n <- length(train)+1
  m <- n+N-1
  
  if(N<10){
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2017,9-N))
  } else {
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2016,8)) 
  }
  
  e <- abs((compare-fitted)/compare) * 100
  e <- ifelse(is.na(e)==TRUE, 0, e)
  e <- ifelse(e==Inf, 0, e)
  
  print(forecast::accuracy(MA$mean, occupancy))
  print(mean(e))
  
  autoplot(compare) +
    autolayer(MA, series="MA", alpha=0.5) +
    xlab("") + ylab("Occupancy (%)")  + 
    theme(panel.background = element_blank()) + 
    scale_y_continuous(breaks=seq(0,100,25))
}
