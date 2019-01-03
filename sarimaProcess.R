sarimaProcess <- function(data, N){  
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,7))
  }
  
  arimafit <- auto.arima(train, lambda="auto", max.P=0, max.Q=0, seasonal = TRUE, trace = FALSE, 
                       test = "adf",
                       approximation= FALSE,
                       allowmean = TRUE,
                       allowdrift = TRUE)

  ARIMA <- forecast(arimafit, h=N)
  print(summary(ARIMA))
  fitted <- ARIMA$mean
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

print(forecast::accuracy(ARIMA$mean, occupancy))
print(mean(e))

autoplot(compare) +
  autolayer(ARIMA, series="SARIMA", alpha=0.5) +
  xlab("") + ylab("Occupancy (%)")  + 
  theme(panel.background = element_blank()) + 
  scale_y_continuous(breaks=seq(0,100,25))
}
