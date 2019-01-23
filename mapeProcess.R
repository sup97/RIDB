mapeProcess <- function(data, N){  
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,8))
  }
  
  n <- length(train)+1
  m <- n+N-1
  
  if(N<10){
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2017,9-N))
  } else {
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2016,9)) 
  }
 
  if (N>2) {MA <- forecast(ma(train, 3), h=N)
  fitted <- MA$mean
  
  me <- abs((compare-fitted)/compare) * 100
  me <- ifelse(is.na(me)==TRUE, 0, me)
  me <- ifelse(me==Inf, 0, me)
  print(mean(me))
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
  fitted <- SARIMA$mean

  se <- abs((compare-fitted)/compare) * 100
  se <- ifelse(is.na(se)==TRUE, 0, se)
  se <- ifelse(se==Inf, 0, se)
  print(mean(se))
  
  ETS <- forecast(ets(train), h=N)
  fitted <- ETS$mean
  ee <- abs((compare-fitted)/compare) * 100
  ee <- ifelse(is.na(ee)==TRUE, 0, ee)
  ee <- ifelse(ee==Inf, 0, ee)
  print(mean(ee))
  
  NNAR <- forecast(nnetar(train, P=8, lambda = "auto", repeats = 100), h=N)
  fitted <- NNAR$mean
  ne <- abs((compare-fitted)/compare) * 100
  ne <- ifelse(is.na(ne)==TRUE, 0, ne)
  ne <- ifelse(ne==Inf, 0, ne)
  print(mean(ne))
  
  KNN <- knn_forecasting(train, h = N, lags = 1:12, k = 3, msas = "MIMO")
  fitted <- KNN$prediction
  ke <- abs((compare-fitted)/compare) * 100
  ke <- ifelse(is.na(ke)==TRUE, 0, ke)
  ke <- ifelse(ke==Inf, 0, ke)
  print(mean(ke))
  
  Combination <- (ETS[["mean"]] + SARIMA[["mean"]] +
                    NNAR[["mean"]] + KNN[["prediction"]])/4
  
  fitted <- Combination

  e <- abs((compare-fitted)/compare) * 100
  e <- ifelse(is.na(e)==TRUE, 0, e)
  e <- ifelse(e==Inf, 0, e)
  print(mean(e))
}
