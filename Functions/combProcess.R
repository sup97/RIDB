combProcess <- function(data, N){  
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  if(N<10){
    train <- window(occupancy, end=c(2017,8-N))
  } else {
    train <- window(occupancy, end=c(2016,8))
  }
  
  SARIMA <- forecast(auto.arima(train, lambda="auto", max.P=0, max.Q=0, seasonal = TRUE, trace = FALSE, 
                                test = c("kpss", "adf", "pp"),
                                seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                                approximation= FALSE,
                                allowmean = TRUE,
                                allowdrift = TRUE), h=N)
  
  ETS <- forecast(ets(train), h=N)
  NNAR <- forecast(nnetar(train, P=8, lambda = "auto", repeats = 100), h=N)
  KNN <- knn_forecasting(train, h = N, lags = 1:12, k = 3, msas = "MIMO")
  
  Combination <- (ETS[["mean"]] + SARIMA[["mean"]] +
                    NNAR[["mean"]] + KNN[["prediction"]])/4
  
  
  fitted <- Combination
  n <- length(train)+1
  m <- n+N-1
  
  if(N<10){
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2017,9-N))
  } else {
    compare <- ts(occupancy[n:m], frequency = 12, start = c(2016,9)) 
  }
  
  e <- abs((compare-fitted)/compare) * 100
  e <- ifelse(is.na(e)==TRUE, 0, e)
  e <- ifelse(e==Inf, 0, e)
  
  print(forecast::accuracy(Combination, occupancy))
  print(mean(e))
  
  gtemp.df = data.frame(Time=c(as.Date(time(compare))), 
                        gtemp=c(compare), 
                        gtempl=c(Combination))
  cbbPalette <- c("#D55E00", "#000000")
  
  ggplot(data = gtemp.df, aes(x=Time, y=value, color=series )  )             +
    ylab('Occupancy (%)')                                 +
    geom_line(aes(y=gtemp , col='Real'), size=1.2, alpha=.5)  +
    geom_line(aes(y=gtempl, col='Combination'), size=1.2, alpha=.5)    +
    theme(panel.background = element_blank()) + 
    scale_y_continuous(breaks=seq(0,100,25)) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
    scale_colour_manual(values=cbbPalette)

  
}
