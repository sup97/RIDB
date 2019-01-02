maProcess <- function(data, N){
  data$month <- as.Date(data$month, "%Y-%m-%d")
  occupancy <- ts(data$occupancy, frequency = 12, start = c(2007, 5))
  Window <- window(occupancy, start=c(2016,8))
  print(accuracy(ma(occupancy, N),occupancy))
  
  fitted <- ma(occupancy, N)
  e <- abs((occupancy-fitted)/occupancy) * 100
  e <- e[!is.na(e)]
  e <- e[e!=Inf]
  print(mean(e))
  
  MA <- ma(Window, N)
  
   autoplot(occupancy) +
    autolayer(ma(Window,N), series=paste0(N, "-MA")) +
    xlab("") + ylab("") +
    theme(panel.background = element_blank()) + 
    scale_y_continuous(breaks=seq(0,100,25))
}
