unitTest <- function(camp){
  occupancy <- ts(camp$occupancy, frequency = 12)
  
  result <- matrix(NA, 2, 5)
  colnames(result) <- c("ADF", "ADF2", "KPSS", "OCSB", "CH")
  a <- unitrootTest(occupancy, lags=12, type=c("c"))
  result[1,1] <- round(a@test$statistic, 3)
  result[2,1] <- round(a@test$p.value[1], 3)
  
  a <- adfTest(occupancy, lags=12, type=c("c"))
  result[1,2] <- round(a@test$statistic, 3)
  result[2,2] <- round(a@test$p.value[1], 3)
  
  a <-  tseries::kpss.test(occupancy, null="Level", lshort=FALSE)
  result[1,3] <- round(a$statistic, 3)
  result[2,3] <- round(a$p.value, 3)
  
  a <- ocsb.test(occupancy, lag.method = c("fixed"), maxlag = 12)
  result[1,4] <- round(a$statistic, 3)
  
  a <- uroot::ch.test(occupancy, type = c("dummy"), lag1 = TRUE)
  result[1,5] <- round(a$statistic[1], 3)
  result[2,5] <- round(a$pvalues[1], 3)
  return(result)
}
