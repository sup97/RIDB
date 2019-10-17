unitTest <- function(camp){
  occupancy <- ts(camp$occupancy, frequency = 12)
  occupancy2 <- diff(occupancy)
  
  result <- matrix(NA, 4, 5)
  colnames(result) <- c("ADF", "ADF2", "KPSS", "OCSB", "CH")
  rownames(result) <- c("statistic", "p-value", "IO", "p-value2")
  
  a <- unitrootTest(occupancy, lags=12, type=c("c"))
  b <- unitrootTest(occupancy2, lags=12, type=c("c"))
  result[1,1] <- round(a@test$statistic, 3)
  result[2,1] <- round(a@test$p.value[1], 3)
  result[3,1] <- round(b@test$statistic, 3)
  result[4,1] <- round(b@test$p.value[1], 3)
  
  a <- adfTest(occupancy, lags=12, type=c("c"))
  b <- adfTest(occupancy2, lags=12, type=c("c"))
  result[1,2] <- round(a@test$statistic, 3)
  result[2,2] <- round(a@test$p.value[1], 3)
  result[3,2] <- round(b@test$statistic, 3)
  result[4,2] <- round(b@test$p.value[1], 3)
  
  a <-  tseries::kpss.test(occupancy, null="Level", lshort=FALSE)
  b <-  tseries::kpss.test(occupancy2, null="Level", lshort=FALSE)
  result[1,3] <- round(a$statistic, 3)
  result[2,3] <- round(a$p.value, 3)
  result[3,3] <- round(b$statistic, 3)
  result[4,3] <- round(b$p.value, 3)
  
  a <-  ocsb.test(occupancy, lag.method = c("fixed"), maxlag = 12)
  b <-  ocsb.test(occupancy2, lag.method = c("fixed"), maxlag = 12)
  result[1,4] <- round(a$statistic, 3)
  result[3,4] <- round(b$statistic, 3)

  a <- uroot::ch.test(occupancy, type = c("dummy"), lag1 = TRUE)
  b <- uroot::ch.test(occupancy2, type = c("dummy"), lag1 = TRUE)
  result[1,5] <- round(a$statistic[1], 3)
  result[2,5] <- round(a$pvalues[1], 3)
  result[3,5] <- round(b$statistic[1], 3)
  result[4,5] <- round(b$pvalues[1], 3)
  
  print(ocsb.test(occupancy2, lag.method = c("fixed"), maxlag = 12))
  return(result)
}
