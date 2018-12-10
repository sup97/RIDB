
formatCamp <- function(ID){
  n=ID
  name <- data[data$id==n & as.Date(data$date, format="%Y-%m-%d")<"2018-01-01",] 
  name$date <- as.Date(name$date, format = "%Y-%m-%d")
  min <- as.Date(min(name$date))
  max <- as.Date(max(name$date))
  name1 <- name %>%
    complete(date = seq(min, max, by="day")) %>%
    mutate(Campground = na.locf(Campground, na.rm=FALSE)) %>% 
    mutate(Park = na.locf(Park, na.rm=FALSE)) %>% 
    mutate(id = na.locf(id, na.rm=FALSE)) %>%
    mutate(occupancy = replace_na(occupancy, 0)) %>%
    mutate(avgfee = replace_na(avgfee, 0)) %>%
    mutate(aggPaid = replace_na(aggPaid, 0)) %>%
    mutate(avgPaid = replace_na(avgPaid, 0)) %>%
    mutate(avgNumberOfPeople = replace_na(avgNumberOfPeople, 0)) %>%
    mutate(totalNumberOfPeople = replace_na(totalNumberOfPeople, 0)) %>%
    mutate(avgdateDiff = replace_na(avgdateDiff, 0)) %>%
    mutate(avglengthStay = replace_na(avglengthStay, 0)) %>%
    mutate(numOfSites = na.locf(numOfSites, na.rm=FALSE)) %>% 
    mutate(recGovNumOfSites = na.locf(recGovNumOfSites, na.rm=FALSE)) %>%
    mutate(occupancy = 100*occupancy) %>%
    distinct(date, .keep_all=TRUE)
  
  fwrite(name, paste0("descriptive_", ID, ".csv"))
  fwrite(name1, paste0("campground_", ID, ".csv"))
  
  name$month <- format(as.Date(name$date, format = "%Y-%m-%d"), "%Y-%m")
  nameM <- name %>%
    group_by(month) %>%
    mutate(totalNumberOfPeople = sum(totalNumberOfPeople)) %>%
    mutate(avgNumberOfPeople = mean(avgNumberOfPeople)) %>%
    mutate(occupancy = mean(occupancy)) %>%
    mutate(avgPaid = sum(avgPaid)) %>%
    mutate(avgfee = mean(avgfee)) %>%
    mutate(avglengthStay = mean(avglengthStay)) %>%
    mutate(avgdateDiff = mean(avgdateDiff)) %>%
    distinct(month, .keep_all=TRUE)
  
  nameM$month <- as.yearmon(nameM$month, "%Y-%m")
  nameM$month <- as.Date(nameM$month)
  
  fwrite(nameM, paste0("campground_monthly_", ID, ".csv"))
}

