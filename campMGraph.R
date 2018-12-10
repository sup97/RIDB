campMGraph <- function(data, dataM){
  dataM$month <- as.Date(dataM$month)
  ggplot(dataM, aes(x=month, y=occupancy, group=1)) +
   geom_line(size = 1) +
       labs(x="", y="Monthly Average Occupancy (%)") + 
    theme(axis.title.y = element_text(size = 9)) + 
    theme(axis.title.x = element_text(size = 9))  +
    theme(panel.background = element_blank())   + 
    scale_y_continuous(breaks=seq(0,100,25))
}


ggplot(meadowsM, aes(x=month, y=occupancy, group=1)) +
  geom_line(size = 1) +
  labs(x="", y="Monthly Average Occupancy (%)") + 
  theme(axis.title.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 9))  +
  theme(panel.background = element_blank())   + 
  scale_y_continuous(breaks=seq(0,100,25))
