campGraph <- function(data, dataM){
  ggplot(data, aes(x=month, group=1)) +
    geom_line(aes(y=occupancy, color="Daily", linetype="Daily")) +
    geom_line(data = dataM, aes(x=month, y=occupancy, color="Monthly", linetype="Monthly"), alpha = 0.5, size = 1.5) +
    scale_color_manual(
      name = "Data", 
      values = c(Daily = "dimgrey", Monthly = "firebrick")) +
    scale_linetype_manual(
      name = "Data", 
      values = c(Daily = "solid", Monthly = "solid")) +
    labs(x="", y="Average Occupancy (%)") + 
    theme(axis.title.y = element_text(size = 9)) + 
    theme(axis.title.x = element_text(size = 9))  +
    theme(panel.background = element_blank())  
}
