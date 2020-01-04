library(ggplot2)
library(reshape2)


barChart <- function(resultTable, techniques, nbTechs = -1, ymin, ymax, xAxisLabel = "I am the X axis", yAxisLabel = "I am the Y Label"){
  #tr <- t(resultTable)
  if(nbTechs <= 0){
    stop('Please give a positive number of Techniques, nbTechs');
  }
  
  tr <- as.data.frame(resultTable)
  nbTechs <- nbTechs - 1 ; # seq will generate nb+1
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_time
  tr$CI1 <- tr$mean_time - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(seq.int(0, nbTechs, 1));
  
  breaks <- c(as.character(tr$technique));
  print(tr)
  g <- ggplot(tr, aes(x=technique, y=mean_time)) + 
    geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_time-CI1, ymax=mean_time+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +
    #labs(title="Overall time per technique") +
    labs(x = xAxisLabel, y = yAxisLabel) + 
    scale_y_continuous(limits = c(ymin,ymax)) +
    scale_x_discrete(name="",breaks,techniques)+
    coord_flip() +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black")         # dots
  
  print(g)
}