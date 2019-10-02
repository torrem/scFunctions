library(maptools)
library(numbers)

setwd("C:/Users/Mike/Documents/Snow Crab/Test Run 3")


load("C:/Users/Mike/Documents/Snow Crab/Shapefiles/AKMap.RData")


d = read.csv('Resilts Test1ConRes.csv')

d$h1N = ifelse(d$horizPos1 > 0,d$horizPos1, 360-abs(d$horizPos1) )


d$starter = ifelse(as.character(d$startTime) == as.character(d$time), 1,0)

starters = subset(d, starter==1)

#coordinates(starters) =~ horizPos1 + horizPos2






points(starters$horizPos2~starters$h1N, cex=0.5)


polygon(AK, col='grey')
points(starters$horizPos2 ~starters$horizPos1)
points(mean(starters$horizPos2)~mean(starters$h1N), col="blue", pch=16, cex=1 )

starters$horizPos1 = -starters$horizPos1


settled = subset(d, typeName %in% c('C1F','C1M'))


plot(starters$horizPos2 ~starters$horizPos1)
points(settled$horizPos2~settled$h1N, col='green', pch=17, cex=1)






