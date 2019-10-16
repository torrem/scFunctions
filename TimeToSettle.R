# Centroid and spread of settled individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce frequency plot that shows time of settle
#'
#'@description Function to produce produce frequency plot that shows time of settle
#'
#'@return frequency plot that shows time of settle
#'
#'
require(ggplot2)

TR2000 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2000"
TR2001 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2001"
TR2002 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2002"
TR2003 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2003"

TR2041 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2041"
TR2042 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2042"
TR2043 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2043"
TR2044 = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Batch Run/2044"


x = list(TR2000 = TR2000, TR2001 = TR2001, TR2002 = TR2002, TR2003 = TR2003)
y = list(TR2041 = TR2041, TR2042 = TR2042, TR2043 = TR2043, TR2044 = TR2044)

FreqTableX = data.frame(Breaks = seq(1,12,by=1))

for (i in 1:length(x)){
  resdr = x[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],]

  settlers$month = substr(settlers$time, start = 6, stop = 7)
  settlers$month = as.numeric(settlers$month)

  hist = hist(settlers$month, breaks = seq(0,12, by=1), plot=FALSE)
  counts = hist$counts
  FreqTableX$counts = counts
  colnames(FreqTableX)[i+1] = paste(names(x)[i])
}
FreqTableX$mean = apply(FreqTableX[,c(seq(2,length(x)+1,by=1))],1, mean)

##
FreqTableY = data.frame(Breaks = seq(1,12,by=1))

for (i in 1:length(y)){
  resdr = y[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],]

  settlers$month = substr(settlers$time, start = 6, stop = 7)
  settlers$month = as.numeric(settlers$month)

  hist = hist(settlers$month, breaks = seq(0,12, by=1), plot=FALSE)
  counts = hist$counts
  FreqTableY$counts = counts
  colnames(FreqTableY)[i+1] = paste(names(x)[i])
}
FreqTableY$mean = apply(FreqTableY[,c(seq(2,length(x)+1,by=1))],1, mean)


##### Freq plot

d = data.frame(cbind(c(FreqTableX$Breaks, FreqTableY$Breaks),c(FreqTableX$mean,FreqTableY$mean),
                     c(rep('X',12), rep('Y',12))))
colnames(d) <- c("breaks", "freq", "group")
d$br = as.numeric(as.character(d$breaks))
d$breaks = as.factor(as.numeric(as.character(d$breaks)))
d$freq = as.numeric(as.character(d$freq))
d = d[order(d[,4]),]

ggplot(d, aes(x=breaks, y=freq, fill=group)) +
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat='identity',position="dodge", width = 0.5)+
  scale_fill_manual(values=c('blue', 'red'))+
  scale_x_discrete(breaks = unique(d$br), labels=c(1:12))+
  labs(x="Settle Month",y="Frequency")






