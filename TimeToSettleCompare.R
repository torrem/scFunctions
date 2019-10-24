# Centroid and spread of settled individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce frequency plot that shows time of settle
#'
#'@description Function to produce produce frequency plot that shows time of settle
#'
#'@return frequency plot that shows time of settle
#'
#'@export
#'
#'
require(ggplot2)

SettleMonthC <-function(group1, group2, col1="blue", col2="red"){

FreqTableX = data.frame(Breaks = seq(1,12,by=1))

for (i in 1:length(group1)){
  resdr = group1[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$month = substr(settlers$time, start = 6, stop = 7)
  settlers$month = as.numeric(settlers$month)

  hist = hist(settlers$month, breaks = seq(0,12, by=1), plot=FALSE)
  counts = hist$counts
  FreqTableX$counts = counts
  colnames(FreqTableX)[i+1] = paste(names(group1)[i])
}
FreqTableX$mean = apply(FreqTableX[,c(seq(2,length(group1)+1,by=1))],1, mean)

##
FreqTableY = data.frame(Breaks = seq(1,12,by=1))

for (i in 1:length(group2)){
  resdr = group2[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$month = substr(settlers$time, start = 6, stop = 7)
  settlers$month = as.numeric(settlers$month)

  hist = hist(settlers$month, breaks = seq(0,12, by=1), plot=FALSE)
  counts = hist$counts
  FreqTableY$counts = counts
  colnames(FreqTableY)[i+1] = paste(names(group2)[i])
}
FreqTableY$mean = apply(FreqTableY[,c(seq(2,length(group2)+1,by=1))],1, mean)


##### Freq plot

d = data.frame(cbind(c(FreqTableX$Breaks, FreqTableY$Breaks),c(FreqTableX$mean,FreqTableY$mean),
                     c(rep(paste(names(group1)[1],names(group1)[length(group1)]   , sep=" - "),12),
                       rep(paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - "),12))))
colnames(d) <- c("breaks", "freq", "Period")
d$br = as.numeric(as.character(d$breaks))
d$breaks = as.factor(as.numeric(as.character(d$breaks)))
d$freq = as.numeric(as.character(d$freq))
d = d[order(d[,4]),]
d$Period = factor(d$Period, levels = c(paste(names(group1)[1],names(group1)[length(group1)]   , sep=" - "),
                                       paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - ")))

SMCplot =  ggplot(d, aes(x=breaks, y=freq, fill=Period)) +
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(stat='identity',position="dodge", width = 0.5)+
  scale_fill_manual(values=c(col1, col2))+
  scale_x_discrete(breaks = unique(d$br), labels=c(1:12))+
  labs(x="Settle Month",y="Frequency")

return(SMCplot)

}



