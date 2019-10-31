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

SettleMonthC <-function(group1, group2, group3,
                        g1name ="Hindcast", g2name = "RCP4.5", g3name = "RCP8.5",
                        col1='dimgrey', col2='#0073C2FF', col3='coral'){

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
  FreqTableX$counts = (FreqTableX$counts/sum(FreqTableX$counts)) * 100
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
  FreqTableY$counts = (FreqTableY$counts/sum(FreqTableY$counts)) * 100
  colnames(FreqTableY)[i+1] = paste(names(group2)[i])
}
FreqTableY$mean = apply(FreqTableY[,c(seq(2,length(group2)+1,by=1))],1, mean)


FreqTableZ = data.frame(Breaks = seq(1,12,by=1))

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
  FreqTableZ$counts = counts
  FreqTableZ$counts = (FreqTableZ$counts/sum(FreqTableZ$counts)) * 100
  colnames(FreqTableZ)[i+1] = paste(names(group2)[i])
}
FreqTableZ$mean = apply(FreqTableZ[,c(seq(2,length(group2)+1,by=1))],1, mean)


##### Freq plot

d = data.frame(cbind(c(FreqTableX$Breaks, FreqTableY$Breaks,FreqTableZ$Breaks),
                     c(FreqTableX$mean,FreqTableY$mean,FreqTableZ$mean),
                     c(rep(g1name,12),
                       rep(g2name,12),
                       rep(g3name,12))))
colnames(d) <- c("breaks", "freq", "Period")
d$br = as.numeric(as.character(d$breaks))
d$breaks = as.factor(as.numeric(as.character(d$breaks)))
d$freq = as.numeric(as.character(d$freq))
d = d[order(d[,4]),]
d$Period = factor(d$Period, levels = c(g1name,
                                       g2name,
                                       g3name))

SMCplot =  ggplot(d, aes(x=breaks, y=freq, fill=Period)) +
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(stat='identity',position="dodge", width = 0.5)+
  scale_fill_manual(values=c(col1, col2, col3))+
  scale_x_discrete(breaks = unique(d$br), labels=c(1:12))+
  labs(x="Settle Month",y="Frequency")

return(SMCplot)

}



