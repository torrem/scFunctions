# Max age of individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce hsitogram of max age
#'
#'@description Function to produce produce hist of max age
#'
#'@return hist that shows max age
#'
#'@export
#'
#'
require(ggplot2)


MaxAge <-function(group1, group2, group3,col1='green', col2='purple', col3='red'){

  FreqTableX = data.frame(Breaks = seq(1,370,by=10))

  for (i in 1:length(group1)){
    resdr = group1[i]
    load(paste(resdr,"/dfrs.RData",sep=""))
    #if (length(dfrs) < 5) next
    settlers = dfrs[[5]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

    settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

    hist = hist(settlers$age_in_days, breaks = seq(0,370, by=10), plot=FALSE)
    counts = hist$counts
    FreqTableX$counts = counts
    FreqTableX$counts = (FreqTableX$counts/sum(FreqTableX$counts)) * 100

    colnames(FreqTableX)[i+1] = paste(names(group1)[i])
  }
  FreqTableX$mean = apply(FreqTableX[,c(seq(2,length(group1)+1,by=1))],1, mean)



FreqTableY = data.frame(Breaks = seq(1,370,by=10))

for (i in 1:length(group2)){
  resdr = group2[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

  hist = hist(settlers$age_in_days, breaks = seq(0,370, by=10), plot=FALSE)
  counts = hist$counts
  FreqTableY$counts = counts
  FreqTableY$counts = (FreqTableY$counts/sum(FreqTableY$counts)) * 100

  colnames(FreqTableY)[i+1] = paste(names(group2)[i])
}
FreqTableY$mean = apply(FreqTableY[,c(seq(2,length(group2)+1,by=1))],1, mean)




FreqTableZ = data.frame(Breaks = seq(1,370,by=10))

for (i in 1:length(group3)){
  resdr = group3[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

  hist = hist(settlers$age_in_days, breaks = seq(0,370, by=10), plot=FALSE)
  counts = hist$counts
  FreqTableZ$counts = counts
  FreqTableZ$counts = (FreqTableZ$counts/sum(FreqTableZ$counts)) * 100

  colnames(FreqTableZ)[i+1] = paste(names(group3)[i])
}
FreqTableZ$mean = apply(FreqTableZ[,c(seq(2,length(group3)+1,by=1))],1, mean)




## Plot the 3 frequency tables


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
