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
require(ggridges)
require(dplyr)

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


d = data.frame(cbind(c(FreqTableX$Breaks, FreqTableY$Breaks,FreqTableZ$Breaks),c(FreqTableX$mean,FreqTableY$mean,FreqTableZ$mean),
                     c(rep(paste(names(group1)[1],names(group1)[length(group1)]   , sep=" - "),37),
                       rep(paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - "),37),
                       rep(paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - "),37))))
colnames(d) <- c("breaks", "freq", "Period")
d$br = as.numeric(as.character(d$breaks))
d$breaks = as.factor(as.numeric(as.character(d$breaks)))
d$freq = as.numeric(as.character(d$freq))
d = d[order(d[,4]),]
d$Period = factor(d$Period, levels = c(paste(names(group1)[1],names(group1)[length(group1)]   , sep=" - "),
                                       paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - "),
                                       paste(names(group3)[1],names(group3)[length(group3)]   , sep=" - ")))



mu = data.frame(Period = c('A','B','C'), mean= rep(NA,3))
mu[1,2] = sum(FreqTableX$Breaks * FreqTableX$mean)/sum(FreqTableX$mean)
mu[2,2] = sum(FreqTableY$Breaks * FreqTableY$mean)/sum(FreqTableY$mean)
mu[3,2] = sum(FreqTableZ$Breaks * FreqTableZ$mean)/sum(FreqTableZ$mean)





d = data.frame(cbind(c(FreqTableX$Breaks, FreqTableY$Breaks,FreqTableZ$Breaks),c(FreqTableX$mean,FreqTableY$mean,FreqTableZ$mean),
                     c(rep('A',37),
                       rep('B',37),
                       rep('C',37))))
colnames(d) <- c("breaks", "freq", "Period")
d$br = as.numeric(as.character(d$breaks))
d$breaks = as.factor(as.numeric(as.character(d$breaks)))
d$freq = as.numeric(as.character(d$freq))
d = d[order(d[,4]),]
d$Period = factor(d$Period, levels = c(paste(names(group1)[1],names(group1)[length(group1)]   , sep=" - "),
                                       paste(names(group2)[1],names(group2)[length(group2)]   , sep=" - "),
                                       paste(names(group3)[1],names(group3)[length(group3)]   , sep=" - ")))

#SMCplot =
windows(width = 12, height = 12)
  ggplot(d, aes(x=breaks, y=freq, fill=Period)) +
  theme_bw(base_size = 13)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_bar(stat='identity',position="dodge", width = 0.5)+
  scale_fill_manual(values=c(col1, col2, col3))+
  scale_x_discrete(breaks = unique(d$br), labels=seq(10,370, by=10))+
  labs(x="Age at settlement (days)",y="Percentage")


  windows(width = 12, height = 12)
  ggplot(d, aes(x=breaks, y=freq, fill=Period)) +
    theme_bw(base_size = 13)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_bar(stat='identity',position="dodge", width = 0.5)+
    scale_fill_manual(values=c(col1, col2, col3))+
    scale_x_discrete(breaks = unique(d$br), labels=seq(10,370, by=10))+
    labs(x="Age at settlement (days)",y="Percentage")


   ggplot(d, aes(x = br, y=freq, fill=Period))+
    geom_density(aes(y=freq, fill = Period), alpha = 0.4) +
    geom_vline(aes(xintercept = mean, color = Period),
               data = mu, linetype = "dashed") +
    scale_color_manual(values = c(col1, col2, col3))+
    scale_fill_manual(values = c(col1, col2, col3))


   a <- ggplot(wdata, aes(x = weight))
   a + geom_density(aes(fill = sex), alpha = 0.4) +
    # geom_vline(aes(xintercept = grp.mean, color = sex),
              #  data = mu, linetype = "dashed") +
     scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
     scale_fill_manual(values = c("#868686FF", "#EFC000FF"))


  theme_set(theme_ridges())
  windows(width = 12, height = 12)
  ggplot(d, aes(x=freq, y=Period)) +
    #theme_bw(base_size = 13)+
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_density_ridges()+
    scale_fill_manual(values=c(col1, col2, col3))
    #scale_x_discrete(breaks = unique(d$br), labels=seq(10,370, by=10))+
    #labs(x="Age at settlement (days)",y="Percentage")



  ggplot()+
    geom_area(aes(y=freq, x=br, fill=Period), data=d, stat="identity")
















  # create factors with value labels
  mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                        labels=c("3gears","4gears","5gears"))
  mtcars$am <- factor(mtcars$am,levels=c(0,1),
                      labels=c("Automatic","Manual"))
  mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                       labels=c("4cyl","6cyl","8cyl"))




  qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5),
        main="Distribution of Gas Milage", xlab="Miles Per Gallon",
        ylab="Density")








  return(SMCplot)



}
