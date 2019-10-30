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
require(dplyr)

MaxAge2 <-function(group1, group2, group3,
                  g1name ="Hindcast", g2name = "RCP4.5", g3name = "RCP8.5",
                  col1='dimgrey', col2='#0073C2FF', col3='coral'){

  FreqTableX = vector()

  for (i in 1:length(group1)){
    resdr = group1[i]
    load(paste(resdr,"/dfrs.RData",sep=""))
    #if (length(dfrs) < 5) next
    settlers = dfrs[[5]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

    settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

    FreqTableX = c(FreqTableX,settlers$age_in_days)
    print(paste("reading: ",names(group1[i])), sep="")
  }

FreqTableX = as.data.frame(FreqTableX); FreqTableX$Group = rep(g1name, nrow(FreqTableX));colnames(FreqTableX)[1]="Age"


FreqTableY = vector()

for (i in 1:length(group2)){
  resdr = group2[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

  FreqTableY = c(FreqTableY,settlers$age_in_days)
  print(paste("reading: ",names(group2[i])), sep="")
}

FreqTableY = as.data.frame(FreqTableY); FreqTableY$Group = rep(g2name, nrow(FreqTableY));colnames(FreqTableY)[1]="Age"




FreqTableZ = vector()

for (i in 1:length(group3)){
  resdr = group3[i]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next
  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  settlers$age_in_days<- as.numeric(round(difftime(settlers$time,settlers$startTime , units = c("days"))))

  FreqTableZ = c(FreqTableZ,settlers$age_in_days)
  print(paste("reading: ",names(group3[i])), sep="")
}

FreqTableZ = as.data.frame(FreqTableZ); FreqTableZ$Group = rep(g3name, nrow(FreqTableZ));colnames(FreqTableZ)[1]="Age"



FreqTableXYZ = rbind(FreqTableX,FreqTableY,FreqTableZ)







## Plot the 3 frequency tables



mu <- FreqTableXYZ %>%
  group_by(Group) %>%
  summarise(grp.mean = mean(Age))
mu

#SMCplot <-
windows(width = 12, height = 12)
  ggplot(FreqTableXYZ, aes(x = Age))+
geom_density(aes(fill = Group), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = Group),
             data = mu, linetype = "dashed", lwd=1.5) +
  scale_color_manual(values=c(col1, col2, col3))+
  scale_fill_manual(values=c(col1, col2, col3))



  #return(SMCplot)



}




