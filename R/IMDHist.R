# Centroid and spread of settled individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce histogram of IMD for each stage
#'
#'@description Function to produce produce frequency plot that shows time of settle
#'
#'@return frequency plot that shows time of settle
#'
#'@export
#'
#'
#'


IMDHist <-function(group, path){

FreqTable = data.frame(year=numeric(), typeName=character(), ageInStage=numeric(), TempInStage=numeric() )


for (kk in 1:length(group)){

  Aresd = group[kk]
  Pth =  Aresd[[1]][1]

  ## make A list##

  #C1 = data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale.csv",sep="/"))
  M =  data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.Megalopa.Megalopa.csv",sep="/"))
  Z =  data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.Zooea.Zooea.csv",sep="/"))



  names(M) <- as.character(unlist(M[1,])); M=M[-1,]
  names(Z) <- as.character(unlist(Z[1,])); Z=Z[-1,]


  Z1 = subset(Z, typeName =="Z1")
  Z2 = subset(Z, typeName =="Z2")


  Z1$temperature = as.numeric(Z1$temperature);Z1$ageInStage = as.numeric(Z1$ageInStage)
  Z2$temperature = as.numeric(Z2$temperature);Z2$ageInStage = as.numeric(Z2$ageInStage)
  M$temperature = as.numeric(M$temperature);M$ageInStage = as.numeric(M$ageInStage)








  MCon = M[order(M$origID)[!duplicated(sort(M$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  MCon$temp = 999

for (i in 1:nrow(MCon)){

  temp2 =  M[which(M$origID == MCon[i,]$origID   ),]$temperature

  MCon[i,]$temp = median(temp2)

}


  Z2Con = Z2[order(Z2$origID)[!duplicated(sort(Z2$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  Z2Con$temp = 999

  for (i in 1:nrow(Z2Con)){

    temp2 =  Z2[which(Z2$origID == Z2Con[i,]$origID   ),]$temperature

    Z2Con[i,]$temp = median(temp2)

  }

  Z1Con = Z1[order(Z1$origID)[!duplicated(sort(Z1$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  Z1Con$temp = 999
  for (i in 1:nrow(Z1Con)){

    temp2 =  Z1[which(Z1$origID == Z1Con[i,]$origID   ),]$temperature

    Z1Con[i,]$temp = median(temp2)

  }






  Z1C = cbind(substr(Z1Con$startTime, 1, 4), Z1Con$typeName, Z1Con$ageInStage, Z1Con$temperature)
  Z2C = cbind(substr(Z2Con$startTime, 1, 4), Z2Con$typeName, Z2Con$ageInStage, Z2Con$temperature)
  MC = cbind(substr(MCon$startTime, 1, 4), MCon$typeName, MCon$ageInStage, MCon$temperature)

  FreqTable = rbind(FreqTable,Z1C,Z2C,MC)





  print(paste("calculating IMD & Temp for: ",names(group[kk])), sep="")

}

ff = FreqTable

FreqTable = as.data.frame(FreqTable)
colnames(FreqTable)[1:4] = c("year", 'typeName', 'ageInStage', 'temp' )






FreqTable$typeName =  as.factor(FreqTable$typeName)
levels(FreqTable$typeName) <- c("Z1", "Z2", "M")

FreqTable$ageInStage = as.numeric(as.character(FreqTable$ageInStage))
FreqTable$temp = as.numeric(as.character(FreqTable$temp))


if(length(strsplit(names(group[1]), '_')[[1]])==2){png( paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep=""),
                                                               width = 12, height = 8, units = "in", res = 300)

}

## forecast names##
if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"_",
                                                               strsplit(names(group[1]),'_')[[1]][3],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep=""),
                                                               width = 12, height = 8, units = "in", res = 300)

}







g = lattice::histogram(~ ageInStage | typeName, data = FreqTable,
                   layout = c(3, 1), scales=list(tck=c(1,0), alternating=FALSE))

p =  lattice::densityplot(~temp|typeName, data=FreqTable, plot.points=FALSE,
                      layout = c(3, 1), strip=FALSE,scales=list(tck=c(1,0), alternating=FALSE))


gridExtra::grid.arrange(g, p, nrow = 2)





dev.off()



}









