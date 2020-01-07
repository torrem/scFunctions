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


IMDHist <-function(group, path, col='cyan',){

FreqTable = data.frame(typeName=character(), ageInStage=numeric() )

for (kk in 1:length(group)){
  resdr = group[[kk]]
  load(paste(resdr,"/dfrs.RData",sep=""))
  #if (length(dfrs) < 5) next

  Z1 = dfrs[[1]]; Z1 = subset(Z1, typeName=='Z1' & ageInStage>0 )
  Z2 = dfrs[[1]]; Z2 = subset(Z2, typeName=='Z2' & ageInStage>0)
  M =  dfrs[[2]]; M = subset(M,ageInStage>0 )

  FreqTable = rbind(FreqTable,Z1[,c(1,18)],Z2[,c(1,18)],M[,c(1,18)])

}



## hindcast names##
if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep="")
                                                         , width = 12, height = 8, units = "in", res = 200)

}

## forecast names##
if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"_",
                                                               strsplit(names(group[1]),'_')[[1]][3],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep="")
                                                         , width = 12, height = 8, units = "in", res = 200)

}




lattice::histogram(~ ageInStage | typeName, data = FreqTable,
                   layout = c(3, 1), scales=list(alternating=FALSE))




dev.off()



}









