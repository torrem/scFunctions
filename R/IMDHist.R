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


IMDHist <-function(group, mintemp=2 ,path){

FreqTable = data.frame(year=numeric(), typeName=character(), origID=numeric(),ageInStage=numeric(), TempInStage=numeric())


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

  M1 = subset(M, typeName =="M1")
  M2 = subset(M, typeName =="M2")

  Z1$temperature = as.numeric(Z1$temperature);Z1$ageInStage = as.numeric(Z1$ageInStage)
  Z2$temperature = as.numeric(Z2$temperature);Z2$ageInStage = as.numeric(Z2$ageInStage)
  M1$temperature = as.numeric(M1$temperature);M1$ageInStage = as.numeric(M1$ageInStage)
  M2$temperature = as.numeric(M2$temperature);M2$ageInStage = as.numeric(M2$ageInStage)


  M1Con = M1[order(M1$origID)[!duplicated(sort(M1$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  M1Con$temp = 999

    #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  finalMatrix <- foreach(i=1:nrow(M1Con), .combine=rbind) %dopar% {
    tempMatrix = M1[which(M1$origID == M1Con[i,]$origID   ),]$temperature #calling a function
    #do other things if you want

    M1Con[i,]$temp = median(tempMatrix) #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
  }
  #stop cluster
  stopCluster(cl)

  M1Con$temp = finalMatrix[,1]


  M2Con = M2[order(M2$origID)[!duplicated(sort(M2$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  M2Con$temp = 999

  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  finalMatrix <- foreach(i=1:nrow(M2Con), .combine=rbind) %dopar% {
    tempMatrix = M2[which(M2$origID == M2Con[i,]$origID   ),]$temperature #calling a function
    #do other things if you want

    M2Con[i,]$temp = median(tempMatrix) #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
  }
  #stop cluster
  stopCluster(cl)

  M2Con$temp = finalMatrix[,1]


  Z2Con = Z2[order(Z2$origID)[!duplicated(sort(Z2$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  Z2Con$temp = 999



  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  finalMatrix <- foreach(i=1:nrow(Z2Con), .combine=rbind) %dopar% {
    tempMatrix = Z2[which(Z2$origID == Z2Con[i,]$origID   ),]$temperature #calling a function
    #do other things if you want

    Z2Con[i,]$temp = median(tempMatrix) #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
  }
  #stop cluster
  stopCluster(cl)

  Z2Con$temp = finalMatrix[,1]




  Z1Con = Z1[order(Z1$origID)[!duplicated(sort(Z1$origID),fromLast=TRUE)],]
  #MCon2 = dfrs[[2]];  MCon2 = subset(MCon2, ageInStage>0)
  Z1Con$temp = 999


  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  finalMatrix <- foreach(i=1:nrow(Z1Con), .combine=rbind) %dopar% {
    tempMatrix = Z1[which(Z1$origID == Z1Con[i,]$origID   ),]$temperature #calling a function
    #do other things if you want

    Z1Con[i,]$temp = median(tempMatrix) #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
  }
  #stop cluster
  stopCluster(cl)

  Z1Con$temp = finalMatrix[,1]






  Z1C = cbind(substr(Z1Con$startTime, 1, 4), Z1Con$typeName,  Z1Con$origID, Z1Con$ageInStage, Z1Con$temperature)
  Z2C = cbind(substr(Z2Con$startTime, 1, 4), Z2Con$typeName,  Z2Con$origID, Z2Con$ageInStage, Z2Con$temperature)
  M1C = cbind(substr(M1Con$startTime, 1, 4), M1Con$typeName,  M1Con$origID, M1Con$ageInStage, M1Con$temperature)
  M2C = cbind(substr(M2Con$startTime, 1, 4), M2Con$typeName,  M2Con$origID, M2Con$ageInStage, M2Con$temperature)

  FreqTable = rbind(FreqTable,Z1C,Z2C,M1C,M2C)





  print(paste("calculating IMD & Temp for: ",names(group[kk])), sep="")

}





ff1 = FreqTable

ff = FreqTable

FreqTable = ff

FreqTable = as.data.frame(FreqTable)
colnames(FreqTable)[1:4] = c("year", 'typeName', 'ageInStage', 'temp' )


## save FreqTable as R file ##

save(FreqTable, file=paste0(path,"/FreqTable_IMDHist_",
                            strsplit(names(group[1]), '_')[[1]][2], "_",
                            strsplit(names(group[length(group)]), '_')[[1]][2],
                            ".RData"))

# load(paste0(path,"/FreqTable_IMDHist_",
#      strsplit(names(group[1]), '_')[[1]][2], "_",
#      strsplit(names(group[length(group)]), '_')[[1]][2],
#     ".RData"))


FreqTable$typeName =  as.factor(FreqTable$typeName)
levels(FreqTable$typeName) <- c("Z1", "Z2", "M1", "M2")

FreqTable$ageInStage = as.numeric(as.character(FreqTable$ageInStage))
FreqTable$temp = as.numeric(as.character(FreqTable$temp))

#FreqTable = subset(FreqTable, temp>3)

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








#
# library(MASS)
#
# # Default call
# k <- kde2d(FreqTable$ageInStage ~ FreqTable$temp, n=200)
# image(k)
#
#
#
#
# rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
# r <- rf(32)


#  ## if there are mostly 0 values for ageinstage ##
# fff = subset(FreqTable, typeName=="M2")
#
# fff$ageInStage = ifelse(fff$ageInStage<=1,runif(length(which(fff$ageInStage<=1))),fff$ageInStage)

FreqTable$ageInStage = ifelse(FreqTable$ageInStage<=1,runif(length(which(FreqTable$ageInStage<=1))),FreqTable$ageInStage)



library(ggplot2)

# Default call (as object)
p <- ggplot(FreqTable, aes(ageInStage,temp))


# ## raster
 p + stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE)+
   facet_grid(. ~ typeName) + scale_fill_viridis_c(option = "plasma")

 # # ## raster
 # p + stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE)+
 #   facet_grid(. ~ typeName) + scale_fill_viridis_c(limits = c(0.001, .01),option = "plasma", na.value= "white")



## contour
 p + stat_density_2d(aes(fill = stat(nlevel)), n = 150,  geom = "polygon")+
   facet_grid(. ~ typeName) + scale_fill_viridis_c(option = "plasma")


# function(x)
# {
#   r <- quantile(x, c(0.25, 0.75))
#   h <- (r[2] - r[1])/1.34
#   4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
# }

#
# g = lattice::histogram(~ ageInStage | typeName, data = FreqTable,
#                    layout = c(3, 1), scales=list(tck=c(1,0), alternating=FALSE))
#
# p =  lattice::densityplot(~temp|typeName, data=FreqTable, plot.points=FALSE,
#                       layout = c(3, 1), strip=FALSE,scales=list(tck=c(1,0), alternating=FALSE))
#
#
# gridExtra::grid.arrange(g, p, nrow = 2)





dev.off()



}









