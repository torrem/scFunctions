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

FreqTable = data.frame(year=numeric(), typeName=character(), origID=numeric(),ageInStage=numeric(), TempInStage=numeric())


  for (kk in 1:length(group)){
    resdr = group[kk]
    load(paste(resdr,"/dfrs.RData",sep=""))

    ## Find  Settlers ##
    NSC1 = dfrs[[3]]
    NSC1 = NSC1[order(NSC1$origID)[!duplicated(sort(NSC1$origID))],]

    ## Find  Megalope 2 ##
    NSM2 = dfrs[[2]]; NSM2 = subset(NSM2, NSM2$typeName=="M2")
    NSM2 = NSM2[order(NSM2$origID)[!duplicated(sort(NSM2$origID),fromLast=TRUE)],]


    ## Find  Megalope 1 ##
    NSM1 = dfrs[[2]]; NSM1 = subset(NSM1, NSM1$typeName=="M1")
    NSM1 = NSM1[order(NSM1$origID)[!duplicated(sort(NSM1$origID),fromLast=TRUE)],]


    ## Find  Z2 ##
    NSZ2 = dfrs[[1]]; NSZ2 = subset(NSZ2, NSZ2$typeName=="Z2")
    NSZ2 = NSZ2[order(NSZ2$origID)[!duplicated(sort(NSZ2$origID),fromLast=TRUE)],]


    ## Find  Z2 ##
    NSZ1 = dfrs[[1]]; NSZ1 = subset(NSZ1, NSZ1$typeName=="Z1")
    NSZ1 = NSZ1[order(NSZ1$origID)[!duplicated(sort(NSZ1$origID),fromLast=TRUE)],]


C1C = cbind(substr(NSC1$startTime, 1, 4), NSC1$typeName,  NSC1$origID, NSC1$age, NSC1$temperature)
M2C = cbind(substr(NSM2$startTime, 1, 4), NSM2$typeName,  NSM2$origID, NSM2$ageInStage, NSM2$temperature)
M1C = cbind(substr(NSM1$startTime, 1, 4), NSM1$typeName,  NSM1$origID, NSM1$ageInStage, NSM1$temperature)
Z2C = cbind(substr(NSZ2$startTime, 1, 4), NSZ2$typeName,  NSZ2$origID, NSZ2$ageInStage, NSZ2$temperature)
Z1C = cbind(substr(NSZ1$startTime, 1, 4), NSZ1$typeName,  NSZ1$origID, NSZ1$ageInStage, NSZ1$temperature)

FreqTable =  rbind(FreqTable, Z1C, Z2C , M1C, M2C, C1C )





    print(paste("Calculating IMD & Temp for: ",names(group[kk])), sep="")

  }








FreqTable = as.data.frame(FreqTable)
colnames(FreqTable)[1:5] = c("year", 'typeName', "origID", 'age', 'temp' )



levels(FreqTable$typeName) <- c("Z1", "Z2", "M1", "M2", "C1")



## save FreqTable as R file ##

# save(FreqTable, file=paste0(path,"/FreqTable_IMDHist_",
#                             strsplit(names(group[1]), '_')[[1]][2], "_",
#                             strsplit(names(group[length(group)]), '_')[[1]][2],
#                             ".RData"))

# load(paste0(path,"/FreqTable_IMDHist_",
#      strsplit(names(group[1]), '_')[[1]][2], "_",
#      strsplit(names(group[length(group)]), '_')[[1]][2],
#     ".RData"))





FreqTable$age = as.numeric(as.character(FreqTable$age))
FreqTable$temp = as.numeric(as.character(FreqTable$temp))

#FreqTable = subset(FreqTable, temp>3)

library(ggplot2)

FreqTable$age = ifelse(FreqTable$age<=1,runif(length(which(FreqTable$age<=1))),FreqTable$age)



if(length(strsplit(names(group[1]), '_')[[1]])==2){png( paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep=""),
                                                               width = 16, height = 8, units = "in", res = 300)

}

## forecast names##
if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/IMDHist_",
                                                               ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                               strsplit(names(group[1]),'_')[[1]][1],"_",
                                                               strsplit(names(group[1]),'_')[[1]][2],"_",
                                                               strsplit(names(group[1]),'_')[[1]][3],"-",
                                                               strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep=""),
                                                               width = 16, height = 8, units = "in", res = 300)

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







# Default call (as object)


# # ## raster
#   p + stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE)+
#    facet_grid(. ~ typeName) + scale_fill_viridis_c(option = "plasma")

 # # ## raster
 # p + stat_density_2d(geom = "raster", aes(fill = stat(ndensity)), contour = FALSE)+
 #   facet_grid(. ~ typeName) + scale_fill_viridis_c(limits = c(0.001, .01),option = "plasma", na.value= "white")



# contour'
P = ggplot(FreqTable, aes(age,temp))+
stat_density_2d(aes(fill = stat(nlevel)), n = 150,  geom = "polygon")+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
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



print(P)

dev.off()



}









