# Map settled individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that maps settled individuals
#'
#'@description Function to produce map settled individuals
#'
#'@return map of settled individuals
#'@export
#'
#'



#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
#source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");



SettledMapWNS <-function(group, path, addLegend=TRUE ){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels
  #data(ConGridFinal)


## map stuff ####

  data(ConGridFinal)

  ## Convert coordinates to fit on map ##
  for (i in 1:length(ConGrid1)){

    if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = x
    }

    if (length(ConGrid1@polygons[[i]]@Polygons) > 1) {
      for (gh in 1:length(ConGrid1@polygons[[i]]@Polygons)){
        x = ConGrid1@polygons[[i]]@Polygons[[gh]]@coords[,1]
        x = ifelse(x > 0,x, 360-abs(x) )
        ConGrid1@polygons[[i]]@Polygons[[gh]]@coords[,1] = x
      }

    }
  }


  ArrowCoords = data.frame(Region=ConGrid1@data$OBJECTID,dd = rep(NA, length(ConGrid1@data$OBJECTID)),
                           gg = rep(NA, length(ConGrid1@data$OBJECTID)))

  for (i in 1:length(ConGrid1)){

    if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
      dd = ConGrid1@polygons[[i]]@Polygons[[1]]@labpt
      dd[1] = ifelse(dd[1] > 0,dd[1], 360-abs(dd[1]) )
      ArrowCoords[i,2] = dd[1]
      ArrowCoords[i,3] = dd[2]
    }

    if (length(ConGrid1@polygons[[i]]@Polygons) > 1) {

      polylist = data.frame(poly = 1:length(ConGrid1@polygons[[i]]@Polygons),
                            area = rep(NA,length(ConGrid1@polygons[[i]]@Polygons)),
                            dd = rep(NA,length(ConGrid1@polygons[[i]]@Polygons)),
                            gg = rep(NA,length(ConGrid1@polygons[[i]]@Polygons)))


      for (gh in 1:length(ConGrid1@polygons[[i]]@Polygons)){

        polylist[gh,]$area = ConGrid1@polygons[[i]]@Polygons[[gh]]@area

        polylist[gh,]$dd = ConGrid1@polygons[[i]]@Polygons[[gh]]@labpt[1]
        polylist[gh,]$dd = ifelse(polylist[gh,]$dd > 0,polylist[gh,]$dd, 360-abs(polylist[gh,]$dd))

        polylist[gh,]$gg = ConGrid1@polygons[[i]]@Polygons[[gh]]@labpt[2]

      }

      polylist <- polylist[order(-polylist$area),]

      dd = c(mean(c(polylist[1,]$dd, polylist[2,]$dd)), mean(c(polylist[1,]$gg, polylist[2,]$gg)))
      ArrowCoords[i,2] = dd[1]
      ArrowCoords[i,3] = dd[2]



    }
  }

  ArrowCoords[1,2]=181.5;ArrowCoords[1,3]=60.5
  ArrowCoords[7,2]=188.3;ArrowCoords[7,3]=56.5
  ArrowCoords[12,2]=198.8;ArrowCoords[12,3]=57.4
  ArrowCoords[13,2]=195;ArrowCoords[13,3]=58.7
  ArrowCoords[14,2]=191.3;ArrowCoords[14,3]=59.3
  ArrowCoords[17,2]=190;ArrowCoords[17,3]=66.3






#### filter out unsuccesful individuals ####
  coords = data.frame(Year=vector(),Type=vector(),ID=vector(),x=vector(), y=vector())

  for (kk in 1:length(group)){
    resdr = group[kk]
    load(paste(resdr,"/dfrs.RData",sep=""))


    for (i in c(1:4)){
      # print(paste("Convertving Coordinates for", typeNames[i]))
      #dfrs[[]]
      for (kkk in 1:nrow(dfrs[[i]])){
        dfrs[[i]][kkk,"horizPos1"]  = ifelse(dfrs[[i]][kkk,"horizPos1"] > 0,dfrs[[i]][kkk,"horizPos1"], 360-abs(dfrs[[i]][kkk,"horizPos1"]))
      }

    }



    ## Find settlers ##
    settlers = dfrs[[3]]
    #settlers = settlers[!duplicated(settlers$origID), ]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used
    settlers = subset(settlers, age>=200 | temperature < 0)

    ## Find unsuccessfull Megalope 2 ##
    NSM2 = dfrs[[2]]; NSM2 = subset(NSM2, NSM2$typeName=="M2")
    NSM2 = NSM2[order(NSM2$origID)[!duplicated(sort(NSM2$origID),fromLast=TRUE)],]
    NSM2 = subset(NSM2, ageInStage>=300 )

    ## Find unsuccessfull Megalope 1 ##
    NSM1 = dfrs[[2]]; NSM1 = subset(NSM1, NSM1$typeName=="M1")
    NSM1 = NSM1[order(NSM1$origID)[!duplicated(sort(NSM1$origID),fromLast=TRUE)],]
    NSM1 = subset(NSM1, ageInStage>=100 | temperature < 0)

    ## Find unsuccessfull Z2 ##
    NSZ2 = dfrs[[1]]; NSZ2 = subset(NSZ2, NSZ2$typeName=="Z2")
    NSZ2 = NSZ2[order(NSZ2$origID)[!duplicated(sort(NSZ2$origID),fromLast=TRUE)],]
    NSZ2 = subset(NSZ2, ageInStage>=100 | temperature < 0)

    ## Find unsuccessfull Z2 ##
    NSZ1 = dfrs[[1]]; NSZ1 = subset(NSZ1, NSZ1$typeName=="Z1")
    NSZ1 = NSZ1[order(NSZ1$origID)[!duplicated(sort(NSZ1$origID),fromLast=TRUE)],]
    NSZ1 = subset(NSZ1, ageInStage>=100 | temperature < 0)



    print(paste("compiling unsucessful XY vlaues for: ",names(group[kk])), sep="")

  }














  ## hindcast names##
  if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/SM_",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }

  ## forecast names##
  if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/SM_",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][3],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }





  getBeringMap(openWindow=TRUE, addGrid = FASLE,addDepth=FALSE)
  #getBeringMap(addDepth=FALSE, addGrid = FALSE)


  data(ConGridFinal)

  ## Convert coordinates to fit on map ##
  for (i in 1:length(ConGrid1)){

    if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = x
    }

    if (length(ConGrid1@polygons[[i]]@Polygons) > 1) {
      for (gh in 1:length(ConGrid1@polygons[[i]]@Polygons)){
        x = ConGrid1@polygons[[i]]@Polygons[[gh]]@coords[,1]
        x = ifelse(x > 0,x, 360-abs(x) )
        ConGrid1@polygons[[i]]@Polygons[[gh]]@coords[,1] = x
      }

    }
  }

  ConGrid1 <- maptools::unionSpatialPolygons(ConGrid1, ConGrid1@data$OBJECTID, avoidGEOS=FALSE)




  ## plot xy coords for each life stage ##

  points(settlers$horizPos2~settlers$horizPos1, col="purple")
  points(NSM2$horizPos2~NSM2$horizPos1, col="green")
  points(NSZ1$horizPos2~NSZ1$horizPos1, col="lightblue")
  points(NSM1$horizPos2~NSM1$horizPos1, col="purple", pch=16)
  points(NSZ2$horizPos2~NSZ2$horizPos1, col="blue", pch=16)







  raster::plot(ConGrid1,add=TRUE, lwd=2)
  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")

  if(addLegend==TRUE){

    zbreaks = c(paste("Z1: ",nrow(NSZ1), sep=""),
                paste("Z2: ",nrow(NSZ2), sep=""),
                paste("M: ",nrow(NSM), sep=""),
                paste("C1: ",nrow(settlers), sep=""))



    legend(x = "bottomright", legend = zbreaks, fill = c("green", "purple", "red", "blue"),
           bg = "white",cex = 2)

  }





  dev.off()





}













# ## convert coordinates to work with map ##
# for (i in 1:length(Pdfrs)){
#
#   for (kkk in 1:nrow(Pdfrs[[i]])){
#     Pdfrs[[i]][kkk,"Horiz. position 1"]  =
#       ifelse(Pdfrs[[i]][kkk,"Horiz. position 1"] > 0,Pdfrs[[i]][kkk,"Horiz. position 1"], 360-abs(Pdfrs[[i]][kkk,"Horiz. position 1"]))
#   }
#
# }

# for (kkkk in 1:3){
#   x = Pdfrs[[kkkk]]
#   x=x[,c(1,10,9)]
#
#   Deads = rbind(Deads,x,use.names=FALSE)
# }



## plot starters and settlers





# col=vector()
# for (h in 1:nrow(Deads)){
#   if(Deads[[h,1]]=="Z1"){col = c(col,"pink")}
#   if(Deads[[h,1]]=="Z2"){col = c(col,"purple")}
#   if(Deads[[h,1]]=='M1'){col = c(col,"green")}
# }
#
# Deads = cbind(Deads,col)
#
# # for (i in nrow(1:length(Pdfrs))){
# #   #if(names(Pdfrs)[i]=="settlers"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='red', pch=15)}
# #   points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='green', pch=15)}
# #   if(names(Pdfrs)[i]=="NSZ2"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='purple', pch=15)}
# #   if(names(Pdfrs)[i]=="NSZ1"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='pink', pch=15)}
# # }
# MM = subset(Deads, Life.stage.type.name == "M1")
# ZZ2  = subset(Deads, Life.stage.type.name == "Z2")
# ZZ1 = subset(Deads, Life.stage.type.name == "Z1")
#
# points(MM$"Horiz..position.2"~MM$"Horiz..position.1", cex=0.8, col=Deads$col, pch=15)
# points(ZZ2$"Horiz..position.2"~ZZ2$"Horiz..position.1", cex=0.8, col=Deads$col, pch=15)
# points(ZZ1$"Horiz..position.2"~ZZ1$"Horiz..position.1", cex=0.8, col=Deads$col, pch=15)
#
#
#










