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



SettledMap <-function(group, path, cl = 'red'){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels
  data(ConGridFinal)

  for (kk in 1:length(group)){
  load(paste(group[kk],"/dfrs.RData",sep=""))

    ## convert coordinates to work with map ##
    for (i in c(1,4)){
      # print(paste("Convertving Coordinates for", typeNames[i]))
      #dfrs[[]]
      for (kkk in 1:nrow(dfrs[[i]])){
        dfrs[[i]][kkk,"horizPos1"]  = ifelse(dfrs[[i]][kkk,"horizPos1"] > 0,dfrs[[i]][kkk,"horizPos1"], 360-abs(dfrs[[i]][kkk,"horizPos1"]))
      }

    }
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

  ## Find settlers ##
     settlers = dfrs[[4]]
    #settlers = settlers[!duplicated(settlers$origID), ]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used


    ## get coords for labels ##
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




  ## plot starters and settlers








    ## hindcast names##
    if(length(strsplit(names(group[1]), '_')[[1]])==2){paste("/SettleMap_",
                                                                   ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                   strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                   strsplit(names(group[kk]),'_')[[1]][2],".png",sep="")

    }

    ## forecast names##
    if(length(strsplit(names(group[1]), '_')[[1]])>2){paste("/SettleMap_",
                                                                   ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                   strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                   strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                   strsplit(names(group[kk]),'_')[[1]][3],".png",sep="")


    }















    ## hindcast names##
    if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/SettleMap_",
                                                                   ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                   strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                   strsplit(names(group[kk]),'_')[[1]][2],".png",sep="")
                                                             , width = 12, height = 12, units = "in", res = 600)

    }

    ## forecast names##
    if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/SettleMap_",
                                                                   ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                   strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                   strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                   strsplit(names(group[kk]),'_')[[1]][3],".png",sep="")
                                                             , width = 12, height = 12, units = "in", res = 600)

    }

  getBeringMap(openWindow=FALSE)
  #getBeringMap()
  points(settlers$horizPos2~settlers$horizPos1, cex=0.8, col=cl, pch=15)
  raster::plot(ConGrid1,add=TRUE)
  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")
  dev.off()
  print(paste("making settled map for: ",names(group[kk])), sep="")


}

}


