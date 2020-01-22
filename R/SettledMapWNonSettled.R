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



SettledMap <-function(group, path, addLegend==TRUE ){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels
  #data(ConGridFinal)

  for (kk in 1:length(group)){
  load(paste(group[kk],"/dfrs.RData",sep=""))

    Aresd = group[kk]
    Pth =  Aresd[[1]][1]
    ## make A list##

    C1 = data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale.csv",sep="/"))
    MF =  data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.Megalopa.Megalopa.csv",sep="/"))
    ZF =  data.table::fread(paste(Pth,"results.disMELS.IBMs.SnowCrab.Zooea.Zooea.csv",sep="/"))

    dfrs = list( ZF=ZF, MF=MF,C1=C1)



    for (i in 1:length(dfrs)){
      dfrs[[i]] =dfrs[[i]][-1,]
      dfrs[[i]]$'Horiz. position 1' = as.numeric(dfrs[[i]]$'Horiz. position 1')
      dfrs[[i]]$`Horiz. position 2` = as.numeric(dfrs[[i]]$`Horiz. position 2`)
     # dfrs[[i]]$ID = as.numeric(dfrs[[i]]$ID)
     # dfrs[[i]]$"Parent ID" = as.numeric(dfrs[[i]]$"Parent ID")
     # dfrs[[i]]$"Original ID" = as.numeric(dfrs[[i]]$"Original ID")


    }





    ## Find settlers ##
    settlers = dfrs[[3]]
    #settlers = settlers[!duplicated(settlers$origID), ]
    settlers = settlers[order(settlers$'Original ID')[!duplicated(sort(settlers$'Original ID'))],] ## makes sure only unique settlers are used


    ## Find unsuccessfull Megalope ##

    ddM = which(is.element(dfrs[[2]]$"Original ID", settlers$"Original ID")==TRUE)

    NSM = dfrs[[2]][-ddM,]
    NSM = NSM[order(NSM$'Original ID')[!duplicated(sort(NSM$'Original ID'),fromLast=TRUE)],]


    ## Find unsuccessfull Z2 ##
    NSZ2 = dfrs[[1]]; NSZ2 = subset(NSZ2, NSZ2$"Life stage type name"=="Z2")

    ddZ2 = which(is.element(NSZ2$"Original ID", dfrs[[2]]$`Original ID`)==TRUE)

    NSZ2 = NSZ2[-ddZ2,]
    NSZ2 = NSZ2[order(NSZ2$'Original ID')[!duplicated(sort(NSZ2$'Original ID'),fromLast=TRUE)],]

    ## Find unsuccessfull Z1 ##
    NSZ1 = dfrs[[1]]; NSZ1 = subset(NSZ1, NSZ1$"Life stage type name"=="Z1")

    TZ2 = subset(dfrs[[1]],dfrs[[1]]$"Life stage type name"=="Z2")

    ddZ1 = which(is.element(NSZ1$"Original ID", TZ2$`Original ID`) ==TRUE)

    NSZ1 = NSZ2[-ddZ1,]
    NSZ1 = NSZ1[order(NSZ1$'Original ID')[!duplicated(sort(NSZ1$'Original ID'),fromLast=TRUE)],]


    Pdfrs = list( NSZ1=NSZ1, NSZ2=NSZ2,NSM=NSM, settlers=settlers )

    rmList = vector()
    for (i in 1:length(Pdfrs)){

      if (nrow(Pdfrs[[i]]) == 0) {rmList = c(rmList,i)}
    }

    Pdfrs[rmList] = NULL

    ## convert coordinates to work with map ##
    for (i in 1:length(Pdfrs)){

      for (kkk in 1:nrow(Pdfrs[[i]])){
        Pdfrs[[i]][kkk,"Horiz. position 1"]  =
          ifelse(Pdfrs[[i]][kkk,"Horiz. position 1"] > 0,Pdfrs[[i]][kkk,"Horiz. position 1"], 360-abs(Pdfrs[[i]][kkk,"Horiz. position 1"]))
      }

    }



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
 # getBeringMap()

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

  for (i in rev(1:length(Pdfrs))){
    if(names(Pdfrs)[i]=="settlers"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='red', pch=15)}
    if(names(Pdfrs)[i]=="NSM"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='green', pch=15)}
    if(names(Pdfrs)[i]=="NSZ2"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='purple', pch=15)}
    if(names(Pdfrs)[i]=="NSZ1"){points(Pdfrs[[i]]$"Horiz. position 2"~Pdfrs[[i]]$"Horiz. position 1", cex=0.8, col='pink', pch=15)}
  }
  raster::plot(ConGrid1,add=TRUE, lwd=2)
  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")

  if(addLegend==TRUE){

    zbreaks = c("Z1" ,"Z2", "M", "C1")



    legend(x = "bottomright", legend = zbreaks, fill = c("pink", "purple", "green", "red") ,
            bg = "white",cex = 1)

  }



  dev.off()
  print(paste("making settled map for: ",names(group[kk])), sep="")


}

}


