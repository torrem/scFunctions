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




  coords = data.frame(Year=vector(),Type=vector(),ID=vector(),x=vector(), y=vector())

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

    ddM = which(is.element(dfrs[[2]]$"Original ID", dfrs[[3]]$"Original ID")==TRUE)

    NSM = dfrs[[2]][-ddM,]
    NSM = NSM[order(NSM$'Original ID')[!duplicated(sort(NSM$'Original ID'),fromLast=TRUE)],]


    ## Find unsuccessfull Z2 ##
    NSZ2 = dfrs[[1]]; NSZ2 = subset(NSZ2, NSZ2$"Life stage type name"=="Z2")

    ddZ2 = which(is.element(NSZ2$"Original ID", dfrs[[2]]$`Original ID`)==TRUE)

    NSZ2 = NSZ2[-ddZ2,]
    NSZ2 = NSZ2[order(NSZ2$'Original ID')[!duplicated(sort(NSZ2$'Original ID'),fromLast=TRUE)],]

    ## Find unsuccessfull Z1 ##
    NSZ1 = dfrs[[1]]; NSZ1 = subset(NSZ1, NSZ1$"Life stage type name"=="Z1")

    TZ2 = dfrs[[1]]; TZ2 = subset(TZ2, TZ2$"Life stage type name"=="Z2")

    ddZ1 = which(is.element(NSZ1$"Original ID", TZ2$`Original ID`) ==TRUE)

    NSZ1 = NSZ1[-ddZ1,]
    NSZ1 = NSZ1[order(NSZ1$'Original ID')[!duplicated(sort(NSZ1$'Original ID'),fromLast=TRUE)],]


    Pdfrs = list( NSZ1=NSZ1, NSZ2=NSZ2,NSM=NSM, settlers=settlers)

    rmList = vector()
    for (i in 1:length(Pdfrs)){

      if (nrow(Pdfrs[[i]]) == 0) {rmList = c(rmList,i)}
    }

    Pdfrs[rmList] = NULL

    AllC1 = dfrs[[3]];AllC1= AllC1[,c(1:6,14,17:18)]
    AllM = dfrs[[2]];AllM= AllM[,c(1:6,14,17:18)]
    AllZ2 = dfrs[[1]]; AllZ2 = subset(AllZ2, AllZ2$"Life stage type name"=="Z2");AllZ2= AllZ2[,c(1:6,14,17:18)]
    AllZ1 = dfrs[[1]]; AllZ1 = subset(AllZ1, AllZ1$"Life stage type name"=="Z1");AllZ1= AllZ1[,c(1:6,14,17:18)]

    alldfrs = rbind( AllZ1, AllZ2,AllM, AllC1)






    #sss = subset(alldfrs[[i]], alldfrs[[i]]$`Original ID`==UNID[jj])

for (j in 1:length(Pdfrs)){

## make table of unique ids
    UNID = Pdfrs[[j]]$`Original ID`
    UNID = sample(UNID,5)
  for(jj in 1:length(UNID)){

    sss = subset(alldfrs, alldfrs$`Original ID`==UNID[jj])
    for (ggg in 1:nrow(sss)){


    a = strsplit(sss[ggg,]$track, ";")
    for (gg in 1:lengths(a)){
      aa = strsplit(a[[1]][gg], ":")
      coords=rbind(coords,
                   c(regmatches(group[kk], regexpr("[:0-2:][0-9][0-9][0-9]", group[kk])),
                   Pdfrs[[j]]$`Life stage type name`[1],
                   sss[1]$`Original ID`, aa[[1]][1], aa[[1]][2]))
      coords = setNames(coords, c('Year','Type',"ID", "x", "y"))
      coords$Year = as.numeric(coords$Year)
      coords$Type = as.character(coords$Type)
      coords$ID = as.numeric(as.character(coords$ID))
      coords$x = as.numeric(as.character(coords$x));coords$y = as.numeric(as.character(coords$y))
      }

}
}
}



    print(paste("compiling trajectories for: ",names(group[kk])), sep="")

  }







    ###### take coords and map trajectories ####_


  # ## convert coordinates to work with map ##
    for (kkk in 1:nrow(coords)){
      coords$x  =
        ifelse(coords$x > 0,coords$x, 360-abs(coords$x))
    }



  CCoords = subset(coords, Type == "C1F")
  MCoords = subset(coords, Type == "M1")
  Z2Coords = subset(coords, Type == "Z2")
  Z1Coords = subset(coords, Type == "Z1")



  Crd = list(CCoords =CCoords, MCoords=MCoords, Z2Coords=Z2Coords, Z1Coords=Z1Coords  )



  rmList2 = vector()
  for (i in 1:length(Crd)){

    if (nrow(Crd[[i]]) == 0) {rmList2 = c(rmList2,i)}
  }

  Crd[rmList2] = NULL







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





  getBeringMap(openWindow=FALSE, addGrid = FALSE,addDepth=FALSE)
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


  raster::plot(ConGrid1,add=TRUE, lwd=2)
  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")

## plot trajectories for each life stage ##
  for (u in 1:length(Crd)){

    cID = unique(Crd[[u]]$ID)

for (uu in 1:length(cID)){
  ghy = subset(Crd[[u]], ID == cID[uu])
    trj <- TrajFromCoords(ghy[,c(4,5)])
  resampled <- TrajRediscretize(trj, 0.0003)

  if(ghy$Type[1] == "C1F"){plot(resampled, lwd = 2,draw.start.pt = FALSE, col= adjustcolor( "blue4", alpha.f = 0.3), add=TRUE)}
  if(ghy$Type[1] == "M1"){plot(resampled, lwd = 2,draw.start.pt = FALSE, col= adjustcolor( "red", alpha.f = 0.3), add=TRUE)}
  if(ghy$Type[1] == "Z2"){plot(resampled, lwd = 2,draw.start.pt = FALSE, col= adjustcolor( "goldenrod2", alpha.f = 0.3), add=TRUE)}
  if(ghy$Type[1] == "Z1"){plot(resampled, lwd = 2,draw.start.pt = FALSE, col= adjustcolor( "green", alpha.f = 0.3), add=TRUE)}

  }
}



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










