# Map number of successful individuals per meter in each connectivity region
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that
#'shows number of successful individuals per meter in each connectivity region
#'
#'@description Function to produce map of number of successful individuals per meter in each connectivity region
#'
#'@return map of number of successful individuals per meter in each connectivity region
#'
#'@export
#'
#'






SettlePerM2 <-function(group, path){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels



  SIZ = data.frame(Region = 1:18)

  for (kk in 1:length(group)){
  resdr = group[kk]
  load(paste(resdr,"/dfrs.RData",sep=""))

  data(ConGridFinal)
  ## convert coordinates to work with map ##
  for (i in c(1,4)){
    # print(paste("Convertving Coordinates for", typeNames[i]))
    #dfrs[[]]
    for (kkk in 1:nrow(dfrs[[i]])){
      dfrs[[i]][kkk,"horizPos1"]  = ifelse(dfrs[[i]][kkk,"horizPos1"] > 0,dfrs[[i]][kkk,"horizPos1"], 360-abs(dfrs[[i]][kkk,"horizPos1"]))
    }

  }

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








  ## Find starters and settlers ##

  settlers = dfrs[[4]]
  #settlers = settlers[!duplicated(settlers$origID), ]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used



  ## Connectivity Matrix ##



  ## Pull out Settlers (C1M & C1F) from dfrs
  settlers = as.data.frame(settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),])
  sp::coordinates(settlers)=~horizPos1 + horizPos2
  sp::proj4string(settlers) <- sp::proj4string(ConGrid1)
  s = sp::over(settlers, ConGrid1); settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid


  RegionCode = data.frame(MapRegion = ConGrid1$OBJECTID, MapOrder = 1:18 )

  ##change congrid1 region code to connectivity zone code on my map ##
  # for (i in 1:15){
  #   if (s[i,2]==999){next}
  #   s[i,2] = RegionCode[which(RegionCode[,2]==s[i,2]),1]
  #
  # }





  SettleInZone = data.frame(Region = 1:18, NumSettlers = rep(NA, 18))





  # ZoneArea = c(45467923122.099998,48733983826.099998,41876641917.599998,29150756998.799999,50041280842.699997,28156982144.700001,
  #              48463658481.400002,32175199970.5,     43058333151.599998,51965675314.099998,4310824373.58,
  #              36272213040.599998,43645241013.099998,31182972193.700001,46377460312.400002,73897745180.199997,
  #              65750246504.099998,67469098918.199997,24266122353.799999,67796246840.199997,28853274483.599998,
  #              42299337357.599998,22181812802.799999,261250592356,156851996233,104870483607,29208450343.400002)



  ZoneArea = c(35911676627.099998, 39408149960.300003, 46956045354.300003, 41094685962.199997, 34211330674.599998,
               45881540952.5, 22208137040.200001, 57357157291.800003, 136700470037, 1293071211090, 103302772116, 68298378731.5,
               54178280831.699997, 48773932053.699997, 62250352285.099998, 129268973061, 49039954043.099998, 44736019789.099998)






  SettleInZone = cbind(SettleInZone,ZoneArea)


 # SettleInZone$Region = RegionCode$MapRegion

 # SettleInZone$R2 = RegionCode$MapRegion
  #SettleInZone <- SettleInZone[order(SettleInZone$R2),]

    for (i in 1:18){
      x = nrow(subset(settlers, OBJECTID==i))
      SettleInZone[i,2] = x
    }



  SettleInZone$Density = SettleInZone$NumSettlers/SettleInZone$ZoneArea

  ##add density to SIZ
  SIZ$r = SettleInZone$Density;
  colnames(SIZ)[kk+1] = paste(names(group[kk]))
  }

  ### take mean of SIZ
  SIZ$Mean = apply(SIZ[2:length(SIZ)],1,mean)

  SettleInZone$Density = SIZ$Mean

  SettleInZone$ColorCode = ifelse(round((SettleInZone$Density/max(SettleInZone$Density))*100)>100,
                                  100,round((SettleInZone$Density/max(SettleInZone$Density))*100) )



   ## plot starters and settlers


  #create map




  ## hindcast names##
  if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/SettlePerM2Map",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }

  ## forecast names##
  if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/SettlePerM2Map",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][3],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }









getBeringMap(openWindow=FALSE,addGrid=FALSE,addDepth=FALSE)
 # mypal <- colorRampPalette(c("blue" ,"red","yellow"), bias=1)
 cc = rev(RColorBrewer::brewer.pal(9,"RdYlBu"))
 mypal <- colorRampPalette(c(cc[1], cc[2], cc[3], cc[4], cc[5], cc[6], cc[7], cc[8], cc[9]))
 #mypal <- colorRampPalette(c("blue", "lightblue1", "lightpink", "red"))
 for (i in 1:18){

    #ii = which(SettleInZone[,4]==i)
    c = SettleInZone$ColorCode[i]
    if(c<1){c=c+1}
    raster::plot(ConGrid1[i,], col=mypal(100)[c], add=TRUE)
    #raster::plot(ConGrid1[i,], col=cc[3], add=TRUE)
 }


  lgd_ = rep(NA, 9)

  #maxden = formatC(max(SettleInZone$Density), format = "e", digits=2 )

  lgd_[c(1,5,9)] = c(formatC(max(SettleInZone$Density), format = "e", digits=2 ),
                     formatC(mean(SettleInZone$Density), format = "e", digits=2 ),
                     0)


  legend(x = "bottomright", y = 1,
         legend = lgd_,
         fill = RColorBrewer::brewer.pal(9,"RdYlBu"),
         border = NA,
         bty="n",
         title="Density (per m^2)
      ",
         y.intersp = 0.5,
         cex = 1.5, text.font = 2)


  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")

  #return(SettleInZone[,1:4])
dev.off()

}








