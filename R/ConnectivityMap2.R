# connectivity between zones
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a connectivity map
#'
#'@description Function to produce connectivity map
#'
#'@return map showing connectivity between zones
#'
#'@export
#'
#'

#require(RCurl)
#require(diagram)

# source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
# source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
# source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");



ConnMap <-function(group, path, connThreshold = 15, connMax = 50, addLegend=TRUE, addDepthLegend=TRUE){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

  data(ConGridFinal)

CMlist <- vector("list", 2)

 for (kk in 1:length(group)){
 #for (kk in 1:3){
  resdr = group[kk]
  load(paste(resdr,"/dfrs.RData",sep=""))
  ## convert coordinates to work with map ##
  for (i in c(1,4)){
   # print(paste("Convertving Coordinates for", typeNames[i]))
    #dfrs[[]]
    for (kkk in 1:nrow(dfrs[[i]])){
    dfrs[[i]][kkk,"horizPos1"]  = ifelse(dfrs[[i]][kkk,"horizPos1"] > 0,dfrs[[i]][kkk,"horizPos1"], 360-abs(dfrs[[i]][kkk,"horizPos1"]))
    }

  }


  # for (i in 1:length(ConGrid1)){
  #
  #   if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
  #     dd = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
  #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
  #     ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = dd
  #   }
  #
  #   if (length(ConGrid1@polygons[[i]]@Polygons) ==2) {
  #     dd = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
  #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
  #     ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = dd
  #
  #     dd = ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1]
  #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
  #     ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1] = dd
  #   }
  # }


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





  ## Find starters and settlers ##
  starters = as.data.frame(dfrs[[1]])

  starters$starter = ifelse(starters$startTime == starters$time, 1,0)

  starters = subset(starters, starter==1)
  #starters = starters[!duplicated(starters$origID), ]






  settlers = dfrs[[4]]
  #settlers = settlers[!duplicated(settlers$origID), ]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used



  ## Connectivity Matrix ##

  ## pull out starters from Z1 in dfrs
  starters = starters[!is.na(starters$horizPos1) & !is.na(starters$horizPos2),]
  sp::coordinates(starters)=~horizPos1 + horizPos2
  sp::proj4string(starters) <- sp::proj4string(ConGrid1)
  s = sp::over(starters, ConGrid1); starters= cbind(data.frame(starters),s) ## match ResultsConn file to connectivity grid

  ## Get rid of starters that are on the fringe of sink regions
  h = table(starters$OBJECTID)
  starters = subset(starters,!(OBJECTID %in% as.numeric(names(which(h < max(h)* 0.16)))))

  ## Pull out Settlers (C1M & C1F) from dfrs
  settlers = as.data.frame(settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),])
  sp::coordinates(settlers)=~horizPos1 + horizPos2
  sp::proj4string(settlers) <- sp::proj4string(ConGrid1)
  s = sp::over(settlers, ConGrid1); settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid

  StartInRegion = data.frame(OBJECTID = 1:18, NumStarters = rep(NA, 18))
  for (i in 1:18){
    d = nrow(subset(starters, OBJECTID==i))
    StartInRegion[i,2] = d
  }

  ConnectMatrix = matrix(nrow=18, ncol = 18, dimnames=list(c(1:18),c(1:18)))

  for (i in 1:18){
    dd = subset(settlers, OBJECTID==i)
    gg = as.data.frame(table(starters[dd$origID,]$OBJECTID))

   # print(nrow(dd))
    if (nrow(dd) > 0){
      for (k in 1:nrow(gg)){
        ConnectMatrix[as.numeric(paste(gg[k,1])),i] = gg[k,2]
      }
    }
  }


  ConnectMatrix[is.na(ConnectMatrix)] = 0

  for (i in 1:ncol(ConnectMatrix)){
    ConnectMatrix[,i] = round(((ConnectMatrix[,i]/StartInRegion[,2])*100),1)
  }

  ConnectMatrix[is.nan(ConnectMatrix)] = 0


  CMlist[[kk]] = ConnectMatrix
names(CMlist)[kk] = paste("CM",names(group[kk]), sep="")

print(paste("calculating connecivity matrix for: ",names(group[kk])), sep="")
   }

ConnectMatrix = apply(simplify2array(CMlist), 1:2, mean)



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


  #getBeringMap()
  #maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")


  #CMap %<a-%{
  ## Label Zones ##
#


png(paste(path,"/ConnectivityMap_",substr(names(group)[kk], 1, 8),".png",sep=""), width = 12, height = 12, units = "in", res = 600)
  getBeringMap(openWindow=FALSE)

 # lab = 1:18
 # dd = c(200.05070, 196.80137, 193.84557, 192.25913, 191.10922, 197.80395, 195.25373, 191.68229, 189.12109, 187.36041, 195.03239, 193.23246, 189.45572, 186.0, 184.07891, 184.20322, 180.17429, 180.1170, 174.95373, 191.13708, 186.01163, 178.9745, 174.44257 , 182.2094, 177.50780, 174.65190, 194.98104)
 # gg = c(57.50158,  58.07277,  59.20534,  60.28851,  61.68657,  56.14115,  56.82816,  57.87291,  59.05969,  60.88825,  54.95754,  55.63593,  56.56982,  58.0,  59.63626,  62.82903,  64.07788,  61.3,  61.52478,  54.65115,  56.19912,  60.0044,  60.83757,   55.0024,  57.65041,  59.49971,  53.94815)
  maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")

  print("Making Connectivity Map...")
 # ArrowCoords$Region = as.numeric( ArrowCoords$Region)
 # ArrowCoords = ArrowCoords[order(ArrowCoords$Region),]

  col <- colorRampPalette(c("Blue", "white","red"), bias=1)
  LW = 1:10

  for (i in 1:ncol(ConnectMatrix)){

    for (k in 1:nrow(ConnectMatrix))

      if(ConnectMatrix[k,i] > connThreshold){
        #print(paste(k,i))


        if (i==k){
          ValCol = ConnectMatrix[k,i]/connMax*100;if(ValCol>100){ValCol=100}
          ValLWD = ConnectMatrix[k,i]/connMax*10;if(ValLWD>10){ValLWD=10}

          diagram::curvedarrow(from = c(ArrowCoords[i,2], ArrowCoords[i,3]), to = c(ArrowCoords[i,2], ArrowCoords[i,3]) + c(0.8,0),
                               curve = -1, arr.pos = 1, arr.type="triangle",
                               arr.col = col(100)[ValCol],
                               lcol=col(100)[ValCol],
                               lwd = LW[ValLWD])
        }

        else{
          ValCol = (ConnectMatrix[k,i]/connMax)*100;if(ValCol>100){ValCol=100}
          ValLWD = (ConnectMatrix[k,i]/connMax)*10;if(ValLWD>10){ValLWD=10}

          arrows(ArrowCoords[k,2], ArrowCoords[k,3], ArrowCoords[i,2], ArrowCoords[i,3],
                 col=col(100)[ValCol],
                 lwd = LW[ValLWD])
          print(paste("k = ",k,";i=",i,
                      ArrowCoords[k,2], ArrowCoords[k,3],
                      ArrowCoords[i,2], ArrowCoords[i,3],
                "ValLWD = ",ValLWD, sep=""))
        }
      }
  }


  if(addLegend==TRUE){
  lgd_ = rep(NA, 5)
  lgd_[c(5,3,1)] = c(connThreshold, (connMax+connThreshold)/2    ,paste(">= ",connMax,sep=""))


  legend(x = "topright", y = 1,
         legend = lgd_,
         lty = 1,
         lwd = c(9, 7, 5, 3, 1),
         col = c(rev(col(5))[1:2],rev(col(10))[6], rev(col(5))[4:5]   ),
         bg = "white",
         title="% Connectivity",
         y.intersp = 0.5,
         cex = 1.2, text.font = 0.7)
  }

  if(addDepthLegend==TRUE){

    zbreaks = c(20,50,100,150,200,round(seq(400, 8000, by=1500)))

    c1 <- colorRampPalette(c("cyan","seagreen1","lightgoldenrod1","tan1"))
    c2 <- colorRampPalette(c("blue3", "dodgerblue1"))

    legend(x = "bottomright", legend = zbreaks, fill = c(rev(c1(5)), rev(c2(6))),
           title = 'depth (m)', bg = "white",cex = 1)

}


dev.off()
 # return(CMap)

}

### need to standardize arrow scale
