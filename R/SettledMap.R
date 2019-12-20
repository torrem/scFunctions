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
  data(ConGrid1)

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

    if (length(ConGrid1@polygons[[i]]@Polygons) ==2) {
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = x

      x = ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1] = x
    }
  }

  ## Find settlers ##
  settlers = dfrs[[4]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used



  ## plot starters and settlers

  png(paste(path,"/SettleMap_",names(group)[kk],".png",sep=""), width = 12, height = 12, units = "in", res = 200)

  getBeringMap(openWindow=FALSE)
  points(settlers$horizPos2~settlers$horizPos1, cex=0.8, col=cl, pch=15)
  raster::plot(ConGrid1,add=TRUE)
  lab = 1:27
  dd = c(200.05070, 196.80137, 193.84557, 192.25913, 191.10922, 197.80395, 195.25373, 191.68229, 189.12109, 187.36041, 195.03239, 193.23246, 189.45572, 186.0, 184.07891, 184.20322, 180.17429, 180.1170, 174.95373, 191.13708, 186.01163, 178.9745, 174.44257 , 182.2094, 177.50780, 174.65190, 194.98104)
  gg = c(57.50158,  58.07277,  59.20534,  60.28851,  61.68657,  56.14115,  56.82816,  57.87291,  59.05969,  60.88825,  54.95754,  55.63593,  56.56982,  58.0,  59.63626,  62.82903,  64.07788,  61.3,  61.52478,  54.65115,  56.19912,  60.0044,  60.83757,   55.0024,  57.65041,  59.49971,  53.94815)
  maptools::pointLabel(dd, gg,labels=paste(lab), cex=1.5, col="black")
  dev.off()
  print(paste("making settled map for: ",names(group[kk])), sep="")


}

}


