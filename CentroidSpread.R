# Centroid and spread of settled individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that calculates centroid and spread of settled individuals
#'
#'@description Function to produce centroid and spread of settled individuals
#'
#'@return map showing centroid and spread of settled individuals
#'@export

require(RCurl)
require(diagram)
require(car)
require(raster)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

SettledMap <-function(resdr, conf = 0.5, cl = 'green', alpha=0.3, add=FALSE, addStarters=FALSE,
                      addSettlers=TRUE){

  load(paste(resdr,"/dfrs.RData",sep=""))

  #create basemap
  #dev.new(width=6, height=6, unit="in")
  #windows(width = 12, height = 12);getBeringMap()


  ## convert coordinates to work with map ##
  for (i in 1:length(typeNames)){
   # print(paste("Convertving Coordinates for", typeNames[i]))
    dfrs[[i]][,"horizPos1"]  = ifelse(dfrs[[i]][,"horizPos1"] > 0,dfrs[[i]][,"horizPos1"], 360-abs(dfrs[[i]][,"horizPos1"]))
    ## convert tracks
  }

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

  ## Find starters and settlers ##

  dfrs[[1]]$starter = ifelse(as.character(dfrs[[1]]$startTime) == as.character(dfrs[[1]]$time), 1,0)

  starters = subset(dfrs[[1]], starter==1)
  settlers = dfrs[[5]]
  settlers = settlers[!duplicated(settlers$origID), ]

  ## Connectivity Matrix ##

  ## pull out starters from Z1 in dfrs
  starters = starters[!is.na(starters$horizPos1) & !is.na(starters$horizPos2),]
  coordinates(starters)=~horizPos1 + horizPos2
  s = over(starters, ConGrid1); starters= cbind(data.frame(starters),s) ## match ResultsConn file to connectivity grid

  ## Get rid of starters that are on the fringe of sink regions
  h = table(starters$Region)
  starters = subset(starters,!(Region %in% as.numeric(names(which(h < max(h)* 0.16)))))

  ## Pull out Settlers (C1M & C1F) from dfrs
  settlers = settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),]
  coordinates(settlers)=~horizPos1 + horizPos2
  s = over(settlers, ConGrid1); settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid

  StartInRegion = data.frame(Region = 1:27, NumStarters = rep(NA, 27))
  for (i in 1:27){
    x = nrow(subset(starters, Region==i))
    StartInRegion[i,2] = x
  }

  ## plot starters and settlers
  if(add==FALSE){windows(width = 12, height = 12);getBeringMap()}
  if(addStarters==TRUE){points(starters$horizPos2~starters$horizPos1, cex=1, col="blue", pch=15)}
  if(addSettlers==TRUE){points(settlers$horizPos2~settlers$horizPos1, cex=1, col="yellow", pch=15)}

  X = as.data.frame(cbind(settlers$horizPos1,settlers$horizPos2))
  X = subset(X, V1 > 182 )
  X = as.matrix(X)


  return(
  ellipse(center = colMeans(X), shape = cov(X),radius = sqrt(qchisq(conf, df=2)),
          col=cl,fill = TRUE, fill.alpha = alpha,
          center.pch = 17, center.cex = 2)
  )
}


