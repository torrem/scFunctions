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

CentroidMap <-function(group, conf = 0.5, cl = 'green', alpha=0.3, add=FALSE){


   CMlist <- vector("list", 2)
  XMean = vector("list",2)

  for (kk in 1:length(group)){
    load(paste(group[kk],"/dfrs.RData",sep=""))


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

  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used



  ## plot starters and settlers


  X = as.data.frame(cbind(settlers$horizPos1,settlers$horizPos2))
  X = subset(X, V1 > 182 )
  X = as.matrix(X)

  CMlist[[kk]] = cov(X)
  names(CMlist)[kk] = paste("CM",names(group[kk]), sep="")

  XMean[[kk]] = colMeans(X)
  names(XMean)[kk] = paste("Mean",names(group[kk]), sep="")

  print(paste("calculating centroid spread for: ",names(group[kk])), sep="")
  }

  covX = apply(simplify2array(CMlist), 1:2, mean)
  colmeanX =  apply(simplify2array(XMean), 1:2, mean)
  colmeanX = rowMeans(colmeanX)

  if(add==FALSE){windows(width = 12, height = 12);getBeringMap()}

  return(
  ellipse(center = colmeanX, shape = covX,radius = sqrt(qchisq(conf, df=2)),
          col=cl,fill = TRUE, fill.alpha = alpha,
          center.pch = 17, center.cex = 2)
  )
}


