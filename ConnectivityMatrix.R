# connectivity between zones

#'
#'@title Take disMELS output file dfrs (from readAllResults) and produces a connectivity matrix
#'
#'@description Function to produce connectivity matrix.
#'
#'@return a matrix showing connectivity between zones

require(RCurl)
require(diagram)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://github.com/torrem/SnowCrabFunctions/raw/master/BeringMap.R");

ConnMatrix <-function(resdr){
  
  #resfn<-"results.";
  #confn<-"resultsConn.csv";
  load(paste(resdr,"/dfrs.RData",sep="")) 
  
  
  
  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels
  
  
  
  
  #create basemap
  
  #dev.new(width=6, height=6, unit="in")
  #windows(width = 12, height = 12);getBeringMap()
  
  
  ## convert coordinates to work with map ##
  for (i in 1:length(typeNames)){
    print(paste("Convertving Coordinates for", typeNames[i]))
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
  
  #con = readr::read_csv(file=paste(resdr,"/",confn, sep=""))
  #con$horizPos1 = ifelse(con$horizPos1 > 0,con$horizPos1, 360-abs(con$horizPos1) )
  
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
  
  ConnectMatrix = matrix(nrow=27, ncol = 27, dimnames=list(c(1:27),c(1:27)))
  
  for (i in 1:27){
    x = subset(settlers, Region==i)
    y = as.data.frame(table(starters[x$origID,]$Region))
    
    #print(nrow(x))
    if (nrow(x) > 0){
      for (k in 1:nrow(y)){
        ConnectMatrix[as.numeric(paste(y[k,1])),i] = y[k,2]
      }
    }
  }
  
  
  ConnectMatrix[is.na(ConnectMatrix)] = 0
  
  for (i in 1:ncol(ConnectMatrix)){
    ConnectMatrix[,i] = round(((ConnectMatrix[,i]/StartInRegion[,2])*100),1)
  }
  
  ConnectMatrix[is.nan(ConnectMatrix)] = 0
  
  return(ConnectMatrix)

}


 


