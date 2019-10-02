# connectivity between zones
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a connectivity map
#'
#'@description Function to produce connectivity map
#'
#'@return map showing connectivity between zones

require(RCurl)
require(diagram)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://github.com/torrem/SnowCrabFunctions/raw/master/BeringMap.R");

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

ConnMap <-function(resdr){
  
  load(paste(resdr,"/dfrs.RData",sep="")) 
  
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
    
   # print(nrow(x))
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
  
  
  ArrowCoords = data.frame(Region=ConGrid1@data$Region,x = rep(NA, length(ConGrid1@data$Region)),
                           y = rep(NA, length(ConGrid1@data$Region)))
  
  for (i in 1:length(ConGrid1)){
    
    if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@labpt
      x[1] = ifelse(x[1] > 0,x[1], 360-abs(x[1]) )
      ArrowCoords[i,2] = x[1]
      ArrowCoords[i,3] = x[2]
    }
    
    if (length(ConGrid1@polygons[[i]]@Polygons) ==2) {
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@labpt
      x[1] = ifelse(x[1] > 0,x[1], 360-abs(x[1]) )
      x2 = ConGrid1@polygons[[i]]@Polygons[[2]]@labpt
      x2[1] = ifelse(x2[1] > 0,x2[1], 360-abs(x2[1]) )
      x = c(mean(c(x[1], x2[1])), mean(c(x[2], x2[2])))
      ArrowCoords[i,2] = x[1]
      ArrowCoords[i,3] = x[2]
      
    }
  }
  
  
  CMap %<a-%{
  ## Label Zones ## 
  
  windows(width = 12, height = 12);getBeringMap()
  lab = 1:27
  x = c(200.35070, 197.06137, 193.84557, 192.25913, 191.10922, 197.80395, 195.25373, 192.18229, 189.12109, 187.36041, 195.43239, 193.43246, 189.65572, 186.79300, 184.07891, 185.20322, 179.17429, 179.1170, 174.95373, 191.13708, 186.71163, 178.9745, 174.44257 , 182.2094, 177.50780, 174.65190, 194.98104)
  y = c(57.65158,  58.57277,  59.20534,  60.28851,  61.68657,  56.14115,  56.82816,  57.87291,  59.05969,  60.28825,  54.95754,  55.65593,  56.96982,  58.22234,  59.63626,  62.82903,  64.07788,  61.7978,  61.52478,  54.65115,  56.19912,  60.0044,  60.83757,   55.0024,  57.65041,  59.49971,  53.94815)
  maptools::pointLabel(x, y,labels=paste(lab), cex=1.5, col="black")
  
  print("Making Connectivity Map...")
  ArrowCoords = ArrowCoords[order(ArrowCoords$Region),] 
  
  col <- colorRampPalette(c("Blue", "white","red"), bias=1)
  LW = 1:10
  
  for (i in 1:ncol(ConnectMatrix)){
    
    for (k in 1:nrow(ConnectMatrix))
      
      if(ConnectMatrix[k,i] > 10){
        #print(paste(k,i))
        
        
        if (i==k){
          diagram::curvedarrow(from = c(ArrowCoords[i,2], ArrowCoords[i,3]), to = c(ArrowCoords[i,2], ArrowCoords[i,3]) + c(0.8,0),
                               curve = -1, arr.pos = 1, arr.type="triangle", 
                               arr.col = col(100)[ConnectMatrix[k,i]/max(ConnectMatrix)*100],
                               lcol=col(100)[ConnectMatrix[k,i]/max(ConnectMatrix)*100],
                               lwd = LW[ConnectMatrix[k,i]/max(ConnectMatrix)*10])
        }
        
        else{ 
          arrows(ArrowCoords[k,2], ArrowCoords[k,3], ArrowCoords[i,2], ArrowCoords[i,3], 
                 col=col(100)[ConnectMatrix[k,i]/max(ConnectMatrix)*100],
                 lwd = LW[ConnectMatrix[k,i]/max(ConnectMatrix)*10])
        }
      }
  }
  }

  return(CMap)
  
}


