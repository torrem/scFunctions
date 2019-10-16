# Map number of successful individuals per meter in each connectivity region
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that 
#'shows number of successful individuals per meter in each connectivity region
#'
#'@description Function to produce map of number of successful individuals per meter in each connectivity region
#'
#'@return map of number of successful individuals per meter in each connectivity region


resdr = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Test Run 5"


require(RCurl)
require(diagram)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://github.com/torrem/SnowCrabFunctions/raw/master/BeringMap.R");

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

SettlePerM2 <-function(resdr, Map=FALSE, addSettlers=TRUE){
  
  load(paste(resdr,"/dfrs.RData",sep="")) 
  

  ## convert coordinates to work with map ##
  for (i in 1:length(typeNames)){
    #print(paste("Convertving Coordinates for", typeNames[i]))
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
  
  SettleInZone = data.frame(Region = 1:27, NumSettlers = rep(NA, 27))
  for (i in 1:27){
    x = nrow(subset(settlers, Region==i))
    SettleInZone[i,2] = x
  }
  
  
 
  
  ZoneArea = c(45467923122.099998,48733983826.099998,41876641917.599998,29150756998.799999,50041280842.699997,28156982144.700001,
               48463658481.400002,32175199970.5,     43058333151.599998,51965675314.099998,4310824373.58,
               36272213040.599998,43645241013.099998,31182972193.700001,46377460312.400002,73897745180.199997,
               65750246504.099998,67469098918.199997,24266122353.799999,67796246840.199997,28853274483.599998,
               42299337357.599998,22181812802.799999,261250592356,156851996233,104870483607,29208450343.400002)
  
  
  SettleInZone = cbind(SettleInZone,ZoneArea)
  
  SettleInZone$Density = SettleInZone$NumSettlers/SettleInZone$ZoneArea
  
  SettleInZone$ColorCode = ((SettleInZone$Density-min(SettleInZone$Density))/
                              (max(SettleInZone$Density)-min(SettleInZone$Density))*100)
  
  RegionCode = data.frame(MapRegion = ConGrid1$Region, MapOrder = 1:27 )
  RegionCode <- RegionCode[order(RegionCode$MapRegion),]
  
   ## plot starters and settlers
  
  
  #create map
  
if (Map==TRUE){
 windows(width = 12, height = 12);getBeringMap(addGrid=FALSE,addDepth=FALSE)
 #mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red", "darkred"))
 mypal <- colorRampPalette(c("blue", "white", "red"))
  for (i in RegionCode[1:27,2]){
   
    c = SettleInZone$ColorCode[i]
    if(c<1){c=c+1}
    plot(ConGrid1[i,], col=mypal(100)[c], add=TRUE)
   }
  
  if(addSettlers==TRUE){points(settlers$horizPos2~settlers$horizPos1, cex=1, col="yellow", pch=15)}
}
 
  
  return(SettleInZone[,1:4])
  
  
} 
  
  
  
  
  
  
  
