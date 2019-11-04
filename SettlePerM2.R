# Map number of successful individuals per meter in each connectivity region
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that
#'shows number of successful individuals per meter in each connectivity region
#'
#'@description Function to produce map of number of successful individuals per meter in each connectivity region
#'
#'@return map of number of successful individuals per meter in each connectivity region


require(RCurl)
require(diagram)
require(RColorBrewer)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

SettlePerM2 <-function(group){



  SIZ = data.frame(Region = 1:27)

  for (kk in 1:length(group)){
  resdr = group[kk]
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

  settlers = dfrs[[5]]
  settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

  ## Pull out Settlers (C1M & C1F) from dfrs
  settlers = settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),]
  coordinates(settlers)=~horizPos1 + horizPos2
  s = over(settlers, ConGrid1);s$Region[is.na(s$Region)] <- 999


  RegionCode = data.frame(MapRegion = ConGrid1$Region, MapOrder = 1:27 )

  ##change congrid1 region code to connectivity zone code on my map ##
  # for (i in 1:15){
  #   if (s[i,2]==999){next}
  #   s[i,2] = RegionCode[which(RegionCode[,2]==s[i,2]),1]
  #
  # }


  settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid



  SettleInZone = data.frame(Region = 1:27, NumSettlers = rep(NA, 27))





  ZoneArea = c(45467923122.099998,48733983826.099998,41876641917.599998,29150756998.799999,50041280842.699997,28156982144.700001,
               48463658481.400002,32175199970.5,     43058333151.599998,51965675314.099998,4310824373.58,
               36272213040.599998,43645241013.099998,31182972193.700001,46377460312.400002,73897745180.199997,
               65750246504.099998,67469098918.199997,24266122353.799999,67796246840.199997,28853274483.599998,
               42299337357.599998,22181812802.799999,261250592356,156851996233,104870483607,29208450343.400002)

  SettleInZone = cbind(SettleInZone,ZoneArea)


 # SettleInZone$Region = RegionCode$MapRegion

  SettleInZone$R2 = RegionCode$MapRegion
  #SettleInZone <- SettleInZone[order(SettleInZone$R2),]

    for (i in 1:27){
      x = nrow(subset(settlers, Region==i))
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

  SettleInZone$ColorCode = ifelse(round((SettleInZone$Density/0.00000001)*9)>9,
                                  9,round((SettleInZone$Density/0.00000001)*9) )



   ## plot starters and settlers


  #create map




 windows(width = 8, height = 8);getBeringMap(addGrid=FALSE,addDepth=FALSE)
 # mypal <- colorRampPalette(c("blue" ,"red","yellow"), bias=1)
 #mypal <- colorRampPalette(c("blue", "cyan", "yellow", "red", "darkred"))
 #mypal <- colorRampPalette(c("blue", "lightblue1", "lightpink", "red"))
 for (i in 1:27){

    ii = which(SettleInZone[,4]==i)
    c = SettleInZone$ColorCode[i]
    if(c<1){c=c+1}
    plot(ConGrid1[ii,], col=rev(brewer.pal(9,"RdYlBu"))[c], add=TRUE)
 }


  lgd_ = rep(NA, 9)
  lgd_[c(1,5,9)] = c("1 x 10^-8","5 x 10^-9",0)


  legend(x = "bottomright", y = 1,
         legend = lgd_,
         fill = brewer.pal(9,"RdYlBu"),
         border = NA,
         bty="n",
         title="Density (per m^2)
      ",
         y.intersp = 0.5,
         cex = 1.5, text.font = 2)




  #return(SettleInZone[,1:4])


}








