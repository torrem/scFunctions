#'
#'@title Make a Bering Sea basemap to plot disMELS results
#'
#'@description Function to get a Bering Sea basemap.
#'
#'@return a base graphic map saved as a pseudo-object in R
#'
#'@export


# require(raster)
# require(mapdata)
# require(maptools)
# require(pryr)
# require(repmis)
# require(rangeBuilder)

#setwd("https://github.com/torrem/SnowCrabFunctions.git")


getBeringMap<-function(addGrid=TRUE, addLand = TRUE, addDepth=TRUE, openWindow=TRUE, ConGridNum = FALSE){

  data(ak)
  data(ConGridFinal)
  data(AlaskaLand)

  #repmis::source_data("https://github.com/torrem/scFunctions/blob/master/data/ConGrid.Rdata?raw=true")
  #repmis::source_data("https://github.com/torrem/scFunctions/blob/master/data/ak.Rdata?raw=true")

  if(openWindow==TRUE){windows(width = 12, height = 12)}

####--- Add depth raster map with land----#


#ak2 =raster("C:/Users/Mike/Documents/Snow Crab/Shapefiles/CRM_AK/crm_southak.asc")
#ak =ak2
ak =  raster::crop(ak,c(168,210,48,70))
ak[ak>=0]<-NA


### add land###----
#plot(wrld_simpl, col='grey', xlim = c(min(longGrid), max(longGrid)), ylim = c(min(latGrid),max(latGrid)))

# m = map('world2Hires',  c('USA:Alaska', 'USSR'),
#         fill = TRUE, plot=FALSE, col='grey', xlim= c(175,210))
m2 <- maptools::map2SpatialPolygons(m, IDs=m$names, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

#png(filename = "AKMapStartCentSettle.png", res = 600, height = 6, width = 6, units = "in")

#colorRampPalette(c("black", "blue" ,"purple","yellow")
#colorRampPalette(c("red","purple","blue","cadetblue1","white"))
#dev.new()
#BERMap %<a-%{
if(addDepth==TRUE){
  mypal <- colorRampPalette(c("blue3", "dodgerblue1" , "cyan","seagreen1","lightgoldenrod1","tan1"), bias=0.015)
raster::plot(ak, col=mypal(10000), yaxs="i", legend=FALSE)
mypal1 = colorRampPalette(c("blue3", "dodgerblue1" , "cyan","seagreen1","lightgoldenrod1","tan1"))
#addRasterLegend(ak, ramp=mypal1(10000), longFrac=0.8, ncolors = 10000)
}
if(addDepth==FALSE){raster::plot(ak, col='White', yaxs="i",legend=FALSE)}
if(addLand==TRUE){raster::plot(m2, add=TRUE, col='grey')}
#dev.off()

#save(ak, m2, file = "AKMapData.RData")
#load("AKMapData.RData")

## SP version of the map
# spplot(ak, sp.layout = list("sp.polygons", m2, first = FALSE, fill = "grey"),
#        scales = list(draw = TRUE),
#        col.regions = mypal(100))

#save(AKMap, file = "AKMap.RData")


###- add grid----



## Transform coordinates tow work with other components of map ##

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
















ConGrid1 <- maptools::unionSpatialPolygons(ConGrid1, ConGrid1@data$OBJECTID, avoidGEOS=FALSE)

  if(addGrid==TRUE){raster::plot(ConGrid1, lwd=2,add=TRUE)}
  if (ConGridNum == TRUE){ maptools::pointLabel(ArrowCoords$dd, ArrowCoords$gg,labels=paste(ArrowCoords$Region), cex=1.5, col="black")}

rm(ak, m,envir = globalenv())

}
