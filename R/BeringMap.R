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


getBeringMap<-function(addGrid=TRUE, addDepth=TRUE, openWindow=TRUE){

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
if(addDepth==FALSE){plot(ak, col='White', yaxs="i",legend=FALSE)}
raster::plot(m2, add=TRUE, col='grey')
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

#shape2 <- aggregate(ConGrid1,dissolve=T)

# install.packages("gpclib")
# library("gpclib")
# gpclibPermit()
# [1] TRUE
# Warning message:
# In gpclibPermit() :
#  support for gpclib will be withdrawn from maptools at the next major release

ConGrid1 <- maptools::unionSpatialPolygons(ConGrid1, ConGrid1@data$OBJECTID, avoidGEOS=FALSE)



plot(ConGrid1[10,])

summarise(ConGrid1[10,])

  if(addGrid==TRUE){raster::plot(ConGrid1, lwd=1.5,add=TRUE)}


rm(ak, m,envir = globalenv())

}
