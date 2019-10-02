#'
#'@title Make a Bering Sea basemap to plot disMELS results
#'
#'@description Function to get a Bering Sea basemap.
#'
#'@return a base graphic map saved as a pseudo-object in R


require(raster)
require(mapdata)
require(maptools)
require(pryr)
require(repmis)

#setwd("https://github.com/torrem/SnowCrabFunctions.git")

source_data("https://github.com/torrem/SnowCrabFunctions/blob/master/Shapefiles/ConGrid.Rdata?raw=true")
source_data("https://github.com/torrem/SnowCrabFunctions/blob/master/Shapefiles/ak.Rdata?raw=true")

getBeringMap<-function(addGrid=TRUE, addDepth=TRUE){


####--- Add depth raster map with land----#


#ak2 =raster("C:/Users/Mike/Documents/Snow Crab/Shapefiles/CRM_AK/crm_southak.asc")
#ak =ak2
ak =  crop(ak,c(168,210,48,70))
ak[ak>=0]<-NA


### add land###----
#plot(wrld_simpl, col='grey', xlim = c(min(longGrid), max(longGrid)), ylim = c(min(latGrid),max(latGrid)))

m = map('world2Hires',  c('USA:Alaska', 'USSR'),
        fill = TRUE, plot=FALSE, col='grey', xlim= c(175,210))
m2 <- map2SpatialPolygons(m, IDs=m$names, proj4string=CRS("+proj=longlat +datum=WGS84"))

#png(filename = "AKMapStartCentSettle.png", res = 600, height = 6, width = 6, units = "in")


#dev.new()
BERMap %<a-%{
if(addDepth==TRUE){mypal <- colorRampPalette(c("black", "blue" ,"purple","yellow"), bias=0.05)
plot(ak, col=mypal(1000), yaxs="i", legend.args = list(text = 'Depth (m)', cex=0.8))}
if(addDepth==FALSE){plot(ak, col='White', yaxs="i",legend=FALSE)}  
plot(m2, add=TRUE, col='grey')
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
    
    if (length(ConGrid1@polygons[[i]]@Polygons) ==2) {
      x = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = x
      
      x = ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1]
      x = ifelse(x > 0,x, 360-abs(x) )
      ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1] = x
    }
  }
  
  if(addGrid==TRUE){plot(ConGrid1,add=TRUE)}

}

return(BERMap)

}
