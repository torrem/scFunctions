# TRack individual movements of Z1 to C1
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that tracks movement of sucessfully settled individuals
#'
#'@description Function to map tracks of settled individuals
#'
#'@return map showing tracks of settled individuals

require(RCurl)

source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
source("https://github.com/torrem/SnowCrabFunctions/raw/master/BeringMap.R"); 

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

#TrackMap <-function(resdr){
resdr = TRh = "C:/Users/Mike/Documents/Snow Crab/Dismels Runs/Horz1000_Vert0"
load(paste(resdr,"/dfrs.RData",sep="")) 


#determine successful vs. unsuccessful indivs
#source("extractIndexForSuccessfulIndivs.R");
  settle<-c("C1M", 'C1F');#life stage at which success is evaluated

  OID = vector()
  for (i in 1:length(typeNames)){
     if (typeNames[i] %in% settle==TRUE){
       print(typeNames[i])
       OID = c(OID,dfrs[[i]][,"origID"])
     }
  }

  for (i in 1:length(typeNames)){
    print(paste("evaluating success for", typeNames[i]))
    dfrs[[i]]$successful = ifelse(dfrs[[i]][,'origID'] %in% unique(OID)==TRUE,TRUE ,FALSE)
   }

  #save(dfrs, file=paste(resdr,"/dfrs.RData", sep=""));
 # return(dfrs)
  summary(dfrs)

    #create basemap
  #dev.new(width=6, height=6, unit="in")
  windows(width = 12, height = 12);getBeringMap()


  ## convert coordinates to work with map ##
  for (i in 1:length(typeNames)){
    print(paste("Convertving Coordinates for", typeNames[i]))
    dfrs[[i]][,"horizPos1"]  = ifelse(dfrs[[i]][,"horizPos1"] > 0,dfrs[[i]][,"horizPos1"], 360-abs(dfrs[[i]][,"horizPos1"]))
    ## convert tracks
 


  ## plot trajectories
  for (i in 1:length(typeNames)){
    g = subset(dfrs[[i]], successful==TRUE)
    points(g$horizPos2~g$horizPos1, cex=1, col=i+2, pch=i+14)

  }

  legend("bottomleft",horiz=TRUE, legend=typeNames,
         col=3:(3+length(typeNames)), pch=15:(15+length(typeNames)), cex=1.5, bg="white")
  }

}