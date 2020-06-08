# connectivity between zones
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a connectivity map
#'
#'@description Function to produce connectivity map
#'
#'@return map showing connectivity between zones
#'
#'@export
#'
#'

#require(RCurl)
#require(diagram)

# source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
# source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
# source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");



FEP <-function(group, path){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

  data(ConGridFinal)


  Zres = data.frame(Zone = vector(), Freq=vector(), Year=vector())
  Dres = data.frame(Depth = vector(), Freq=vector(), Year=vector())

  for (kk in 1:length(group)){
    #for (kk in 1:3){
    resdr = group[kk]
    load(paste(resdr,"/dfrs.RData",sep=""))
    ## convert coordinates to work with map ##
    for (i in c(1,4)){
      # print(paste("Convertving Coordinates for", typeNames[i]))
      #dfrs[[]]
      for (kkk in 1:nrow(dfrs[[i]])){
        dfrs[[i]][kkk,"horizPos1"]  = ifelse(dfrs[[i]][kkk,"horizPos1"] > 0,dfrs[[i]][kkk,"horizPos1"], 360-abs(dfrs[[i]][kkk,"horizPos1"]))
      }

    }


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





    ## Find starters and settlers ##
    starters = as.data.frame(dfrs[[1]])

    starters$starter = ifelse(starters$startTime == starters$time, 1,0)

    starters = subset(starters, starter==1)
    #starters = starters[!duplicated(starters$origID), ]






    settlers = dfrs[[4]]
    #settlers = settlers[!duplicated(settlers$origID), ]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used



    ## Connectivity Matrix ##

    ## match starters with connectivity zone
    starters = starters[!is.na(starters$horizPos1) & !is.na(starters$horizPos2),]
    sp::coordinates(starters)=~horizPos1 + horizPos2
    sp::proj4string(starters) <- sp::proj4string(ConGrid1)
    s = sp::over(starters, ConGrid1); starters= cbind(data.frame(starters),s) ## match ResultsConn file to connectivity grid


    # ## Match settlers with connectivity zone
    # settlers = as.data.frame(settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),])
    # sp::coordinates(settlers)=~horizPos1 + horizPos2
    # sp::proj4string(settlers) <- sp::proj4string(ConGrid1)
    # s = sp::over(settlers, ConGrid1); settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid

## pick out starters that settled
    Made_it = starters[match(settlers$origID, starters$origID  ),]

    library(dplyr)
    Zones <- as.data.frame(Made_it %>% group_by(OBJECTID) %>% summarise(Freq=n()))
    Z2 <- as.data.frame(starters %>% group_by(OBJECTID) %>% summarise(Freq=n()))
    PSurv =  merge(Zones,Z2,by="OBJECTID")
    PSurv = PSurv$Freq.x/PSurv$Freq.y




  Zones = cbind(Zones, PSurv,rep(strsplit(names(resdr),'_')[[1]][2], length(Zones$OBJECTID)) )
  colnames(Zones) = c('Zone', 'Freq', 'PSurv', 'Year')


  # subset zones 1-8
  Zones = Zones[Zones$Zone == 1 | Zones$Zone == 2
                |Zones$Zone == 3 | Zones$Zone == 4|
                  Zones$Zone == 5 | Zones$Zone == 6|
                  Zones$Zone == 7 | Zones$Zone == 8, ]

  Zones$propRec = Zones$Freq/nrow(settlers)

Depths =  hist(Made_it$bathym, breaks=seq(from = 0, to= max(Made_it$bathym)+50, by=50),include.lowest=FALSE, plot=FALSE)
Depths = as.data.frame(cbind(Depths$breaks[-1],Depths$counts,
                             rep(strsplit(names(resdr),'_')[[1]][2], length(Depths$counts))))

D2 =  hist(starters$bathym, breaks=seq(from = 0, to= max(starters$bathym)+50, by=50),include.lowest=FALSE, plot=FALSE)
D2 = as.data.frame(cbind(D2$breaks[-1],D2$counts,
                             rep(strsplit(names(resdr),'_')[[1]][2], length(D2$counts))))

Depths$V1 = as.numeric(as.character(Depths$V1));Depths$V2 = as.numeric(as.character(Depths$V2))
D2$V1 = as.numeric(as.character(D2$V1));D2$V2 = as.numeric(as.character(D2$V2))
Depths = subset(Depths, V1 < 500)
D2 = subset(D2, V1 < 500)

DPSurv = Depths$V2/D2$V2

Depths = cbind(Depths,DPSurv);
Depths = Depths[,c(1,2,4,3)]

colnames(Depths) = c('Depth', 'Freq', 'PSurv','Year')
Depths$Depth = as.numeric(as.character(Depths$Depth));Depths$Freq = as.numeric(as.character(Depths$Freq))
Depths = subset(Depths, Depth < 500)

Depths$propRec = Depths$Freq/nrow(settlers)


Zres = rbind(Zres, Zones)
Dres = rbind(Dres, Depths)

print(paste("calculating Float End Point for: ",names(group[kk])), sep="")

}

Zres$Yr = substr(as.character(Zres$Year), start=3, stop=4)
Zres$Yr = as.factor(Zres$Yr)
Zres$Yr = factor(Zres$Yr, levels = c(71:99, "00","01","02","03","04"))
Dres$Yr = substr(as.character(Dres$Year), start=3, stop=4)
Dres$Yr = as.factor(Dres$Yr)
Dres$Yr = factor(Dres$Yr, levels = c(71:99, "00","01","02","03","04"))

### make Plots

  library(ggplot2)
  library(ggpubr)

  #Zone Plots
 A =  ggplot(data=Zres, aes(x=Yr, y=propRec, group=Zone, colour=Zone)) +
    geom_point() +
    geom_line()+
    #ylim(0,10)+
    xlab("Year") +
    ylab("Proportion of Total Settlement")+
    theme(text = element_text(size=20))

 B =  ggplot(data=Zres, aes(x=Yr, y=PSurv, group=Zone, colour=Zone)) +
    geom_point() +
    geom_line()+
    #ylim(0,10)+
    xlab("Year") +
    ylab("Percent Settlement")+
    theme(text = element_text(size=20))

Dres$Depth = as.factor(Dres$Depth)

  #Depth Plots
C =   ggplot(data=Dres, aes(x=Yr, y=propRec, group=Depth, colour=Depth)) +
    geom_point() +
    geom_line()+
    #ylim(0,10)+
    xlab("Year") +
    ylab("Proportion of Total Settlement")+
    theme(text = element_text(size=20))

 D =  ggplot(data=Dres, aes(x=Yr, y=PSurv, group=Depth, colour=Depth)) +
    geom_point() +
    geom_line()+
    #ylim(0,10)+
    xlab("Year") +
    ylab("Percent Settlement")+
    theme(text = element_text(size=20))







 ## hindcast names##
 if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/FEP_",
                                                                ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                strsplit(names(group[1]),'_')[[1]][2],"-",
                                                                strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep="")
                                                          , width = 24, height = 10, units = "in", res = 600)

 }

 ## forecast names##
 if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/FEP_",
                                                                ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                strsplit(names(group[1]),'_')[[1]][3],"-",
                                                                strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep="")
                                                          , width = 24, height = 10, units = "in", res = 600)

 }



  figure <- ggarrange(A, B, C, D,
                      ncol = 2, nrow = 2)
print(figure)

dev.off()
}
