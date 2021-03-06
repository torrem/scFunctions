# connectivity MATRIX between zones
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a connectivity matrix
#'
#'@description Function to produce connectivity matrix
#'
#'@return matrix showing connectivity between zones
#'
#'@export
#'
#'

library(viridis)

ConnMatrixMean <-function(group, path){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

  data(ConGridFinal)

  CMlist <- vector("list", 2)

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

    ## pull out starters from Z1 in dfrs
    starters = starters[!is.na(starters$horizPos1) & !is.na(starters$horizPos2),]
    sp::coordinates(starters)=~horizPos1 + horizPos2
    sp::proj4string(starters) <- sp::proj4string(ConGrid1)
    s = sp::over(starters, ConGrid1); starters= cbind(data.frame(starters),s) ## match ResultsConn file to connectivity grid

    ## Get rid of starters that are on the fringe of sink regions
    h = table(starters$OBJECTID)
    starters = subset(starters,!(OBJECTID %in% as.numeric(names(which(h < max(h)* 0.16)))))

    ## Pull out Settlers (C1M & C1F) from dfrs
    settlers = as.data.frame(settlers[!is.na(settlers$horizPos1) & !is.na(settlers$horizPos2),])
    sp::coordinates(settlers)=~horizPos1 + horizPos2
    sp::proj4string(settlers) <- sp::proj4string(ConGrid1)
    s = sp::over(settlers, ConGrid1); settlers= cbind(data.frame(settlers),s) ## match ResultsConn file to connectivity grid



    ##### subset out settlers by time and temperature #####_

    settlers = subset(settlers, age<=200 & temperature > 0)



    StartInRegion = data.frame(OBJECTID = 1:18, NumStarters = rep(NA, 18))
    for (i in 1:18){
      d = nrow(subset(starters, OBJECTID==i))
      StartInRegion[i,2] = d
    }

    ConnectMatrix = matrix(nrow=18, ncol = 18, dimnames=list(c(1:18),c(1:18)))

    for (i in 1:18){
      dd = subset(settlers, OBJECTID==i)
      gg = as.data.frame(table(starters[dd$origID,]$OBJECTID))

      # print(nrow(dd))
      if (nrow(dd) > 0){
        for (k in 1:nrow(gg)){
          ConnectMatrix[as.numeric(paste(gg[k,1])),i] = gg[k,2]
        }
      }
    }


    ConnectMatrix[is.na(ConnectMatrix)] = 0

    for (i in 1:ncol(ConnectMatrix)){
      ConnectMatrix[,i] = round(((ConnectMatrix[,i]/StartInRegion[,2])*100),1)
    }

    ConnectMatrix[is.nan(ConnectMatrix)] = 0


    CMlist[[kk]] = ConnectMatrix
    names(CMlist)[kk] = paste("CM",names(group[kk]), sep="")

    print(paste("calculating connecivity matrix for: ",names(group[kk])), sep="")
  }




  ConnectMatrixMean = apply(simplify2array(CMlist), 1:2, mean)
  ConnectMatrixVAR = apply(simplify2array(CMlist), 1:2, var)

  ConnectMatrixMean = ConnectMatrixMean[1:8,]
  ConnectMatrixMean <- apply(ConnectMatrixMean, 2, rev)

  ConnectMatrixVAR = ConnectMatrixVAR[1:8,]
  ConnectMatrixVAR <- apply(ConnectMatrixVAR, 2, rev)













### figure out names ####+

## group1 names ##
## hindcast names##
if(length(strsplit(names(group[1]), '_')[[1]])==2){name1 = paste(ifelse(regexpr("Temp", group[1])[1] >0,"TempMM", "FixedMM"),"_",
                                                                  ifelse(regexpr("AltVM", group[1])[1] >0,"AltVM", ""),"_",
                                                                  strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                  strsplit(names(group[1]),'_')[[1]][2],"-",
                                                                  strsplit(names(group[length(group)]),'_')[[1]][2],sep="")
}








  paste(path,"/meanVarPlot_",1,"_",name1, ".png",sep="")






   png(  paste(path,"/meanVarPlot_",name1, ".png",sep="")
         , width = 10, height = 6, units = "in", res = 600)

   par(mfrow=c(1,2));par(oma=c(3,3,3,3)) # all sides have 3 lines of space
   par(mar=c(5,4,4,2) + 0.1)
   cuts=seq(from=0,to=35, by=5) #set breaks

   plot(ConnectMatrixMean, breaks=cuts, col=viridis(length(cuts)),ylab="", xlab="",main="")
   plot(ConnectMatrixVAR, col=plasma(length(cuts)),ylab="",xlab="",main="")

 dev.off()


}


