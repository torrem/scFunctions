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



ConnMatrix <-function(group1, group2, path){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

  data(ConGridFinal)

  CMlist <- vector("list", 2)

  for (kk in 1:length(group1)){
    #for (kk in 1:3){
    resdr = group1[kk]
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
    names(CMlist)[kk] = paste("CM",names(group1[kk]), sep="")

    print(paste("calculating connecivity matrix for: ",names(group1[kk])), sep="")
  }

  ConnectMatrix1 = apply(simplify2array(CMlist), 1:2, mean)






  #--------------------------------------




  CMlist2 <- vector("list", 2)

  for (kk in 1:length(group2)){
    #for (kk in 1:3){
    resdr = group2[kk]
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


    CMlist2[[kk]] = ConnectMatrix
    names(CMlist2)[kk] = paste("CM",names(group2[kk]), sep="")

    print(paste("calculating connecivity matrix for: ",names(group2[kk])), sep="")
  }

  ConnectMatrix2 = apply(simplify2array(CMlist2), 1:2, mean)





  ConnectMatrix3 = ConnectMatrix2 - ConnectMatrix1


  ConnectMatrix3

  ConnectMatrix4 = ConnectMatrix3[1:8,]
  ConnectMatrix4 <- apply(ConnectMatrix4, 2, rev)
ConnectMatrix4 = round(ConnectMatrix4,1)





### figure out names ####+

## group1 names ##
## hindcast names##
if(length(strsplit(names(group1[1]), '_')[[1]])==2){name1 = paste(ifelse(regexpr("Temp", group1[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                  strsplit(names(group1[1]),'_')[[1]][1],"_",
                                                                  strsplit(names(group1[1]),'_')[[1]][2],"-",
                                                                  strsplit(names(group1[length(group1)]),'_')[[1]][2],sep="")
}




## forecast names##
if(length(strsplit(names(group1[1]), '_')[[1]])>2){name1 = paste(ifelse(regexpr("Temp", group1[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group1[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group1[1]),'_')[[1]][2],"_",
                                                                 strsplit(names(group1[1]),'_')[[1]][3],"-",
                                                                 strsplit(names(group1[length(group1)]),'_')[[1]][3],sep="")


}



## group2 names ##
## hindcast names##
if(length(strsplit(names(group2[1]), '_')[[1]])==2){name2 = paste(ifelse(regexpr("Temp", group2[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                  strsplit(names(group2[1]),'_')[[1]][1],"_",
                                                                  strsplit(names(group2[1]),'_')[[1]][2],"-",
                                                                  strsplit(names(group2[length(group2)]),'_')[[1]][2],sep="")
}




## forecast names##
if(length(strsplit(names(group2[1]), '_')[[1]])>2){name2 = paste(ifelse(regexpr("Temp", group2[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group2[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group2[1]),'_')[[1]][2],"_",
                                                                 strsplit(names(group2[1]),'_')[[1]][3],"-",
                                                                 strsplit(names(group2[length(group2)]),'_')[[1]][3],sep="")


}




  mypal <- colorRampPalette(c("blue",
                              DescTools:: MixColor('blue', 'white', amount1 = 0.8),
                              DescTools:: MixColor('blue', 'white', amount1 = 0.5),
                              DescTools::MixColor('blue', 'white', amount1 = 0.2),
                              "white",'white',
                              DescTools::MixColor('red', 'white', amount1 = 0.2),
                              DescTools::MixColor('red', 'white', amount1 = 0.5),
                              DescTools::MixColor('red', 'white', amount1 = 0.8),
                              "red"),
                            bias=1)



  png(  paste(path,"/MatrixPlot_",name1,"_",name2, ".png",sep="")
        , width = 12, height = 12, units = "in", res = 600)

  plot(ConnectMatrix4,digits=1, col=mypal(20), cex=1, main=paste(name1," - ", name2, sep="") )

dev.off()


}




