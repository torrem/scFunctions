# interannual connectivity
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a line graph showing major patterns of interannual connectivity
#'
#'@description Function to produce line graph
#'
#'@return line graph
#'
#'@export
#'
#'



IAV <-function(group, path, numConn = 6){

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


    # for (i in 1:length(ConGrid1)){
    #
    #   if (length(ConGrid1@polygons[[i]]@Polygons) ==1){
    #     dd = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
    #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
    #     ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = dd
    #   }
    #
    #   if (length(ConGrid1@polygons[[i]]@Polygons) ==2) {
    #     dd = ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1]
    #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
    #     ConGrid1@polygons[[i]]@Polygons[[1]]@coords[,1] = dd
    #
    #     dd = ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1]
    #     dd = ifelse(dd > 0,dd, 360-abs(dd) )
    #     ConGrid1@polygons[[i]]@Polygons[[2]]@coords[,1] = dd
    #   }
    # }


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
    names(CMlist)[kk] = paste("CM",names(group[kk]), sep="")

    print(paste("calculating connecivity matrix for: ",names(group[kk])), sep="")
  }

  MEANConnectMatrix = apply(simplify2array(CMlist), 1:2, mean)



  gg = tail(sort(as.matrix( MEANConnectMatrix)), numConn)

  Conns = data.frame(spawn = vector(), sink = vector())
  for (i in 1:length(gg)){
    g = which(MEANConnectMatrix ==  gg[i], arr.ind = TRUE)
    Conns[i,1] = g[1]
    Conns[i,2] = g[2]
}


for (i in 1:length(group)){

  x = CMlist[[i]]

  for(h in 1:nrow(Conns)){
  Conns[h,i+2] =  x[Conns$spawn[h],Conns$sink[h]]
  }



 # colnames(Conns)[i+2] = paste0("YR_", regmatches(names(CMlist)[i], regexpr("[:0-2:][0-9][0-9][0-9]", names(CMlist)[i])))
   colnames(Conns)[i+2] = paste0("YR_", regmatches(names(CMlist)[i], regexpr("[:0-2:][0-9][0-9][0-9]", names(CMlist)[i])))


}

yearList = vector()
  for (i in 1:length(group)){
   x =  regmatches(names(group)[i], regexpr("[:0-2:][0-9][0-9][0-9]", names(group)[i]))
    yearList = c(yearList,x)
  }


  Conns2 = data.frame(year=vector(),spawn = vector(), sink=vector(), connectivity = vector())


  for (i in 1:(length(Conns)-2)){
    x = data.frame(year=rep(regmatches(names(Conns)[i+2], regexpr("[:0-2:][0-9][0-9][0-9]", names(Conns)[i+2])),numConn),
                   spawn = Conns$spawn,
                   sink=Conns$sink,
                   connectivity =Conns[,i+2])


    Conns2 = rbind(Conns2,x)




  }


  Conns2$ID = paste("spawn ",Conns2$spawn,"_", "settle ",Conns2$sink,sep="")

  Conns2$year = as.numeric(Conns2$year)


  ## hindcast names##
  if(length(strsplit(names(group[1]), '_')[[1]])==2){png(  paste(path,"/IAVConn_",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][2],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }

  ## forecast names##
  if(length(strsplit(names(group[1]), '_')[[1]])>2){png(   paste(path,"/IAVConn_",
                                                                 ifelse(regexpr("Temp", group[1])[1] >0,"TempIMD", "FixedIMD"),"_",
                                                                 strsplit(names(group[1]),'_')[[1]][1],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][2],"_",
                                                                 strsplit(names(group[1]),'_')[[1]][3],"-",
                                                                 strsplit(names(group[length(group)]),'_')[[1]][3],".png",sep="")
                                                           , width = 12, height = 12, units = "in", res = 600)

  }







  theme_set(
    theme_classic(base_size = 14)
  )

  m = ggplot(Conns2, aes(x=year, y=connectivity, color=ID) )+
    geom_line(size=1.5) +
    theme(legend.position="bottom",legend.text=element_text(size=7),
          legend.title = element_blank()) +
    theme(panel.background = element_blank())+
    scale_x_discrete(name ="Year",
                     limits=yearList)+
    ylab("Connectivity (%)")




print(m)




  dev.off()


}





### need to standardize arrow scale
