# Max age of individuals
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce boxplot of average temp for each lifestage
#'
#'@description Function to produce produce hist of max age
#'
#'@return hist that shows max age
#'
#'@export
#'
#'


AvgTemp <-function(group1, group2, group3,
                   g1name ="Hindcast", g2name = "RCP4.5", g3name = "RCP8.5",
                   col1='#F8766D', col2='#0073C2FF', col3='coral'){

  FreqTableX = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Life_Stage", "Temp"))

  for (i in 1:length(group1)){
    resdr = group1[i]
    load(paste(resdr,"/dfrs.RData",sep=""))

    Z1 = dfrs[[1]]
    Z2 = dfrs[[2]]
    M = dfrs[[2]]

    settlers = dfrs[[4]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used


    for (kk in sample(settlers$origID,100, replace=TRUE)){

      Z1T = data.frame(Life_Stage = "Z1",Temp = mean(Z1[which(Z1$origID==kk),22]))
      Z2T = data.frame(Life_Stage = "Z2",Temp = mean(Z2[which(Z2$origID==kk),22]))
      MT = data.frame(Life_Stage = "M",Temp = mean(M[which(M$origID==kk),22]))
      FreqTableX = rbind(FreqTableX,Z1T, Z2T, MT)
    }


    print(paste("reading: ",names(group1[i])), sep="")
  }
  FreqTableX$Group = rep(g1name, nrow(FreqTableX))





  FreqTableY = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Life_Stage", "Temp"))

  for (i in 1:length(group2)){
    resdr = group2[i]
    load(paste(resdr,"/dfrs.RData",sep=""))

    Z1 = dfrs[[1]]
    Z2 = dfrs[[2]]
    M = dfrs[[3]]

    settlers = dfrs[[5]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used


    for (kk in sample(settlers$origID,100,replace=TRUE)){

      Z1T = data.frame(Life_Stage = "Z1",Temp = mean(Z1[which(Z1$origID==kk),22]))
      Z2T = data.frame(Life_Stage = "Z2",Temp = mean(Z2[which(Z2$origID==kk),22]))
      MT = data.frame(Life_Stage = "M",Temp = mean(M[which(M$origID==kk),22]))
      FreqTableY = rbind(FreqTableY,Z1T, Z2T, MT)
    }


    print(paste("reading: ",names(group2[i])), sep="")
  }

  FreqTableY$Group = rep(g2name, nrow(FreqTableY))







  FreqTableZ = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Life_Stage", "Temp"))

  for (i in 1:length(group3)){
    resdr = group3[i]
    load(paste(resdr,"/dfrs.RData",sep=""))

    Z1 = dfrs[[1]]
    Z2 = dfrs[[2]]
    M = dfrs[[3]]

    settlers = dfrs[[5]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used


    for (kk in sample(settlers$origID,100,replace=TRUE)){

      Z1T = data.frame(Life_Stage = "Z1",Temp = mean(Z1[which(Z1$origID==kk),22]))
      Z2T = data.frame(Life_Stage = "Z2",Temp = mean(Z2[which(Z2$origID==kk),22]))
      MT = data.frame(Life_Stage = "M",Temp = mean(M[which(M$origID==kk),22]))
      FreqTableZ = rbind(FreqTableZ,Z1T, Z2T, MT)
    }


    print(paste("reading: ",names(group1[i])), sep="")
  }
  FreqTableZ$Group = rep(g3name, nrow(FreqTableZ))





  FreqTableXYZ = rbind(FreqTableX,FreqTableY,FreqTableZ)




  mu <- FreqTableXYZ %>%
    group_by(Group,Life_Stage) %>%
    summarise(grp.mean = mean(Temp))
  mu



  windows(width = 12, height = 12)
  ggplot(FreqTableXYZ, aes(x = Life_Stage, y = Temp)) +
    geom_boxplot(width = 0.7, fill = "white",outlier.shape = NA) +
    facet_wrap(~ Group)+
    theme(legend.position="bottom")+
   # theme(legend.text = element_text( size=12,face="bold"))+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_jitter(aes(color = Life_Stage, shape = Life_Stage),
                width = 0.2, size = 1.5) +
    # geom_hline(aes(yintercept = grp.mean, color = Life_Stage),
    #            data = mu, linetype = "dashed", lwd=1) +
    scale_color_manual(values = c("grey","#00AFBB", "#E7B800"))+
    labs(x = NULL, y = 'Mean temperature (C)')












}
