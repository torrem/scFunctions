# Read in list of batch runs
#
#'
#'@title Take raw disMELs connectivity results and produce dfrs list
#'
#'@description Function to produce dfrs list
#'
#'@return list of dfrs
#'
#'@export
#'
#'

#require(RCurl)
#require(getStandardAttributes)
#require(getLifeStageInfo)

#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
#source("https://github.com/torrem/SnowCrabFunctions/raw/master/BeringMap.R");

info<-getLifeStageInfo.SnowCrab();
typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels

readResults <-function(resdr, resfn){
  #read model results by java class and sort by startTime, id and time within a class
  cat("\n\n-----------------\n");
  cat("Reading and sorting connectivity files\n");
  dfrs<-list();
  for  (cls in names(info$classInfo)){
    cat("\tclass name:",cls,"\n");
     csv<-file.path(resdr,paste0(resfn,".",cls,".csv"));
     tmp<-readr::read_csv(csv,skip=1);
    # qry<-"select * from tmp
    #       order by startTime,id,time;"
    # tmps<-sqldf::sqldf(qry);
    # tmps$typeName<-factor(tmps$typeName,levels=typeNames);#change typeName from character to factor
     dfrs[[cls]]<-tmp;
  }
  #rm(csv,tmp,qry,tmps);

  # #--reorder model results into list of dataframes by typeName, not java class
  # dfrts<-list();
  # for (typeName in typeNames){
  #   dfrt<-NULL;
  #   for (cls in names(info$classInfo)){
  #     idx<-dfrs[[cls]]$typeName==typeName;
  #     if (any(idx)){
  #       if (sum(idx)==length(dfrs[[cls]]$typeName)) {
  #         tmp<-dfrs[[cls]];
  #       } else {
  #         tmp<-dfrs[[cls]][idx,];
  #       }
  #       dfrt<-rbind(dfrt,tmp);
  #     }
  #   }
  #   dfrts[[typeName]]<-dfrt;
  # }
  # dfrs<-dfrts;#NOTE that dfrs is now dataframes by typename which are ordered by id, startTime, and time
  # rm(dfrts,typeName,dfrt,cls,idx,tmp);

  print(paste("Saving dfrs to:",resdr,sep=""))
  save(dfrs, file=paste(resdr,"/dfrs.RData", sep=""));
  #return(dfrs)
  }

##testline



