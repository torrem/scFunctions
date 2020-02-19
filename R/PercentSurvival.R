# Percent survival
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce a map that maps settled individuals
#'
#'@description Function to produce map settled individuals
#'
#'@return percent survival
#'@export
#'
#'



#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getStandardAttributes.R");
#source("C:/Users/Mike/Documents/Snow Crab/SnowCrabFunctions/ResultsRead/getLifeStageInfo.SnowCrab.R");
#source("https://raw.githubusercontent.com/torrem/scFunctions/master/BeringMap.R");





PSurv <-function(group){

  info<-getLifeStageInfo.SnowCrab();
  typeNames<-factor(info$lifeStageTypes$typeName,levels=info$lifeStageTypes$typeName);#typeNames as factor levels
  #data(ConGridFinal)

Survive = vector()
  for (kk in 1:length(group)){

      resdr = group[kk]
      load(paste(resdr,"/dfrs.RData",sep=""))


    ## Find settlers ##
    settlers = dfrs[[3]]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],]-

    ## Find starters ##
    starters = dfrs[[1]];
    starters=subset(starters, typeName=="Z1")
    starters = starters[order(starters$origID)[!duplicated(sort(starters$origID))],]


    Survive = c(Survive, ((nrow(settlers)/nrow(starters))*100))

}





paste("mean",mean(Survive), "SE", plotrix::std.error(Survive),sep=" ")

}


