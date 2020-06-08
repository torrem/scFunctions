IMDHist <-function(group, path){

  FreqTable = data.frame(year=numeric(), typeName=character(), origID=numeric(),ageInStage=numeric(), TempInStage=numeric())


  for (kk in 1:length(group)){
    resdr = group[kk]
    load(paste(resdr,"/dfrs.RData",sep=""))
