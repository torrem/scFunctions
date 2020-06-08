# Find time until settlement and percent survival
#
#'
#'@title Take disMELS output file dfrs (from readAllResults) and produce histogram of IMD for each stage
#'
#'@description Function to produce produce frequency plot that shows time of settle
#'
#'@return frequency plot that shows time of settle
#'
#'@export
#'
#'
#'


group=Fixed_Hindcast[25:34]




SetSurv <-function(group){

  TTS = data.frame(timeSet = vector())
  Surv = data.frame(SP = rep(NA, length(group)))

   for (kk in 1:length(group)){
    resdr = group[kk]
    load(paste(resdr,"/dfrs.RData",sep=""))




    #### find starters and settlers ####_
    starters = as.data.frame(dfrs[[1]])
    starters$starter = ifelse(starters$startTime == starters$time, 1,0)
    starters = subset(starters, starter==1)
    #starters = starters[!duplicated(starters$origID), ]

    settlers = dfrs[[4]]
    #settlers = settlers[!duplicated(settlers$origID), ]
    settlers = settlers[order(settlers$origID)[!duplicated(sort(settlers$origID))],] ## makes sure only unique settlers are used

    Surv[kk,1] = (nrow(settlers)/nrow(starters))*100




   }







}
