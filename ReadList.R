# Read in list of batch runs
#
#'
#'@title Take raw disMELs resutls and produce dfrs list
#'
#'@description Function to produce dfrs list
#'
#'@return list of dfrs
#'
#'@export
#'
#'



ReadList <-function(FoldDir){

#### Read in lists---------------

  Aresd = FoldDir

    ## make A list##

    Afold =  list.files (pattern = "^[:0-2:][0-9][0-9][0-9]", path = Aresd)

    A = list()
    for (i in 1:length(Afold)){
      A[i] = paste(Aresd,"/",Afold[i],sep="")
      names(A)[i]= paste('A',Afold[i], sep="")
    }

    ## check each batch run and get rid of Years with no settlers ##
    rmlist = vector()
    for ( i in 1:length(Afold)){
      Fsize = file.info(paste(A[[i]],"/results.disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale.csv", sep=""))$size
      if (Fsize < 5000){
        rmlist = c(rmlist,i)
        print(paste("removing ", Afold[i], " due to low settler numbers" ,sep=""))
      }

    }
    if(length(rmlist>0) ){A = A[-rmlist]}

    ## make A dfrs for each item in list ##
    for(i in 1:length(A)){
      if (     ("dfrs.RData" %in% list.files(path = paste(A[i])))==TRUE          ){
        print(paste(names(A)[i], " has already been formatted", sep=""))
      }
      if (     ("dfrs.RData" %in% list.files(path = paste(A[i])))==FALSE          ){
        print(paste("formatting ",names(A)[i], sep=""))
        readResults(resdr = A[i], resfn = 'results.' )
      }

    }
return(A)

}
