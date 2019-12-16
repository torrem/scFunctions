#'
#'@title Get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@description Function to get a list that defines life stage info for the DisMELS Pacific cod IBM.
#'
#'@param resType - results file type (i.e., 'NEW2.0SC')
#'@return a list
#'
#'@details none
#'
#'@export
#'
getLifeStageInfo.SnowCrab<-function(resType='NEW2.0SC'){

    #get standard attributes dataframe
    dfrStdAtts <- getStandardAttributes(resType);

    #java LHS class names
    classNames<-c('disMELS.IBMs.SnowCrab.Zooea.Zooea',
                  'disMELS.IBMs.SnowCrab.Megalopa.Megalopa',
                  'disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale',
                  'disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale');

    #information on 'additional attributes' for each life stage class
    ZooeaClassInfo<-rbind(data.frame(short_name="molt indicator",  data_type="numeric",name="molt indicator",        stringsAsFactors=FALSE),
                        data.frame(short_name="shellthickness",  data_type="numeric",name="shell thickness",         stringsAsFactors=FALSE),
                        data.frame(short_name="temperature",      data_type="numeric",name="temperature",            stringsAsFactors=FALSE),
                        data.frame(short_name="salinity",   data_type="numeric",name="salinity",                     stringsAsFactors=FALSE),
                        data.frame(short_name="pH",      data_type="numeric",name="pH",                              stringsAsFactors=FALSE));
    ZooeaClassInfo  <-rbind(dfrStdAtts,as.data.frame(ZooeaClassInfo,stringsAsFactors=FALSE));

    MegalopaClassInfo<-rbind(data.frame(short_name="molt indicator",  data_type="numeric",name="molt indicator",        stringsAsFactors=FALSE),
                             data.frame(short_name="shellthickness",  data_type="numeric",name="shell thickness",       stringsAsFactors=FALSE),
                             data.frame(short_name="temperature",      data_type="numeric",name="temperature",          stringsAsFactors=FALSE),
                             data.frame(short_name="salinity",   data_type="numeric",name="salinity",                   stringsAsFactors=FALSE),
                             data.frame(short_name="pH",      data_type="numeric",name="pH",                            stringsAsFactors=FALSE));
    MegalopaClassInfo  <-rbind(dfrStdAtts,as.data.frame(MegalopaClassInfo,stringsAsFactors=FALSE));


    ImmatureMaleClassInfo<-rbind(data.frame(short_name="instar",   data_type="character",name="instar?",                   stringsAsFactors=FALSE),
                           data.frame(short_name="ageInInstar",    data_type="numeric",  name="ageInInstar",              stringsAsFactors=FALSE),
                           data.frame(short_name="moltIndicator",      data_type="numeric",  name="moltindicator",        stringsAsFactors=FALSE),
                           data.frame(short_name="size",            data_type="numeric",  name="size (mm CW)",                 stringsAsFactors=FALSE),
                           data.frame(short_name="weight",       data_type="numeric",  name="weight (g)",                 stringsAsFactors=FALSE),
                           data.frame(short_name="shellcondition",   data_type="numeric",  name="shell condition",        stringsAsFactors=FALSE),
                           data.frame(short_name="temperature",      data_type="numeric",name="temperature",          stringsAsFactors=FALSE),
                           data.frame(short_name="salinity",   data_type="numeric",name="salinity",                   stringsAsFactors=FALSE),
                           data.frame(short_name="pH",      data_type="numeric",name="pH",                            stringsAsFactors=FALSE));
    ImmatureMaleClassInfo  <-rbind(dfrStdAtts,as.data.frame(ImmatureMaleClassInfo,stringsAsFactors=FALSE));

    ImmatureFemaleClassInfo <-ImmatureMaleClassInfo;

    #class info, by class
    classInfo<-list();
    classInfo[['disMELS.IBMs.SnowCrab.Zooea.Zooea']]                   <-list(info=ZooeaClassInfo,              typeNames=c("Z1"));
    classInfo[['disMELS.IBMs.SnowCrab.Zooea.Zooea']]                   <-list(info=ZooeaClassInfo,              typeNames=c("Z2"));
    classInfo[['disMELS.IBMs.SnowCrab.Megalopa.Megalopa']]             <-list(info=MegalopaClassInfo,           typeNames=c("M1"));
    classInfo[['disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale']]     <-list(info=ImmatureMaleClassInfo,       typeNames=c("C1M"));
    classInfo[['disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale']]   <-list(info=ImmatureFemaleClassInfo,     typeNames=c("C1F"));


    #map of defined life stage type names to class names
    lifeStageTypes<-rbind(data.frame(typeName="Z1",      class='disMELS.IBMs.SnowCrab.Zooea.Zooea',
                                     name="Zooea1",   nextType="Z1",              stringsAsFactors=FALSE),
                          data.frame(typeName="Z2",      class='disMELS.IBMs.SnowCrab.Zooea.Zooea',
                                     name="Zooea2",   nextType="M1",              stringsAsFactors=FALSE),
                          data.frame(typeName="M1",       class='disMELS.IBMs.SnowCrab.Megalopa.Megalopa',
                                     name="Megalopa", nextType="C1",              stringsAsFactors=FALSE),
                          data.frame(typeName="C1M",         class='disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureMale',
                                     name="ImmatureMale", nextType="C2M",       stringsAsFactors=FALSE),
                          data.frame(typeName="C1F",          class='disMELS.IBMs.SnowCrab.ImmatureCrab.ImmatureFemale',
                                     name="ImmatureFemale", nextType="C2F", stringsAsFactors=FALSE));

    return(invisible(list(resType=resType,classInfo=classInfo,lifeStageTypes=lifeStageTypes)));
}
