edgeListToHeatmapDF<-function(edgeList,userPartyDF,colName1,colName2){
require(dplyr)
require(lazyeval)
  edgeList[[1]]<-userPartyDF[[2]][match(edgeList[[1]],userPartyDF[[1]])]
  edgeList[[2]]<-userPartyDF[[2]][match(edgeList[[2]],userPartyDF[[1]])]
  
  heatmapDF<-edgeList%>%group_by_(colName1,colName2)%>%summarise_(weight="n()")%>%
    ungroup%>%group_by_(colName1)%>%mutate_(total= interp(~sum(weight),weight=as.name("weight")),weight_norm=interp(~weight/total),as.name("weight"),as.name("total"))%>%
    ungroup%>%select_(colName1,colName2,as.name("weight_norm"))
  
  return(heatmapDF)
}