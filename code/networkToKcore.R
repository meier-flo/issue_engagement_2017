networkToKcore<-function(iGraphObject,nodeAttributeVector,mode="all"){
  #nodeAttributeVektor only has to be list of partys
  require(igraph)
  require(dplyr)
  kcore.df<-tibble(
    k_core=vector("double"),
    party=vector("character")
  )
  for(i in seq_along(nodeAttributeVector)){
    subgraph<-induced.subgraph(iGraphObject,which(V(iGraphObject)$party==nodeAttributeVector[i]))   
    kcore<-coreness(subgraph,mode)
    dummy.df<-tibble(
      k_core=kcore,
      party=rep(nodeAttributeVector[i],length(kcore))
    )
    kcore.df<-bind_rows(kcore.df,dummy.df)
  }
  return(kcore.df)
}