networkToClusteringCoefficient<-function(iGraphObject,nodeAttributeVector,type="global",weights=NULL){
#nodeAttributeValue = party list
#global clustering coefficient returns a single value 
#local would return a vector with values which one could simple take the average as done by Aragon
  require(igraph)
  require(dplyr)
  clustering.df<-tibble(
    clusteringCoefficient=vector("double"),
    party=vector("character")
  )
  
  for(i in seq_along(nodeAttributeVector)){
    subgraph<-induced.subgraph(iGraphObject,which(V(iGraphObject)$party==nodeAttributeVector[i]))
    clusteringCoefficient<-transitivity(subgraph,type=type,weights=weights)
    dummy.df<-tibble(
      clusteringCoefficient=clusteringCoefficient,
      party=nodeAttributeVector[i]
    )
    clustering.df<-bind_rows(clustering.df,dummy.df)
  }
  return(clustering.df)
}


