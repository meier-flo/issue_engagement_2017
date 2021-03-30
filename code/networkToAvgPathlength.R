networkToAvgPathlength<-function(iGraphObject,nodeAttributeVector,type="global",directed=TRUE){
  #nodeAttributeValue = party list
  #global clustering coefficient returns a single value 
  #local would return  
  require(igraph)
  require(dplyr)
  avgPathlength.df<-tibble(
   avgPathlength=vector("double"),
    community=vector("numeric")
  )
  
  for(i in seq_along(nodeAttributeVector)){
    subgraph<-induced.subgraph(iGraphObject,which(V(iGraphObject)$community==nodeAttributeVector[i]))
    avgPathlength<-average.path.length(subgraph,directed=directed)
    dummy.df<-tibble(
     avgPathlength=avgPathlength,
      community=nodeAttributeVector[i]
    )
    avgPathlength.df<-bind_rows(avgPathlength.df,dummy.df)
  }
  return(avgPathlength.df)
}