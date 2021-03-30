networkToGiniValue<-function(iGraphObject,nodeAttributeVector,mode="in"){
  require(igraph)
  require(ineq)
  require(dplyr)

      gini.df<-tibble(
        gini=vector("double"),
        party=vector("character")
      )
      
      for(i in seq_along(nodeAttributeVector)){
        giniValue<-Gini(degree(iGraphObject,v=V(iGraphObject)$party==nodeAttributeVector[i],mode))
        dummy.df<-tibble(
          gini=giniValue,
          party=nodeAttributeVector[i]
        )
        gini.df<-bind_rows(gini.df,dummy.df)
      }
return(gini.df)
}