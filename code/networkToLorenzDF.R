networkToLorenzDF<-function(iGraphObject,nodeAttributeVector,mode="in"){
#nodeAttributeVektor only has to be list of partys
require(igraph)
require(ineq)
require(dplyr)
  degree.df<-tibble(
                proportion=vector("double"),
                degree=vector("double"),
                party=vector("character")
              )
  for(i in seq_along(nodeAttributeVector)){
    LcData<-Lc(degree(iGraphObject,v=V(iGraphObject)$party==nodeAttributeVector[i],mode))
      dummy.df<-tibble(
        proportion=LcData$p,
        degree=LcData$L,
        party=rep(nodeAttributeVector[i],length(LcData$p))
          )
    degree.df<-bind_rows(degree.df,dummy.df)
    }
  return(degree.df)
}

