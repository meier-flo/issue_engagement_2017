multiLouvain<-function(igraphObject,n){
  #igraphObject to take the vertex attribute from
  # n is number os runs 
  require(igraph)
  df<-data.frame(hashtags=vertex_attr(igraphObject, "name"),stringsAsFactors = FALSE)
  for(i in 1:n){
    new_community<-paste("community",i,sep="_")
    df[[new_community]]<-membership(cluster_louvain(igraphObject))
  }
  return(df)  
}