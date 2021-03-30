listCommonTopics<-function(data,topicVector){
  return(do.call(rbind,lapply(seq_along(data),function(i){
    print(i)
    return(do.call(rbind,lapply(seq_along(data),function(j) {
      if(j>i){
        if(colnames(data[i])!=colnames(data[j])){
          compareUsers<-data.frame(topicVector,
                                   user1=data[,i],
                                   user2=data[,j],
                                   stringsAsFactors = FALSE)
          commonTopics <- subset(compareUsers,compareUsers[,2]!=0&compareUsers[,3]!=0)
          if(nrow(commonTopics)==0){ 
            user1<-rep(colnames(data[i]),1)
            user2<-rep(colnames(data[j]),1)
            topics<-0
          }else{
            user1<-rep(colnames(data[i]),nrow(commonTopics))
            user2<-rep(colnames(data[j],nrow(commonTopics)))
            topics<-commonTopics[,1]
          }
          pairwise_df<-data.frame(user1=user1,user2=user2,topics=topics,stringsAsFactors = FALSE)
          return(pairwise_df)
        }
      }
      
    })))
    
  })))
}