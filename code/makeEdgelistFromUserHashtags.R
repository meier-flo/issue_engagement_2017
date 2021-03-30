makeEdgelistFromUserHashtags<-function(userHashtagUniqueTable){
  require(dplyr)
  
  hashtagEdgelist<-data.frame(V1=character(),
                              V2=character(),
                              hashtag=character(), stringsAsFactors = FALSE)
  hashtags.unique<-unique(userHashtagUniqueTable[[2]])
  hashtags.seen<-character()
  for(i in seq_along(hashtags.unique)){
      user.table<-userHashtagUniqueTable%>%filter(hashtags==hashtags.unique[i])
    if(!user.table$hashtags[1]%in%hashtags.seen){
      if((nrow(user.table)>1)){
        user.table.expanded<-expand.grid(user.table[[1]],user.table[[1]],stringsAsFactors = FALSE)
        user.table.expanded<-t(apply(user.table.expanded,1,sort))
        user.table.expanded<-as.data.frame(user.table.expanded[!duplicated(user.table.expanded),],stringsAsFactors=FALSE)
        hashtag.table<-data.frame(hashtag=rep(hashtags.unique[i],nrow(user.table.expanded)),stringsAsFactors = FALSE)
        user.table.expanded<-bind_cols(user.table.expanded,hashtag.table)
        hashtagEdgelist<-bind_rows(hashtagEdgelist,user.table.expanded)
        hashtags.seen[i]<-hashtags.unique[i]
        #message("Hashtag is new: It is: ",hashtags.seen[i])
        }else{next}
    }else{next}
  }
  hashtagEdgelist<-hashtagEdgelist%>%group_by(V1,V2)%>%summarise(weight=n())%>%ungroup
  hashtagEdgelist<-hashtagEdgelist%>%filter(V1!=V2)
  return(hashtagEdgelist)
}



# test_function<-function(userHashtagTable){
# user.table<-list()
# hashtags.seen<-character()
# hashtags<-unique(userHashtagTable[[2]])
# for(i in seq_along(hashtags)){
#   print(userHashtagTable$hashtags[i]%in%hashtags.seen)
#   if(userHashtagTable$hashtags[i]%in%hashtags.seen){print("Hashtag already seen")}
#   else{
# user.table[[i]]<-userHashtagTable%>%filter(hashtags==hashtags[i])
# hashtags.seen[i]<-hashtags[i]
# print(hashtags[i])
# print(hashtags.seen[i])
# print(length(hashtags.seen))
#   }
# }
# return(user.table)
# }
