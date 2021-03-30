#Based on Topics not Hashtags
issuesPerUser<-tweets.with.topics%>%select(ID,twitter.handle,party,topic,unique_topics)%>%distinct%>%
                  select(twitter.handle,topic)%>%
                    group_by(twitter.handle,topic)%>%
                      mutate(issueFreq=n())%>%unique%>%
                         ungroup

issuesPerUser_spread<-issuesPerUser%>%spread(twitter.handle,issueFreq,fill=0)


cosineValuesUsersIssues<-as.matrix(issuesPerUser_spread%>%select(-topic))
cosine_similarity_users<-rownames_to_column(as.data.frame(cosine(cosineValuesUsersIssues)))
cosine_similarity_users[upper.tri(cosine_similarity_users)]<- NA

cosine_similarity_users_gathered<-cosine_similarity_users%>%gather(user2,"value",2:(ncol(cosine_similarity_users)))
cosine_similarity_users_gathered<-cosine_similarity_users_gathered%>%filter(!is.na(value))



# Based on Hashtags not Topics
# ------
# cosine similartiy between users for building a similarity network 
source("dependencies.R")
# we can do it with all tweets or the ones tweet.with.topics
hashtagsPerUser<-tweets.with.topics%>%
                  select(twitter.handle,hashtags)%>%
                    group_by(twitter.handle,hashtags)%>%
                      mutate(hashtagsFreqLog=n())%>%unique%>%
                        ungroup
  
hashtagsPerUser_spread<-hashtagsPerUser%>%spread(twitter.handle,hashtagsFreqLog,fill=0)


cosineValuesUsersHashtags<-as.matrix(hashtagsPerUser_spread%>%select(-hashtags))
cosine_similarity_users<-rownames_to_column(as.data.frame(cosine(cosineValuesUsersHashtags)))
cosine_similarity_users[upper.tri(cosine_similarity_users)]<- NA

cosine_similarity_users_gathered<-cosine_similarity_users%>%gather(user2,"value",2:(ncol(cosine_similarity_users)))
cosine_similarity_users_gathered<-cosine_similarity_users_gathered%>%filter(!is.na(value))

   

