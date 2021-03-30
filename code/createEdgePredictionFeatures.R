##########################
######
### Does topical diversity and metionfrequency etc relate
#topcial diverse users mention each other much more frequently
#predict edge in mention graph
# size of a hashtag = how many users have used this hashtag 
# how many 
#number of topics in common
#smallest size etc.
# don't worry tweets.with.topics is filter >-1
# tweets. with.topics comes from topicsByHashtagCoOc.R

######
# ------ Cosine Similarity Values for each User pair 
topicsPerUserFreq<-tweets.with.topics%>%
                  group_by(twitter.handle,topic)%>%
                    summarise(topicFreq=n())


topicsPerUserFreq_spread<-topicsPerUserFreq%>%
                            spread(twitter.handle,topicFreq,fill=0)

cosineValuesUserTopics<-as.matrix(topicsPerUserFreq_spread%>%select(-topic))
cosine_similarity_users<-rownames_to_column(as.data.frame(cosine(cosineValuesUserTopics)))
cosine_similarity_users[upper.tri(cosine_similarity_users)]<- NA

cosine_similarity_users_gathered<-cosine_similarity_users%>%gather(user2,"value",2:(ncol(cosine_similarity_users)))
cosine_similarity_users_gathered<-cosine_similarity_users_gathered%>%filter(!is.na(value))


###############
####
# -------- Euclidean Distance 
topicsPerUserFreqLong<-tweets.with.topics%>%
                    group_by(twitter.handle,topic)%>%
                      summarise(topicFreq=n())%>%ungroup%>%
                         complete(twitter.handle,topic,fill=list(topicFreq=0))

euclideanDistance<-topicsPerUserFreqLong%>%pairwise_dist(item=twitter.handle,feature=topic,value=topicFreq,method="euclidean",upper=FALSE)

###############
####
# -------- Manhattan Distance 
manhattanDistance<-topicsPerUserFreqLong%>%pairwise_dist(item=twitter.handle,feature=topic,value=topicFreq,method="manhattan",upper=FALSE)


##########
# ----------------- Jaccard Coefficient
jaccardDistance<-topicsPerUserFreqLong%>%squarely(proxy::simil)(twitter.handle,topic,topicFreq,method="jaccard")


######
# -------------- Bind the distance based measures 
distanceMeasuresTopics<-left_join(cosine_similarity_users_gathered,euclideanDistance,by=c("rowname"="item2","user2"="item1"))
distanceMeasuresTopics<-left_join(distanceMeasuresTopics,manhattanDistance,by=c("rowname"="item2","user2"="item1"))
distanceMeasuresTopics<-left_join(distanceMeasuresTopics,jaccardDistance,by=c("rowname"="item1","user2"="item2"))

distanceMeasuresTopics<-distanceMeasuresTopics%>%select(user1=rowname,user2=user2,
                                       cosine_sim=value.x,
                                        jaccard_sim=value.y,
                                            euclidean_dist=distance.x,
                                              manhattan_dist=distance.y)
                                               

########
# ----- Number of Topics in common  max = 26 
#turn cosineValuesUserTopics back into a data frame 
usersCommonTopics<-topicsPerUserFreq%>%ungroup%>%pairwise_count(twitter.handle,topic)


###########
#-------------- Features based on topic size i.e. how many users posted a hahstag from this topic

topicsUsedByUsers<-tweets.with.topics%>%
                select(twitter.handle,topic)%>%  
                  group_by(twitter.handle,topic)%>%unique%>%ungroup%>%
                      group_by(topic)%>%summarise(usedByUsers=n())

topicVector<-unique(tweets.with.topics$topic)                      

listUsersCommonTopics<-listCommonTopics(as.tibble(cosineValuesUserTopics),topicVector)

predictEdgesFeatureDF<-left_join(listUsersCommonTopics,topicsUsedByUsers,by=c("topics"="topic"))

predictEdgesFeatureDF_total<-predictEdgesFeatureDF%>%select(-topics)%>%
                    group_by(user1,user2)%>%mutate(min_common_topic=min(usedByUsers),
                                                   min_common_topic_log=log(min_common_topic),
                                                   min_common_topic_inverse=1/min_common_topic,
                                                       max_common_topic=max(usedByUsers),
                                                       max_common_topic_log=log(max_common_topic),
                                                       max_common_topic_inverse=1/max_common_topic,
                                                        avg_common_topics=mean(usedByUsers),
                                                        avg_common_topics_log=log(avg_common_topics),
                                                        avg_common_topics_inverse=1/avg_common_topics,
                                                         sumInverse_common_topics=sum(1/usedByUsers),
                                                         adamicAdar_common_topics=sum(1/log(usedByUsers)))%>%
                                                          ungroup%>%select(-usedByUsers)%>%distinct
                                
# join with common topics
predictEdgesFeatureDF_total<-left_join(predictEdgesFeatureDF_total,usersCommonTopics,by=c("user1"="item1","user2"="item2"))
#join with sim measures
predictEdgesFeatureDF_total<-left_join(predictEdgesFeatureDF_total,distanceMeasuresTopics,c("user1"="user2","user2"="user1"))
# final step make NAs -> 0
predictEdgesFeatureDF_total<-predictEdgesFeatureDF_total%>%
                              mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#now we can add if user x mentioned usery | 1 or not = 0
# and mututal edges 
# mentionEdgeList comes from mentionNetAnalysis.R
mentionEdgeList_2<-mentionEdgeList

edgesToPredict<-mentionEdgeList%>%
                 left_join(mentionEdgeList_2,by=c("twitter.handle"="mentionedUsers","mentionedUsers"="twitter.handle"))
                    
edgesToPredict<-edgesToPredict%>%select(user1=twitter.handle,user2=mentionedUsers,single_edge=weight.x,mutual_edge=weight.y)%>%
                                  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, 1)))


#now lets bring both dfs together by creating a dummy_edge_df
dummy_for_edges<-edgesToPredict%>%select(user1=user2,user2=user1,single_edge,mutual_edge)
dummy_for_edges<-bind_rows(edgesToPredict,dummy_for_edges)%>%distinct
predictEdgesFeatureDF_total<-left_join(predictEdgesFeatureDF_total,dummy_for_edges,by=c("user1"="user1","user2"="user2"))

# again turn na to 0
predictEdgesFeatureDF_total<-predictEdgesFeatureDF_total%>%
                                mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

# add the missing features i.e. same party and same side of RILE scale 
add_missing_features<-predictEdgesFeatureDF_total%>%select(user1,user2)
#join with user table to get party affiliation
add_missing_features<-add_missing_features%>%left_join(user_table,by=c("user1"="twitter.handle"))
add_missing_features<-add_missing_features%>%left_join(user_table,by=c("user2"="twitter.handle"))
#calc features 
add_missing_features<-add_missing_features%>%mutate(same_party=ifelse(party.x==party.y,1,0),
                                                    RILE.x=ifelse((party.x %in% conservative),"conservative","liberal"),
                                                      RILE.y=ifelse((party.y %in% conservative),"conservative","liberal"),
                                                        diff_RILE=ifelse(RILE.x!=RILE.y,1,0),
                                                        diff_party_same_RILE=ifelse(same_party==0&diff_RILE==0,1,0))
add_missing_features<-add_missing_features%>%select(user1,user2,same_party,diff_RILE,diff_party_same_RILE)

#join with large table again
predictEdgesFeatureDF_total<-predictEdgesFeatureDF_total%>%
                                left_join(add_missing_features,by=c("user1"="user1","user2"="user2"))


