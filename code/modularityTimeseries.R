#first filter tweets.user.data with only tweets till election
modularity.timeseries.data<-tweets.user.table%>%filter(weeksTillElection>-1)

calcModularityTimeseriesRT<-function(data,user_table){
  
  modularityRT.df<-tibble(
    time_to_election=vector("numeric"),
    modularity_value=vector("double"),
    network=vector("character")
  )


tweet.con<-dbConnect(SQLite(), dbname="tweets.sqlite")
dbListTables(tweet.con)
tweetRtRelation<-dbGetQuery(tweet.con,"SELECT CAST(sourceId as TEXT) as rtID, 
                            CAST(tweetId as TEXT) as originalTweetID
                            FROM sources_to_tweets") 
dbDisconnect(tweet.con)
####--------------------------------------------------

# if the ID is a RT ID than it's a RT ... 
#join with the relation table and then again with user with X RT's Y

for(i in seq_along(unique(data$weeksTillElection))){

data.filtered<-data%>%filter(weeksTillElection==i-1)
  
rtJoinHelper_RTNetwork<-data.filtered%>%select(twitter.handle,party,ID)
rtEdgeList<-left_join(rtJoinHelper_RTNetwork,tweetRtRelation,by=c("ID"="rtID"))
rtEdgeList<-left_join(rtEdgeList,rtJoinHelper_RTNetwork,by=c("originalTweetID"="ID"))
rtEdgeList<-filter(rtEdgeList,(originalTweetID!="NA"&twitter.handle.y!="NA"))


#------------ user edge list
rtEdgeListUser<-rtEdgeList%>%select(twitter.handle.x,twitter.handle.y)%>%
  group_by(twitter.handle.x,twitter.handle.y)%>%summarise(weight=n())%>%
  arrange(weight)%>%ungroup%>%select(from=twitter.handle.x,to=twitter.handle.y,weight)%>%
  filter(from!=to)

#make a filter string to drop nodes which don't rt or get rted
nodeFilterRTNetwork<-unique(c(rtEdgeListUser$from,rtEdgeListUser$to))

#get Nodes and filter 
rtNetworkNodes<-user_table%>%select(twitter.handle,party)%>%
  filter(twitter.handle%in%nodeFilterRTNetwork)%>%
  mutate(party=ifelse(str_detect(party,"FDP.+"),"FDP",party))

#make an igraph-object
rtIgraph<-graph_from_data_frame(d=rtEdgeListUser,vertices=rtNetworkNodes,directed=TRUE)

modularity_value<-modularity(rtIgraph,as.numeric(as.factor(V(rtIgraph)$party)),weights = as.numeric(as.factor(E(rtIgraph)$weight)))
  
  dummy.df<-tibble(
    time_to_election=i,
    modularity_value=modularity_value,
    network="RT"
    )
  modularityRT.df<-bind_rows(modularityRT.df,dummy.df)
  }
  return(modularityRT.df)
}


calcModularityTimeseriesMention<-function(data,user_table){
  
  modularityMention.df<-tibble(
    time_to_election=vector("numeric"),
    modularity_value=vector("double"),
    network=vector("character")
  )
  
  for(i in seq_along(unique(data$weeksTillElection))){
   
    print(i)
    data.filtered<-data%>%filter(weeksTillElection==i-1)
    head(data.filtered)    
    
    mentionedUsersPerTweet<-data.filtered%>%
      select(twitter.handle,party,mentionedUsers)%>%
      filter(party%in%party_filter)%>%select(twitter.handle,mentionedUsers)%>%
      unnest()%>%
      mutate(mentionedUsers=str_replace(mentionedUsers,pattern="@",replacement=""))%>%
      filter(mentionedUsers%in%user_table$twitter.handle)
    
    
    # now there's the question selfloops alowed? otherwise strip
    mentionEdgeList<-mentionedUsersPerTweet%>%
      group_by(twitter.handle,mentionedUsers)%>%
      summarise(weight=n())%>%
      filter(twitter.handle!=mentionedUsers)%>%filter(weight>1)
    
    #make a filter string to drop nodes which don't mention others/get mentioned
    nodeFilterMentionNetwork<-unique(c(mentionEdgeList$twitter.handle,mentionEdgeList$mentionedUsers))
    
    #get Nodes and filter 
    mentionNetworkNodes<-user_table%>%select(twitter.handle,party)%>%
      filter(twitter.handle%in%nodeFilterMentionNetwork)%>%
      mutate(party=ifelse(party=="FDP DVP","FDP",party))%>%
      filter(party%in%party_filter)
    
    table(mentionNetworkNodes$party)
    # DAS MÜSSTE EIGENTLICH NICHT MEHR SEIN
    mentionEdgeList<-mentionEdgeList%>%filter(twitter.handle%in%mentionNetworkNodes$twitter.handle,mentionedUsers%in%mentionNetworkNodes$twitter.handle)
    
    
    #build a Mention Network from the Edgelist                
    mentionIgraph<-graph_from_data_frame(d=mentionEdgeList,vertices=mentionNetworkNodes,directed=TRUE) 
    
   
    modularity_value<-modularity(mentionIgraph,as.numeric(as.factor(V(mentionIgraph)$party)),weights = as.numeric(as.factor(E(mentionIgraph)$weight)))
    
    dummy.df<-tibble(
      time_to_election=i,
      modularity_value=modularity_value,
      network="Mention"
    )
    modularityMention.df<-bind_rows(modularityMention.df,dummy.df)
  }
  return(modularityMention.df)
}

testi<-calcModularityTimeseriesRT(modularity.timeseries.data,user_table)
testi.1<-calcModularityTimeseriesMention(modularity.timeseries.data,user_table)

modularity.timeseries<-bind_rows(testi,testi.1)
#change the weeks appropriately 
modularity.timeseries<-modularity.timeseries%>%
                        mutate(time_to_election=time_to_election-1)



#ggplot(data=modularity.timeseries,mapping=aes(x=time_to_election,y=modularity_value,color=network))+
#              geom_line(aes(color=network),size=1.5)+scale_x_reverse(limits=c(35,0),breaks=c(seq(35,0,-5)),labels=c(seq(35,0,-5)))+
#                labs(color="Netzwerk")+scale_colour_viridis(discrete=TRUE,end=0.7)+
#                theme_bw(base_size = 16)+ylab("Q Modularity")+xlab("Wochen bis zur Wahl")
               
ggplot(data=modularity.timeseries,mapping=aes(x=time_to_election,
                                                y=modularity_value,
                                                  color=network))+
        geom_smooth(span=0.8,se=FALSE)+
            scale_x_reverse(limits=c(37,-1),breaks=c(seq(37,-1,-5)),labels=c(seq(37,-1,-5)))+
                scale_colour_viridis(discrete=TRUE,end=0.7)+
                   ylab("Q-Modularity")+xlab("Weeks Till Election")+labs(color="")+
                     theme(legend.position="bottom")+
                        theme_ipsum_rc(base_size=12,axis_title_size = 12)+
                          theme(legend.position="bottom")


