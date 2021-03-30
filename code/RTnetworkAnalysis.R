#get the tweet-RT-relation table 
# CAUTION sourceID is the RT ID
# tweetID is the ID of the original Tweet 
source("dependencies.R")

#####---------- get data from db --------------------
tweet.con<-dbConnect(SQLite(), dbname="tweets.sqlite")
dbListTables(tweet.con)
tweetRtRelation<-dbGetQuery(tweet.con,"SELECT CAST(sourceId as TEXT) as rtID, 
                   CAST(tweetId as TEXT) as originalTweetID
                                  FROM sources_to_tweets") 
dbDisconnect(tweet.con)
####--------------------------------------------------

# if the ID is a RT ID than it's a RT ... 
#join with the relation table and then again with user with X RT's Y

rtJoinHelper_RTNetwork<-tweets.user.table%>%select(twitter.handle,party,ID)
rtEdgeList<-left_join(rtJoinHelper_RTNetwork,tweetRtRelation,by=c("ID"="rtID"))
rtEdgeList<-left_join(rtEdgeList,rtJoinHelper_RTNetwork,by=c("originalTweetID"="ID"))
# filter all the NA's
# Problem: this gives us only RT from politicans 
# RTs of users not politicans/in our list are not contained
# after talking to david this doesn't seem to be a problem 

rtEdgeList<-filter(rtEdgeList,(originalTweetID!="NA"&twitter.handle.y!="NA"))


#there are still three that get duplicated whoever knows why
#so we need to do distinct
rtEdgeList<-rtEdgeList%>%distinct


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
#convert to tbl graph
rtTableGraph<-as_tbl_graph(rtIgraph)

#calculate some centrality measures e.g. PageRank etc. 
rtTableGraph<-rtTableGraph%>%mutate(page_rank=centrality_pagerank(),
                                      betweeness=centrality_betweenness(),
                                        #closeness=centrality_closeness(),
                                          degree_out=centrality_degree(mode="out"),
                                            degree_in=centrality_degree(mode="in"),
                                              degree=centrality_degree(mode='all'))
rt_node_measures<-as.tibble(rtTableGraph)

#Plot
rt_graph<-ggraph(rtTableGraph,layout="fr")+geom_edge_link(aes(color=node1.party),alpha=0.1)+
  geom_node_point(aes(color=party,size=degree_in),alpha=0.8)+
    scale_edge_color_manual(values=party_colors_de,guide="none")+
       scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
          theme_graph(base_size = 14)+labs(color="Party",size="In-Degree")+
            ggtitle('RT')

#Which are the Top3 important Politicans of each Party according to the PageRank
top3InfluencersPartys<-as.tibble(rtTableGraph%>%activate(nodes))%>%group_by(party)%>%dplyr::top_n(n=3,wt = page_rank)
#What is the modularity of the RT Network?
modularity(rtIgraph,as.numeric(as.factor(V(rtIgraph)$party)),weights = as.numeric(as.factor(E(rtIgraph)$weight)))
#Q=0.73
reciprocity(rtIgraph)
#0.24
edge_density(rtIgraph)

#make a Lorenz Curve
rt_lorenz<-networkToLorenzDF(rtIgraph,unique(rtNetworkNodes$party))

ggplot(rt_lorenz,mapping=aes(x=proportion*100,y=degree*100,color=party))+
            geom_line(aes(color=party),size=1.5)+
            geom_abline(intercept = 0, slope = 1,linetype=2)+
                xlab("Politicians [%]")+ylab("RT In-Degree Volume [%]")+
                  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                   labs(color="")+theme_ipsum_rc(base_size=12,axis_title_size = 12)

#Gini Values RT  in and out            
rt_gini_in<-networkToGiniValue(rtIgraph,unique(rtNetworkNodes$party))
rt_gini_out<-networkToGiniValue(rtIgraph,unique(rtNetworkNodes$party),mode='out')

#do a GINI average degree thingy like from stars and tides ... first IN-DEGREE
rt_avg_degree_gini_parties<-rt_node_measures%>%
                                    group_by(party)%>%
                                    summarise(mean_degree=mean(degree))

rt_avg_degree_gini_parties<-cbind(rt_avg_degree_gini_parties,rt_gini_in=rt_gini_in$gini)
rt_avg_degree_gini_parties<-cbind(rt_avg_degree_gini_parties,rt_gini_out=rt_gini_out$gini)


a<-ggplot(data=rt_avg_degree_gini_parties,mapping=aes(x=mean_degree,y=rt_gini_in))+
  geom_point(aes(color=party,size=4))+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  theme_ipsum_rc(base_size=12,axis_title_size = 12,plot_title_size = 12)+
  scale_x_continuous(limits=c(1,25))+
  scale_y_continuous(limits=c(0.4,1))+
  xlab("Average Degree")+ylab("Gini")+labs(title='a) RT In-Degree')+guides(size=FALSE)+
  labs(color="")

b<-ggplot(data=rt_avg_degree_gini_parties,mapping=aes(x=mean_degree,y=rt_gini_out))+
  geom_point(aes(color=party,size=4))+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  theme_ipsum_rc(base_size=12,axis_title_size = 12,plot_title_size = 12)+
  scale_x_continuous(limits=c(1,25))+
  scale_y_continuous(limits=c(0.4,1))+
  xlab("Average Degree")+ylab("Gini")+labs(title='RT Out-Degree')+guides(size=FALSE)+
  labs(color="")
       
# Clustering Coefficient 
rt_transitivity<-networkToClusteringCoefficient(rtIgraph,unique(rtNetworkNodes$party))
# Average Path Length 
rt_avgPathlength<-networkToAvgPathlength(rtIgraph,unique(rtNetworkNodes$party))
# here comes K-Core or shell index 
rt_kcore<-networkToKcore(rtIgraph,unique(rtNetworkNodes$party))
rt_kcore<-rt_kcore%>%group_by(party)%>%summarise(max_kcore=max(k_core),avg_kcore=mean(k_core))

#### What about a Heatmap that also visualises the partisan affiliation/homophily 
#We need the party 
#----------- party edge list 

rtEdgeListParty<-rtEdgeList%>%select(party.x,party.y)%>%group_by(party.x,party.y)%>%summarise(weight=n())%>%
  arrange(weight)%>%select(from=party.x,to=party.y,weight)%>%ungroup



#makeHeatmap
rtPartyHeatmap<-rtEdgeListParty%>%group_by(from)%>%
  mutate(total=sum(weight))%>%ungroup%>%mutate(weight_norm=weight/total)

ggplot(data = rtPartyHeatmap, aes(x = from, y = to))+
  geom_tile(aes(fill = weight_norm))+
  scale_fill_viridis(name="% RTs",breaks=c(0.1,0.5,0.95),labels=c("10","50","95"),guide = guide_colorbar(direction = "horizontal",title.position="top"))+
  theme_bw(base_size = 16)+xlab("Retweeter")+ylab("Retweeted")+
  theme(legend.position="bottom")




##################
# retweet-response time 
# tweets that get retweeted long after they have been sent are more likely
# to contain 

tweets_time<-tweets.user.table%>%select(ID,createdAT)

nrow(rtEdgeList%>%distinct)

rt_response_time<-left_join(rtEdgeList,tweets_time,by=c('ID'='ID'))
rt_response_time<-left_join(rt_response_time,tweets_time,by=c('originalTweetID'='ID'))

rt_response_time<-rt_response_time%>%
                    mutate(response_time=as.numeric(difftime(createdAT.x,createdAT.y, units = "secs")))%>%distinct


# We use the RT_Repsonse time later on 
rt_response_time<-rt_response_time%>%mutate(relation=ifelse(party.x==party.y,"WITHIN","ACROSS"))


#what is the number of cross retweets 
table(rt_response_time$relation)


rt_stats<-rtEdgeList%>%select(ID,party.x)%>%group_by(party.x)%>%
            summarise(count_per_party=n())%>%ungroup%>%
               mutate(ratio=round((count_per_party/nrow(rtEdgeList)*100),2))

#This is the RT Network
write_csv(rtNetworkNodes,'rt_network_nodes.csv')
write_csv(rtEdgeListUser,'rt_network_edgelist.csv')


#This is the QuoteNetwork
