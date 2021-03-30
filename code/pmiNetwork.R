#which hashtags do occur together? Hashtag Co-occurence
# Calculate Pointwise Mutual Information of Hashtags a and b
# PMI(a,b) = log(a,b)/p(a)*p(b)
# p(a)= #of Tweets cointaint Hashtag a / number of Tweets (with hashtag of course)
# p(b)= #of Tweets containing Hashtag b / number of Tweets 

source("dependencies.R")
#filter out hashtags that are only used by single users 
calcHashtagPMI<-tweets.user.table%>%filter(weeksTillElection>-1)%>%select(ID,hashtags)%>%unnest()%>%
                  mutate(hashtags=cleanHashtags(hashtags))%>%
                    filter(!(hashtags%in%filterHashtags))%>%
                      pairwise_count(hashtags,ID,sort=TRUE,upper=FALSE)

#get the frequency of Tweets with hashtags but also filtered... only the one at least used by 2 users
freq.vector<-rep(nrow(tweets.user.table%>%filter(weeksTillElection>-1)%>%
                    select(ID,hashtags,hashtagCount)%>%unnest()%>% mutate(hashtags=cleanHashtags(hashtags))%>%
                       filter(!(hashtags%in%filterHashtags))%>%filter(hashtagCount>0)%>%select(ID)%>%distinct),nrow(calcHashtagPMI))

calcHashtagPMI<-cbind(calcHashtagPMI,freq.vector)

#get the frequency for every single hashtag 
freqHashtagsSingle<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                    select(ID,hashtags)%>%unnest()%>%
                      mutate(hashtags=cleanHashtags(hashtags))%>%
                      filter(!(hashtags%in%filterHashtags))%>%
                       group_by(hashtags)%>%summarise(count=n())

#join everything so we have all the counts 
calcHashtagPMI<-left_join(calcHashtagPMI,freqHashtagsSingle,by=c("item1"="hashtags"))
calcHashtagPMI<-left_join(calcHashtagPMI,freqHashtagsSingle,by=c("item2"="hashtags"))
#calc PMI
calcHashtagPMI<-calcHashtagPMI%>%mutate(PMI= log((n/freq.vector)/((count.x/freq.vector)*(count.y/freq.vector))),nPMI=(log((n/freq.vector)/((count.x/freq.vector)*(count.y/freq.vector))))/-log(n/freq.vector))


#Edgelist of PMIs
hashtagPMINetwork<-calcHashtagPMI%>%filter(nPMI>0)%>%select(item1,item2,weights=nPMI)
#How many nodes are in this network?
length(unique(c(hashtagPMINetwork$item1,hashtagPMINetwork$item2)))
# there are nrow(edges) = 102253 and 16624 nodes 
# 21.11.2017 we are off  101 239| 16164 nodes 

#build the graph and make some calculations 
PMInetwork<-graph_from_data_frame(d=hashtagPMINetwork,directed = FALSE)
pmi_tidygraph<-as_tbl_graph(PMInetwork)
pmi_tidygraph<-pmi_tidygraph%>%mutate(betweenness=centrality_betweenness(),
                                          eigenvector=centrality_eigen(),    
                                                degree=centrality_degree())


# do some community detection 
set.seed(1234)
pmi_tidygraph<-pmi_tidygraph%>%mutate(community=group_louvain(weights = weights))

# build new network and calc modularity 
PMInetwork<-graph_from_data_frame(d=hashtagPMINetwork,vertices = as.tibble(pmi_tidygraph%>%activate(nodes)),directed = FALSE)
modularity(PMInetwork,as.numeric(as.factor(V(PMInetwork)$community)))

#look at numbers of nodes per community 
node_infos<-as.tibble(pmi_tidygraph%>%activate(nodes))
numHashtagsPerCommunity<-node_infos%>%group_by(community)%>%summarise(count=n())

#get a df with hashtags topics for further analysis 
hashtag_topic_relation<-node_infos%>%select(hashtag=name,topic=community)


#################
######
# ------ Plot the top 7 largest communites i.e. hashtag co-occurence networks 
top7_communities<-numHashtagsPerCommunity%>%filter(count>1000)%>%select(community)
PMInetwork_tbl<-as_tbl_graph(PMInetwork)
PMInetwork_tbl_top7<-PMInetwork_tbl%>%filter(community%in%top7_communities$community)
PMInetworks_tbl_top7_separate<-PMInetwork_tbl_top7%>%morph(to_split,community)%>%activate(nodes)%>%
                                  top_n(15,degree)%>%
                                    crystallise()

plot_list<-lapply(PMInetworks_tbl_top7_separate$graph, function(graph) {
          ggraph(graph,layout="fr")+
            geom_edge_link(aes(alpha=weights),show.legend = FALSE)+
               geom_node_point(aes(size=degree),show.legend = FALSE)+
                geom_node_text(aes(label=name),size=6,repel=TRUE)+
                theme_graph(base_size=16)
  })
multiplot(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],plot_list[[6]],plot_list[[7]],cols=4)

#Problem with multiLouvain it results in 100 identical results 

#what is the clustering coefficient and average path lenght of the 26 topic?
# we have to adopt networkToClustering values from party to community
pmi_clustering<-networkToClusteringCoefficient(PMInetwork,nodeAttributeVector = top_communities$topic)
#What is the avgPathLength 
pmi_avgPathLength<-networkToAvgPathlength(PMInetwork,nodeAttributeVector = top_communities$topic,directed=FALSE)

