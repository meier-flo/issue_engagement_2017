# the idea would be to draw a edge between politicans if
# they use the same hashtag 

# we need the scripts and packages
source("dependencies.R")
# hashtagsPerUserunique
# wir betrachten NICHT die Häufigkeit der Nutzung... nur OB überhaupt einmal der gleiche Hashtag verwendet wurde
hashtagsPerUserUnique<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                        select(twitter.handle,hashtags)%>%
                           unnest()%>%
                mutate(hashtags=cleanHashtags(hashtags))%>%
                #filter(!(hashtags%in%filterHashtags))%>%
                 filter(!(hashtags%in%filterHashtags))%>%
                 group_by(twitter.handle,hashtags)%>%unique()%>%ungroup


source("makeEdgelistFromUserHashtags.R")
hashtagEdgelist<-makeEdgelistFromUserHashtags(hashtagsPerUserUnique)

#How many share a Hashtag 
nrow(hashtagEdgelist%>%filter(weight<5))/nrow(hashtagEdgelist)
#etwa die Hälfte (45%) nicht mal 5 gemeinsame Hashtags 
hashtagEdgelistFiltered<-hashtagEdgelist%>%filter(weight>49)


nodeFilterHashtagNetwork<-unique(c(hashtagEdgelistFiltered$V1,hashtagEdgelistFiltered$V2))

hashtagNetworkNodes<-user_table%>%select(twitter.handle,party)%>%
                        filter(twitter.handle%in%nodeFilterHashtagNetwork)%>%
                          mutate(party=ifelse(party=="FDP DVP","FDP",party))

#make igraph-Object
hashtagIgraph<-graph_from_data_frame(d=hashtagEdgelistFiltered,vertices=hashtagNetworkNodes,directed=FALSE)

#Plot igraph-Object with ggraph
#ggraph(hashtagIgraph,layout="fr")+geom_edge_link(alpha=0.2)+
#  geom_node_point(aes(color=party))+scale_color_manual(values=party_colors)+theme_graph()


#convert to tbl graph
hashtag_tidygraph<-as_tbl_graph(hashtagIgraph)

# add another column to the edgelist which shows the color 
hashtag_tidygraph<-hashtag_tidygraph%>%activate(edges)%>%
  mutate(edgecolor=ifelse(.N()$party[from]==.N()$party[to],.N()$party[from],"NO"))

ggraph(hashtag_tidygraph,layout="kk")+geom_edge_link(aes(color=edgecolor),alpha=0.2)+
  geom_node_point(aes(color=party))+
  scale_edge_color_manual(values=party_colors,guide="none")+
  scale_color_manual(values=party_colors)+theme_graph()

#Calc Modularity of hashtagNetwork with party sub-networks
modularity(hashtagIgraph,as.numeric(as.factor(V(hashtagIgraph)$party)))


# do community detection with louvain 
hashtag_tidygraph<-hashtag_tidygraph%>%activate(nodes)%>%mutate(community=as.factor(group_louvain()))


testi<-as.tibble(hashtag_tidygraph%>%activate(nodes))
testi2<-as.tibble(hashtag_tidygraph%>%activate(edges))
bla<-testi%>%group_by(community)%>%summarise(group_size=n())


hashtagIgraph<-graph_from_data_frame(d=hashtagEdgelistFiltered,vertices=as.tibble(hashtag_tidygraph%>%activate(nodes)),directed=FALSE)
modularity(hashtagIgraph,as.numeric(as.factor(V(hashtagIgraph)$party)),weights = as.numeric(as.factor(E(hashtagIgraph)$weight)))


