source("dependencies.R")
   
#how many tweets do contain an @mention?
#nrow(tweets.user.table%>%filter(weeksTillElection>-1,mentionCount>0))/nrow(tweets.user.table%>%filter(weeksTillElection>-1))
# 123, 598 or 33,76

tweets_with_mentions<-tweets.user.table%>%filter(weeksTillElection>-1,
                                                  mentionCount>0)%>%distinct
mean(tweets_with_mentions$mentionCount)

############# here shall be mentionEdgeList #######################
#first we select the mentionedUsers column list with twitter.handle
# than we unnest (make data long) strip the @
#filter out mentioned users which are not politicans 
mentionedUsersPerTweet<-tweets.user.table%>%
                          filter(weeksTillElection>-1)%>%
                          select(twitter.handle,party,mentionedUsers)%>%
                            filter(party%in%party_filter)%>%select(twitter.handle,mentionedUsers)%>%
                              unnest()%>%
                             mutate(mentionedUsers=str_replace(mentionedUsers,pattern="@",replacement=""))%>%
                              filter(mentionedUsers%in%user_table$twitter.handle)
                                


# now there's the question selfloops alowed? otherwise strip
mentionEdgeList<-mentionedUsersPerTweet%>%
                    group_by(twitter.handle,mentionedUsers)%>%
                        summarise(weight=n())%>%
                          filter(twitter.handle!=mentionedUsers)
                            #%>%filter(weight>3)

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

#convert To TableGraph
mentionTableGraph<-as_tbl_graph(mentionIgraph)

mentionTableGraph<-mentionTableGraph%>%mutate(page_rank=centrality_pagerank(),
                                    degree_out=centrality_degree(mode="out"),
                                    degree_in=centrality_degree(mode="in"),
                                    degree_all=centrality_degree(mode="all"))

# Plot the Mention-Network
ggraph(mentionTableGraph,layout="fr")+geom_edge_link(aes(color=node1.party),alpha=0.1,show.legend = FALSE)+
  geom_node_point(aes(color=party,size=degree_all),alpha=0.8)+
  scale_edge_color_manual(values=party_colors)+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  theme_graph(base_size = 14)+
  labs(color="Party",size="Degree")


#calculate modularity 
modularity(mentionIgraph,as.numeric(as.factor(V(mentionIgraph)$party)),weights = as.numeric(as.factor(E(mentionIgraph)$weight)))
#Q=0.45

#Which are the Top3 important Politicans of each Party according to the PageRank
top3MentionedUsersPartys<-as.tibble(mentionTableGraph%>%
                            activate(nodes))%>%group_by(party)%>%
                        dplyr::top_n(n=3,wt = page_rank)


top3TroubleMakers<-as.tibble(mentionTableGraph%>%activate(nodes))%>%group_by(party)%>%dplyr::top_n(n=3,wt = degree_out)

#### Here comes everything you can do with graph 



# Heatmap of Mentions - generate party mention edge list 
mentionEdgeListParty<-left_join(mentionEdgeList,user_table,by="twitter.handle")
mentionEdgeListParty<-left_join(mentionEdgeListParty,user_table,by=c("mentionedUsers"="twitter.handle"))
mentionEdgeListParty<-mentionEdgeListParty%>%select(mentioner=party.x,mentioned=party.y,weight=weight)%>%
                        mutate(mentioned=ifelse(mentioned=="FDP DVP","FDP",mentioned))%>%
                         group_by(mentioner,mentioned)%>%summarise(total_weight=sum(weight))%>%ungroup%>%
                           group_by(mentioner)%>%mutate(total=sum(total_weight))%>%ungroup%>%
                             mutate(weight_norm=total_weight/total)+

#Translate Party Names                              
mentionEdgeListParty<-mentionEdgeListParty%>%mutate(mentioned=ifelse(mentioned=="DIE LINKE","The Left",mentioned),
                                                    mentioned=ifelse(mentioned=="GRUENE","Greens",mentioned))%>%
                                            mutate(mentioner=ifelse(mentioner=="DIE LINKE","The Left",mentioner),
                                                   mentioner=ifelse(mentioner=="GRUENE","Greens",mentioner))
                                                
  
ggplot(data = mentionEdgeListParty, aes(x = mentioner, y = mentioned))+
  geom_tile(aes(fill = weight_norm))+
  scale_fill_gradient(low="white",high="grey25",name="% of Mentions",breaks=c(0.05,0.25,0.5,0.70),labels=c("5","25","50","70"),guide = guide_colorbar(direction = "horizontal",title.position="top"))+
  geom_text(aes(label=round(weight_norm,3)),size=3)+xlab("Mentioner")+ylab("Mentioned")+theme_ipsum_rc(base_size=12,axis_title_size = 12)+theme(legend.position="bottom")



#######
#Whats the odds ratio of members mentioning each other 
#conservative<-c("AfD","CDU","CSU","FDP")
chi_square_mentionEdgelist<-mentionEdgeListParty%>%
                                mutate(mentioner=ifelse((mentioner %in% conservative),"conservative","liberal"),
                                       mentioned=ifelse((mentioned %in% conservative),"conservative","liberal"))
chi_square_mentionEdgelist<-chi_square_mentionEdgelist%>%group_by(mentioner,mentioned)%>%
                              summarise(sum_mentions=sum(total_weight))
chi_square_mention_matrix<-matrix(c(7362,5637,30431,5273),2,2)
dimnames(chi_square_mention_matrix) <-  list(c("Own", "Others"), c("Conservative", "Liberal"))
chisq.test(chi_square_mention_matrix)
chi_square_mention_matrix[1,1]




######
#lets calculate how many hops each node is apart from another node
distancesMentionNet<-rownames_to_column(as.data.frame(distances(mentionIgraph,mode = "out")))
distancesMentionNet<-distancesMentionNet%>%gather(user2,"value",2:(ncol(distancesMentionNet)))
distancesMentionNet<-distancesMentionNet%>%filter(value!=0)


