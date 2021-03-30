#we need merge_quote_URL_RT_Final from  turnQuotesToRTs.R



############ here we can also get the quote response time 
#tweets time from RTNetworkAnalysis
quote_response_time<-left_join(merge_quote_URL_RT_Final,tweets_time,by=c('ID'='ID'))
quote_response_time<-left_join(quote_response_time,tweets_time,by=c('quoted_ID'='ID'))
#get rid of the other na leaves us with 4579 complete cases 
quote_response_time<-quote_response_time%>%
                        filter(!is.na(twitter.handle.x))%>%distinct



#calculate RT response time 
# check whether this is whithin or across party
quote_response_time<-quote_response_time%>%
  mutate(response_time=as.numeric(difftime(createdAT.x,createdAT.y, units = "secs")))
quote_response_time<-quote_response_time%>%
  filter(!is.na(twitter.handle.x))%>%
  mutate(relation=ifelse(party.x==party.y,"WITHIN","ACROSS"))



table(quote_response_time$relation)
# 30% of the quotes are not within party that is 25% more 

# is there a party that uses the feature significantly more often then the other parties
quote_usage<-quote_response_time%>%select(party.x)%>%group_by(party.x)%>%
                          summarise(usage=n())%>%ungroup%>%mutate(total=sum(usage),relation=usage/total)


#make one plot for all 4 RT Quote across within
quote_rt_response_time<-quote_response_time%>%
  mutate(type="-Quote")%>%
  unite(col = relation_type,relation,type,sep = '')%>%
  select(response_time,relation_type)

quote_rt_response_time_1<-rt_response_time%>%mutate(response_time=as.numeric(response_time),type="-RT")%>%
  unite(col = relation_type,relation,type,sep = '')%>%
  select(response_time,relation_type)

quote_rt_response_time<-bind_rows(quote_rt_response_time,quote_rt_response_time_1)


# Do the ECDF Plot for the RT Response Time 
ggplot(data=quote_rt_response_time,aes(x=response_time/(60*60),color=as.factor(relation_type)))+
  stat_ecdf(size=2)+scale_x_log10(labels=comma)+scale_color_viridis(discrete=TRUE)+
        theme_ipsum_rc(base_size=30,axis_title_size = 32)+theme(legend.position="bottom")+
             xlab("RT-Response-Time in Hours [log]")+ylab("ECDF")+labs(color="")

#summary of mean,median sd etc. for 
summary_table<-quote_rt_response_time%>%
  group_by(relation_type)%>%
  summarise(mean=mean(response_time)/(60*60),
            median=median(response_time)/(60*60),
            max=max(response_time)/(60*60),
            sd=sd(response_time)/(60*60))


#nice fancy latex output?
kable(summary_table, "latex", booktabs = T)

#do some significane testing by looking at the confidene intrval bootstrapped difference of means
# first with RT and than with Quote 

quote_rt_response_time_1<-quote_rt_response_time_1%>%
  mutate(response_time=response_time/(60*60),relation=as.factor(relation_type))

sampling_distribution <- quote_rt_response_time_1 %>%
  specify(response_time ~ relation_type) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("WITHIN-RT", "ACROSS-RT"))

conf_int <- sampling_distribution %>% 
  pull(stat) %>% 
  quantile(probs = c(0.025, 0.975)) 

conf_int

############### 
# do the same for quote RTs 

quote_rt_response_time_2<-quote_rt_response_time%>%
  mutate(response_time=response_time/(60*60),relation_type=str_remove(relation_type,".*-"),relation_type=as.factor(relation_type))

sampling_distribution <- quote_rt_response_time_2 %>%
  specify(response_time ~ relation_type) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("RT", "Quote"))

conf_int <- sampling_distribution %>% 
  pull(stat) %>% 
  quantile(probs = c(0.025, 0.975)) 

conf_int




# now lets do some network analysis on the quote network

quote_edgelist_user<-merge_quote_URL_RT_Final%>%
                        select(twitter.handle.x,twitter.handle)%>%
                              group_by(twitter.handle.x,twitter.handle)%>%
                                    summarise(weight=n())%>%arrange(weight)%>%
                                        ungroup%>%select(from=twitter.handle.x,to=twitter.handle,weight)%>%
                                          filter(from!=to)


node_filter_quote_network<-unique(c(quote_edgelist_user$from,quote_edgelist_user$to))

#make a filter string to drop nodes which don't rt or get rted
nodeFilterRTNetwork<-unique(c(rtEdgeListUser$from,rtEdgeListUser$to))
quote_rt_filter<-unique(c(nodeFilterRTNetwork,node_filter_quote_network))

quote_network_nodes<-user_table%>%select(twitter.handle,party)%>%
  filter(twitter.handle%in%quote_rt_filter)%>%
  mutate(party=ifelse(str_detect(party,"FDP.+"),"FDP",party))
# get all the users who RT
#make an igraph-object
quote_igraph<-graph_from_data_frame(d=quote_edgelist_user,vertices=quote_network_nodes,directed=TRUE)
#convert to tbl graph
quote_table_graph<-as_tbl_graph(quote_igraph)

#calculate some centrality measures e.g. PageRank etc. 
quote_table_graph<-quote_table_graph%>%mutate(page_rank=centrality_pagerank(),
                                    betweeness=centrality_betweenness(),
                                    #closeness=centrality_closeness(),
                                    degree_out=centrality_degree(mode="out"),
                                    degree_in=centrality_degree(mode="in"),
                                    degree=centrality_degree(mode='all'))
quote_node_measures<-as.tibble(quote_table_graph)

#Plot
quote_graph<-ggraph(quote_table_graph,layout="fr")+geom_edge_link(aes(color=node1.party),alpha=0.1)+
  geom_node_point(aes(color=party,size=degree_in),alpha=0.8)+
  scale_edge_color_manual(values=party_colors_de,guide="none")+
  scale_color_manual(values=party_colors_de)+theme_graph(base_size = 14)+labs(color="Party",size="In-Degree")+
  ggtitle('Quote RT')


#modularity of the quote_network
modularity(quote_igraph_filtered,as.numeric(as.factor(V(quote_igraph_filtered)$party)),weights = as.numeric(as.factor(E(quote_igraph_filtered)$weight)))
#Q=0.45
#reciprocity
reciprocity(quote_igraph_filtered)
#=0.12
edge_density(quote_igraph)

#make a Lorenz Curve for Indegree
quote_lorenz_indegree<-networkToLorenzDF(quote_igraph,unique(quote_network_nodes$party))

#make a Lorenz Curve for Outdegree
quote_lorenz_outdegree<-networkToLorenzDF(quote_igraph,unique(quote_network_nodes$party),mode='out')


ggplot(quote_lorenz_outdegree,mapping=aes(x=proportion*100,y=degree*100,color=party))+
  geom_line(aes(color=party),size=1.5)+
  geom_abline(intercept = 0, slope = 1,linetype=2)+
  xlab("Politicians [%]")+ylab("Quote Out-Degree Volume [%]")+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  labs(color="")+theme_ipsum_rc(base_size=12,axis_title_size = 12)


#Gini Values            
quote_gini_in<-networkToGiniValue(quote_igraph,unique(quote_network_nodes$party))
quote_gini_out<-networkToGiniValue(quote_igraph,unique(quote_network_nodes$party),mode='out')

#do a GINI average degree thingy like from stars and tides ... first IN-DEGREE
quote_avg_degree_gini_parties<-quote_node_measures%>%
                              group_by(party)%>%
                                  summarise(mean_degree=mean(degree))

quote_avg_degree_gini_parties<-cbind(quote_avg_degree_gini_parties,quote_gini_in=quote_gini_in$gini)
quote_avg_degree_gini_parties<-cbind(quote_avg_degree_gini_parties,quote_gini_out=quote_gini_out$gini)


#quote gini indegree
c<-ggplot(data=quote_avg_degree_gini_parties,mapping=aes(x=mean_degree,y=quote_gini_in))+
  geom_point(aes(color=party,size=4))+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  theme_ipsum_rc(base_size=12,axis_title_size = 12,plot_title_size = 12)+
  scale_x_continuous(limits=c(1,25))+
  scale_y_continuous(limits=c(0.4,1))+
  xlab("Average Degree")+ylab("Gini")+labs(title='Quote In-Degree')+guides(size=FALSE)+
  labs(color="")
#quote gini outdegree
d<-ggplot(data=quote_avg_degree_gini_parties,mapping=aes(x=mean_degree,y=quote_gini_out))+
                geom_point(aes(color=party,size=4))+
                 scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                    theme_ipsum_rc(base_size=12,axis_title_size = 12,plot_title_size = 12)+
                      scale_x_continuous(limits=c(1,25))+
                        scale_y_continuous(limits=c(0.4,1))+
                       xlab("Average Degree")+ylab("Gini")+labs(title='Quote Out-Degree')+guides(size=FALSE)+
                          labs(color="")
                          
#a and b come from RTNEtwork analysis
grid_arrange_shared_legend(a,b,c,d,ncol = 4, nrow = 1)
 
#plot the RT and the quote graph in one pic 

grid_arrange_shared_legend(rt_graph,quote_graph,ncol=2,nrow=1)

#Write to CSV
write_csv(quote_network_nodes,'quote_network_nodes.csv')
write_csv(quote_edgelist_user,'quote_network_edgelist.csv')

