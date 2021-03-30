# for this we ned cosine_similartiy_users_gathered based on the topics we detected 
# or use the cosine similarity based on hashtag frequency
# make a plot like in livne - how similar are partymembers i.e. in which party do topics tend to go opposite directions
#double_join for party associations 
withinPartySimilarity<-left_join(cosine_similarity_users_gathered,user_table,by=c("rowname"="twitter.handle"))
withinPartySimilarity<-left_join(withinPartySimilarity,user_table,by=c("user2"="twitter.handle"))
withinPartySimilarity<-withinPartySimilarity%>%select(party.x,party.y,value)

withinPartySimilarity<-withinPartySimilarity%>%
      mutate(party.x=ifelse(party.x==party.y,party.x,"BETWEEN"))%>%
        select(party.x,value)
          withinPartySimilarity$party.x<-str_replace(withinPartySimilarity$party.x,"FDP DVP","FDP")
            party_colors_ecdf<-c(party_colors,BETWEEN="#939393")

ggplot(data = withinPartySimilarity,mapping=aes(x=value,color=party.x))+
  stat_ecdf()+theme_bw(base_size = 16)+scale_color_manual(values=party_colors_ecdf)+
  xlab("Cosinus-Ähnlichkeit")+ylab("ECDF")


# plot a scatterplot topical focus and degree in the mention network 
topicalFocusByUser<-tweets.with.topics%>%
  select(twitter.handle,party,topic)%>%
  group_by(twitter.handle)%>%mutate(topicsUsedByUser=n(),normEntropy=(entropy.empirical(table(topic),unit="log2")/(log2(topicsUsedByUser))),
                                    thematicFocus=ifelse(topicsUsedByUser>1,ifelse(1-normEntropy<0,0,1-normEntropy),1))%>%
                                     select(twitter.handle,party,thematicFocus)%>%unique()

#merge topical focus with the mention degree 
mentionTableGraph_nodes<-as.tibble(mentionTableGraph%>%activate(nodes))

topicalFocusMentionRelation<-left_join(topicalFocusByUser,mentionTableGraph_nodes,by=c("twitter.handle"="name"))

topicalFocusMentionRelation<-topicalFocusMentionRelation%>%filter(!is.na(degree_all))

ggplot(data=topicalFocusMentionRelation,mapping=aes(x=degree_all,y=thematicFocus))+
            geom_point(aes(color=party.x))+geom_smooth()+
            theme_bw(base_size=12)+scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
              xlab("@-Mention Degree")+ylab("Issue Focus")+labs(color="Party")

cor(topicalFocusMentionRelation$thematicFocus,topicalFocusMentionRelation$degree_all,use="everything")

#What is the average cosine similarity for each hop users are away in the mention network
# need distances of mention network 
distancesMentionNet<-distancesMentionNet%>%filter(!is.infinite(value))

distanceCosinePlot<-left_join(distancesMentionNet,cosine_similarity_users_gathered,by=c("rowname"="rowname","user2"="user2"))
distanceCosinePlot<-left_join(distanceCosinePlot,cosine_similarity_users_gathered,by=c("rowname"="user2","user2"="rowname"))

distanceCosinePlot<-distanceCosinePlot%>%
                        mutate(value.y=ifelse(is.na(value.y),value,value.y))%>%
                          select(user1=rowname,user2=user2,cosine_value=value.y,distance=value.x)%>%
                            filter(!is.na(cosine_value))

#What's the median/mean distance between pairs in our mention network?
summary(distanceCosinePlot$distance)

pairs_per_distance<-distanceCosinePlot%>%group_by(distance)%>%summarise(coun=n())

distanceCosinePlot_filtered<-distanceCosinePlot%>%filter(distance<11)

distanceCosinePlot_filtered<-left_join(distanceCosinePlot_filtered,user_table,by=c("user1"="twitter.handle"))
distanceCosinePlot_filtered<-left_join(distanceCosinePlot_filtered,user_table,by=c("user2"="twitter.handle"))

distanceCosinePlot_filtered<-distanceCosinePlot_filtered%>%select(user1:distance,party.x,party.y)
distanceCosinePlot_filtered<-distanceCosinePlot_filtered%>%
                        mutate(relation=ifelse(party.x==party.y,"WITHIN","ACROSS"))

ggplot(data=distanceCosinePlot_filtered,aes(x=as.factor(distance),y=cosine_value,fill=as.factor(relation)))+geom_boxplot(width=0.6)+
              theme_bw(base_size=14)+xlab("Distance in @-Mention Graph")+ylab("Cosine Similarity")+labs(fill="Party")+
              scale_fill_grey(start=0.3,end=0.8)+
                theme_ipsum_rc(base_size=12,axis_title_size = 12)+
                  theme(legend.position="bottom")



distanceCosinePlot<-distanceCosinePlot%>%group_by(distance)%>%
                    summarise(mean_cosine=median(cosine_value))

