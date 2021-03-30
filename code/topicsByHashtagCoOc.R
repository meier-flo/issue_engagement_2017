source("dependencies.R")

#hashtag_topic_relation comes from pmiNetwork.R
num_HashtagsPer_Community<-hashtag_topic_relation%>%group_by(topic)%>%summarise(count=n())
#What percentage do the biggest communities cover? 98%
num_HashtagsPer_Community%>%filter(count>57)%>%summarise(sum=sum(count))/nrow(hashtag_topic_relation)
#Which are these communities so we can filter 
top_communities<-num_HashtagsPer_Community%>%filter(count>57) 

#get the top 5 hashtags decreasing degree
hashtagsPerCommunityTop26<-hashtag_topic_relation%>%filter(topic%in%top_communities$topic)
top5_nodes_pminet<-node_infos%>%select(name,degree,community)%>%
                      filter(community %in% top_communities$topic)%>%
                          group_by(community)%>%top_n(5,degree)


# now lets do futher analysis 
tweets.with.topics<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                           select(ID,party,twitter.handle,hashtags,weeksTillElection)%>%unnest()%>%
                              mutate(hashtags=cleanHashtags(hashtags))%>%
                                left_join(hashtagsPerCommunityTop26,by=c("hashtags"="hashtag"))%>%
                                  filter(!is.na(topic))

#Also this sound like a lot of filtering we are not throwing away to many tweets
#still using a lot of Tweets
nrow(tweets.with.topics%>%select(ID)%>%distinct)/nrow(tweets.user.table%>%filter(weeksTillElection>-1,hashtagCount>0))

#How many different topics are there per Tweet?
tweets.with.topics<-tweets.with.topics%>%
                      group_by(ID)%>%
                        mutate(unique_topics=length(unique(topic)))%>%ungroup

summary(tweets.with.topics$unique_topics)

#Whats the percentage covered of the data set with increasing hashtags
cum_sum_tweets_topics<-tweets.with.topics%>%select(ID,unique_topics)%>%distinct%>%
        group_by(unique_topics)%>%
          summarise(count_topics=n())%>%ungroup%>%
            mutate(total=sum(count_topics),percentage=(count_topics/total)*100,
                   cumsum_percentage=(cumsum(count_topics)/total))

#Do a barchart / cumsum_plot 
ymax<-130000
ggplot(data=cum_sum_tweets_topics)+geom_bar(aes(x=unique_topics,y=count_topics),stat='identity')+
  geom_line(aes(x=unique_topics,y=cumsum_percentage*ymax),col="red", lwd=1)+
  scale_x_continuous("Topics per Tweet",limits = c(0,8),breaks = c(seq(1:8)))+
    scale_y_continuous(name = 'Number of Tweets',labels=comma,breaks = c(0,25000,50000,75000,100000,125000),sec.axis = sec_axis((~./ymax*100),
      name = "Cumulative Percentage of Tweets [%]"))+theme_ipsum_rc(base_size=12,axis_title_size = 12)


#With how many Tweets do how many issues get addressed?
issues_addressed<-tweets.with.topics%>%select(ID,party,topic,unique_topics)%>%distinct


#Which party addresses which topics?
topicsPerParty<-issues_addressed%>%group_by(party,topic)%>%summarise(countTweetsPerTopicPerParty=n())
topicsPerParty<-topicsPerParty%>%group_by(party)%>%mutate(totalTweetsPerParty=sum(countTweetsPerTopicPerParty))%>%ungroup
              

#give the issues the issue name / comes from dependencies
topicsPerParty<-left_join(topicsPerParty,topic_number_name_new,by=c("topic"="number"))

#calc z-scores to see whether one party is dominating the 
topicsPerParty<-topicsPerParty%>%group_by(party)%>%mutate(z_score_group_party=(countTweetsPerTopicPerParty-mean(countTweetsPerTopicPerParty))/sd(countTweetsPerTopicPerParty))%>%
                                    group_by(topic)%>%mutate(z_score_group_topic=(countTweetsPerTopicPerParty-mean(countTweetsPerTopicPerParty))/sd(countTweetsPerTopicPerParty))

tweetsPerTopicPerParty<-issues_addressed%>%select(party,topic)%>%
                  group_by(topic)%>%mutate(countTweetsPerTopic=n())%>%ungroup%>%
                     group_by(topic,party)%>%mutate(countTweetsPerTopicPerParty=n())%>%
                        mutate(ratio=countTweetsPerTopicPerParty/countTweetsPerTopic)

tweetsPerTopicPerParty<-left_join(tweetsPerTopicPerParty,topic_number_name_new,by=c("topic"="number"))

tweetsPerTopicPerParty<-tweetsPerTopicPerParty%>%select(party,name,ratio)%>%distinct

#Make a Percentage plot how many tweets per Topic by each party 
ggplot(data=tweetsPerTopicPerParty,mapping=aes(x=as.factor(name),y=ratio*100,fill=party))+
            geom_bar(position='fill',stat='identity')+scale_y_continuous(labels = percent_format())+
                coord_flip()+
                  scale_fill_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                   ylab("Tweets per Issue per Party [%]")+xlab("")+
                      labs(fill="")+theme(legend.position="bottom")+
                        theme_ipsum_rc(base_size=12,axis_title_size = 12)
                      
#What is the Valence of each Issue?
# We need the valence of every hashtag from hashtagValence
# join node_infos from PMI with hashtagPoliticalValence
issue_valence<-left_join(node_infos,hashtagsPoliticalValence,by=c("name"="hashtags"))
issue_valence<-issue_valence%>%group_by(community)%>%
                  summarise(issue_valence=mean(valence))%>%filter(community%in%top_communities$topic)


#What is the topical focus of each user? regarding their issues
user_issue_focus<-tweets.with.topics%>%select(ID,twitter.handle,party,topic,unique_topics)%>%distinct%>%
                    select(twitter.handle,party,topic)%>%
                    group_by(twitter.handle)%>%mutate(topicsUsedByUser=n(),normEntropy=(entropy.empirical(table(topic),unit="log2")/(log2(topicsUsedByUser))),
                    thematicFocus=ifelse(topicsUsedByUser>1,ifelse(1-normEntropy<0,0,1-normEntropy),1))%>%
                    select(twitter.handle,party,thematicFocus)%>%unique()

b<-ggplot(data=user_issue_focus,aes(x=thematicFocus,color=party))+
  stat_ecdf(size=1.1)+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  xlab("(b) Issue Focus")+ylab("ECDF")+labs(color="")+theme_ipsum_rc(base_size=12,axis_title_size = 12)

#What is the mean user valence?
user_valence<-left_join(tweets.with.topics,hashtagsPoliticalValence,by=c("hashtags"="hashtags"))
user_valence<-user_valence%>%group_by(twitter.handle)%>%mutate(mean_user_valence=mean(valence))%>%
                select(twitter.handle,party,mean_user_valence)%>%distinct
                    
a<-ggplot(data=user_valence,aes(x=mean_user_valence,color=party))+
  stat_ecdf(size=1.1)+
  scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  xlab("(a) Mean User Valence (Hashtags)")+ylab("ECDF")+labs(color="")+theme_ipsum_rc(base_size=12,axis_title_size = 12)

#What is the pairwise Similiarity 
#cosine_similarity_users_gathered comes from cosinesimilarityusers.r
withinPartySimilarity<-left_join(cosine_similarity_users_gathered,user_table,by=c("rowname"="twitter.handle"))
withinPartySimilarity<-left_join(withinPartySimilarity,user_table,by=c("user2"="twitter.handle"))
withinPartySimilarity<-withinPartySimilarity%>%select(party.x,party.y,value)%>%filter(party.x==party.y)
withinPartySimilarity$party.x<-str_replace(withinPartySimilarity$party.x,"FDP DVP","FDP")
withinPartySimilarity$party.y<-str_replace(withinPartySimilarity$party.y,"FDP DVP","FDP")


#withinPartySimilarity<-withinPartySimilarity%>%
#  mutate(party.x=ifelse(party.x==party.y,party.x,"Between Parties"))%>%
#  select(party.x,value)
#withinPartySimilarity$party.x<-str_replace(withinPartySimilarity$party.x,"FDP DVP","FDP")
#party_colors_ecdf<-c(party_colors,`Between Parties`="#939393")

c<-ggplot(data = withinPartySimilarity,mapping=aes(x=value,color=party.x))+
  stat_ecdf(size=1.1)+scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  xlab("(c) Pairwise Cosine Similarity (Issues)")+ylab("ECDF")+theme_ipsum_rc(base_size=12,axis_title_size = 12)


a+b+c+d+plot_layout(ncol=4)

grid_arrange_shared_legend(a,b,c,ncol = 3, nrow = 1)


#Plot z-scores per issue per party

topics_per_party_plot <- topicsPerParty%>%mutate(party=ifelse(party=='GRUENE','Greens',party),
                                                 party=ifelse(party=='DIE LINKE','Left',party))


ggplot(data=topics_per_party_plot,
          mapping=aes(y=z_score_group_party,x=party,fill=party))+geom_col()+
          scale_fill_manual(values=party_colors_en)+
            facet_wrap(~name,ncol=4)+
                theme_minimal(base_size = 13)+
                 theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                    labs(y='Z-Score',x='Party')+theme(legend.title = element_blank(),legend.position="bottom")
  




testi<-topicsPerParty%>%filter(topic==1)
testi<-tweets.with.topics%>%filter(party=='CSU',topic==12)
nrow(tweets.with.topics%>%filter(twitter.handle=='koschyk',party=='CSU',topic==12))41
