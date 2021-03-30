# Create a distribution for Tweets per candiate on Twitter
# avg. hashtags used and avg.mentions used

source('dependencies.R')
require(patchwork)
require(hrbrthemes)

# How many users are tweeting at all 

tweetingUsers<-tweets.user.table%>%filter(weeksTillElection>-1)%>%select(party,twitter.handle)%>%
  group_by(party,twitter.handle)%>%distinct()%>%ungroup%>%group_by(party)%>%
  summarise(count=n())


# Now we need to calculate the percentage of all people that we have for the party
user_table%>%group_by(party)%>%summarise(count=n())%>%view()

tweetingUsers

#AfD CDU CSU LINKE FDP GRUENE SPD
tweetingUsers_totalUsers<-cbind(tweetingUsers,
                              all_users=c(91,324,61,156,70,230,410))

tweetingUsers_totalUsers<-tweetingUsers_totalUsers%>%
                            mutate(percent_active=(count/all_users)*100)      


# Get the number of Tweet for each candidate 

tweetsPerPolitican<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
  group_by(twitter.handle)%>%mutate(freqPerUser=n())%>%
  select(twitter.handle,party,freqPerUser)%>%distinct()


politicansNotTweeting<-user_table%>%filter(!twitter.handle%in%tweetsPerPolitican$twitter.handle)%>%
                mutate(freqPerUser=0)%>%
                  filter(party%in%party_filter)%>%
                    select(twitter.handle,party,freqPerUser)
                      
tweeting_distribution<-rbind(tweetsPerPolitican,
                                  politicansNotTweeting)%>%
                        mutate(party=str_replace(party,'GRUENE','Greens'))%>%
                          mutate(party=str_replace(party,'DIE LINKE','Left'))


# Plot A) Distribution of Tweets per Candidate
tweets_distribution<-ggplot(tweeting_distribution,
             mapping=aes(x=party,y=freqPerUser,
                color=party,fill=party))+geom_boxplot(alpha=0.4)+
                  geom_jitter(width=0.2,alpha=0.2)+
                  scale_fill_manual(values=party_colors_en,
                    labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                      scale_color_manual(values=party_colors_en,
                        labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                          theme_ipsum_rc(base_size=12,axis_title_size = 12)+
                              theme(legend.position="none")+
                                labs(y='Number of Tweets per Candidate',
                                        x='')+coord_flip()

# Plot B) Distribution of Avg. Number of Hashtags per Tweet
## Preparting DATA

tweets_per_candidate<-tweets.user.table%>%
                        group_by(twitter.handle)%>%
                          summarise(count=n())

total_hashtags_per_user<-tweets.user.table%>%
              select(ID,twitter.handle,party,hashtags)%>%
                  unnest(hashtags)%>%
                    mutate(hashtags=cleanHashtags(hashtags))%>%
                      group_by(twitter.handle,party)%>%
                        summarise(count_hashtags=n())
  
hashtag_ratio<-tweets_per_candidate%>%
            left_join(total_hashtags_per_user,
                  by='twitter.handle')%>%
                    filter(!is.na(party))%>%
                      mutate(hashtag_ratio=count_hashtags/count)%>%
                        mutate(party=str_replace(party,'GRUENE','Greens'))%>%
                          mutate(party=str_replace(party,'DIE LINKE','Left'))

#B) Plot                    
hashtag_ratio_distribution<-ggplot(hashtag_ratio,
       mapping=aes(x=party,y=hashtag_ratio,
                   color=party,fill=party))+geom_boxplot(alpha=0.4)+
                    geom_jitter(width=0.2,alpha=0.2)+
                      scale_fill_manual(values=party_colors_en,
                        labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                          scale_color_manual(values=party_colors_en,
                            labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                              theme_ipsum_rc(base_size=12,axis_title_size = 12)+
                                theme(legend.position="none")+
                                  labs(y='Ratio of Hashtags per Tweet',
                                    x='')+coord_flip()

######################
# C) Same for mentions 

total_mentions_per_user<-tweets.user.table%>%
                            select(ID,twitter.handle,party,mentionedUsers)%>%
                              unnest()%>%
                                group_by(twitter.handle,party)%>%
                                  summarise(count_mentions=n())

mention_ratio<-tweets_per_candidate%>%
                 left_join(total_mentions_per_user,
                    by='twitter.handle')%>%
                      filter(!is.na(party))%>%
                        mutate(mention_ratio=count_mentions/count)%>%
                           mutate(party=str_replace(party,'GRUENE','Greens'))%>%
                            mutate(party=str_replace(party,'DIE LINKE','Left'))
#c) Plot
mention_ratio_distribution<-ggplot(mention_ratio,
       mapping=aes(x=party,y=mention_ratio,
                   color=party,fill=party))+geom_boxplot(alpha=0.4)+
                      geom_jitter(width=0.2,alpha=0.2)+
                      scale_fill_manual(values=party_colors_en,
                                        labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                      scale_color_manual(values=party_colors_en,
                                         labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
                      theme_ipsum_rc(base_size=12,axis_title_size = 12)+
                      theme(legend.position="none")+
                      labs(y='Ratio of Mentions per Tweet',
                           x='')+coord_flip()

tweets_distribution+
            mention_ratio_distribution+
              hashtag_ratio_distribution+plot_annotation(tag_levels = 'A')


