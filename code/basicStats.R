source("dependencies.R")
######
#------- Time Series Avg. Tweet Per Politican 
timeSeriesPerTweeterPerWeek<-tweets.user.table%>%group_by(weeksTillElection,party,twitter.handle)%>%
                         summarise(freqPerWeek=n())%>%ungroup%>%
                            group_by(weeksTillElection,party)%>%mutate(totalTweets=sum(freqPerWeek),tweeters=n(),ratio=totalTweets/tweeters)

ggplot(data=timeSeriesPerTweeterPerWeek,mapping=aes(x=weeksTillElection,y=ratio,color=party))+
        geom_line(aes(color=party),size=1.2)+
          scale_x_reverse(breaks = c(seq(35,5,-5),0),label = c("35","30","25","20","15","10","5","Wahlwoche"))+
          scale_color_manual(values=party_colors)+
            theme_bw(base_size = 18)+ylab("Dursch. Anzahl Tweets pro Politiker")+
                xlab("Wochen bis zur Wahl")+labs(color="Partei")


#####
#------- Time Series Total
timeSeriesPerWeek<-tweets.user.table%>%group_by(weeksTillElection,party)%>%
                      summarise(freqPerWeek=n())

ggplot(data=timeSeriesPerWeek,mapping=aes(x=weeksTillElection,y=freqPerWeek,color=party))+
              geom_line(aes(color=party),size=1.2)+
                scale_x_reverse(breaks = c(seq(35,5,-5),0),label = c("35","30","25","20","15","10","5","Wahlwoche"))+
                scale_color_manual(values=party_colors)+
                  theme_bw(base_size = 18)+ylab("Anzahl Tweets")+
                    xlab("Wochen bis zur Wahl")+labs(color="Partei")+
                      annotate("pointrange",x=12,y=5000,ymin=0,ymax=5000,linetype="dashed")+annotate("text",x=12,y=5500,label="Ehe für alle",size=6)+
                        annotate("pointrange",x=2,y=6000,ymin=0,ymax=6000,linetype="dashed")+annotate("text",x=2,y=6500,label="TV-Duell",size=6)


####
#-------- Which Party tweeted the most?
tweetsPerParty<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                   group_by(party)%>%summarise(freqPerParty=n())%>%
                    mutate(total=sum(freqPerParty),ratio=freqPerParty/total*100)
  
ggplot(data=tweetsPerParty,mapping=aes(x=reorder(party,+freqPerParty),y=freqPerParty,fill=party))+
            geom_col(show.legend = FALSE,width=0.8)+
            theme_bw(base_size = 18)+xlab("")+ylab("Anzahl Tweets")+labs(fill="Partei")+
              scale_fill_manual(values=party_colors)+coord_flip()

####
#------- Which Politican tweeted the most?
tweetsPerPolitican<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                      group_by(twitter.handle)%>%mutate(freqPerUser=n())%>%
                        select(twitter.handle,party,freqPerUser)%>%
                          distinct%>%ungroup%>%top_n(10)


ggplot(data=tweetsPerPolitican,mapping=aes(x=reorder(twitter.handle,+freqPerUser),y=freqPerUser,fill=party))+
          geom_col(show.legend = FALSE,width=0.3)+
             theme_bw(base_size = 18)+xlab("Twitter-Name des Accounts")+ylab("Anzahl Tweets")+
                scale_fill_manual(values=party_colors)+coord_flip()


######
#------ Top 10 Tweeters per Party 
topTenTweetersPerParty<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
  group_by(party,twitter.handle)%>%mutate(freqPerUser=n())%>%
  select(twitter.handle,party,freqPerUser)%>%
  distinct%>%ungroup%>%group_by(party)%>%top_n(10)

ggplot(data=topTenTweetersPerParty,mapping=aes(x=reorder(twitter.handle,+freqPerUser),y=freqPerUser,fill=party))+
  geom_col(show.legend = FALSE,width=0.8)+facet_wrap(~party,scales="free")+
  theme_bw(base_size = 18)+xlab("Twitter-Name des Accounts")+ylab("Anzahl Tweets")+
  scale_fill_manual(values=party_colors)+coord_flip()


#####
#----- How many users from which party tweeted at all ?

tweetingUsers<-tweets.user.table%>%filter(weeksTillElection>-1)%>%select(party,twitter.handle)%>%
                        group_by(party,twitter.handle)%>%distinct()%>%ungroup%>%group_by(party)%>%
                          summarise(count=n())
                        
ggplot(data=tweetingUsers,mapping=aes(x=reorder(party,+count),y=count,fill=party,label=count))+
  geom_col(show.legend = FALSE,width=0.8)+geom_text(nudge_y = 25)+
  theme_bw(base_size = 18)+xlab("")+ylab("Anzahl User")+labs(fill="Partei")+
  scale_fill_manual(values=party_colors)+coord_flip()


# average @-Mentions and #-Hashtags per Party
avgMentionHashtag<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                      summarise(meanMention=mean(mentionCount),
                                                  meanHashtag=mean(hashtagCount))


