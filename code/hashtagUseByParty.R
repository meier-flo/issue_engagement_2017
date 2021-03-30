source("dependencies.R")
#Wieviele Tweets pro Partei mit Hashtags
#Durchschnittliche Anzahl an Hashtags pro Tweet + SD
tweetsByPartyFreq<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
          filter(hashtagCount>0)%>%
              group_by(party)%>%summarise(tweetsPerPartyWithHashtag=n(),meanHashtagCount=mean(hashtagCount),sdOfMean=sd(hashtagCount))%>%
                ungroup%>%
                  filter(party %in% party_filter)

# Mean Number of HashtagsPerTweet for each Party 
ggplot(data=tweetsByPartyFreq,mapping=aes(x=reorder(party,-meanHashtagCount),y=meanHashtagCount,fill=party))+
          geom_bar(stat="identity",show.legend = FALSE)+
              geom_errorbar(aes(ymin=meanHashtagCount-sdOfMean,ymax=meanHashtagCount+sdOfMean),width=0.2)+
                theme_bw(base_size = 18)+
                  scale_fill_manual(values=party_colors)+coord_flip()+ylab("Dursch. Anzahl Hashtags")+
                    labs(fill="Partei")+xlab("")



# Make a Boxplot of Hashtag
hashtagCountPerParty<-tweets.user.table%>%filter(weeksTillElection>-1,hashtagCount>0)%>%
                      select(party,hashtagCount)

ggplot(data=hashtagCountPerParty,mapping=aes(x=party,y=hashtagCount,fill=party))+
          geom_boxplot()+
              scale_fill_manual(values=party_colors)+theme_bw(base_size = 18)+
                xlab("Partei")+ylab("Hashtags Pro Tweet")

#NumberofHashtagsPerParty and Usage of Single Hashtags and Ratio
hashtagsPerParty<-tweets.user.table%>%filter(weeksTillElection<17)%>%
  select(party,hashtags)%>%
  unnest()%>%
   mutate(hashtags=cleanHashtags(hashtags))%>%
    group_by(party)%>%mutate(hashtagsPartyTotal=n())%>%
      ungroup%>%
      group_by(party,hashtags)%>%mutate(hashtagFreqPerParty=n(),ratioOfHashtagUse=hashtagFreqPerParty/hashtagsPartyTotal)%>%unique%>%
       ungroup

#What are the top 5 HashtagsPerParty 
topHashtagsPerParty<-hashtagsPerParty%>%select(party,hashtags,hashtagFreqPerParty)%>%
                        group_by(party)%>%top_n(20)%>%filter(party %in% party_filter)

#Make a Barchart of the top 5 Hashtags Per Party Ratiowise = percetage use of hashtag in Tweets with hashtag
ggplot(data=topHashtagsPerParty,mapping=aes(x=reorder(hashtags,-ratioOfHashtagUse),y=ratioOfHashtagUse))+
          geom_bar(stat="identity")+
                facet_wrap(~party,scales = "free")+
                    theme_bw(base_size = 16)


