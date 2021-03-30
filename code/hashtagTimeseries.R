# We filter out the party hashtags cause we look at them separately 
source("dependencies.R")
# Hashtagfrequency per Week and Total
#häufiger Fehler bei Hashtags ist https am Hashtag dran  str_replace_all
#Umlaute entfernen

#How many Tweets do have one Hashtag?
nrow(tweets.user.table%>%filter(weeksTillElection>-1,hashtagCount>0))/nrow(tweets.user.table%>%filter(weeksTillElection>-1))
#When Hashtag how many?
hashtagCount<-tweets.user.table%>%filter(weeksTillElection>-1,hashtagCount>0)
summary(hashtagCount$hashtagCount)


hashtagsPerWeek<-tweets.user.table%>%
    select(ID,hashtags,weeksTillElection)%>%
      unnest()%>%
        filter(weeksTillElection>-1)%>%
        mutate(hashtags=cleanHashtags(hashtags))%>%
          group_by(hashtags)%>%
            mutate(hashtagFreq=n())%>%
               ungroup%>%
                  group_by(weeksTillElection,hashtags)%>%
                    mutate(hashtagFreqPerWeek=n())%>%select(weeksTillElection,hashtags,hashtagFreq,hashtagFreqPerWeek)%>%unique()%>%
                      ungroup()

#We filter cause Party Hashtags turn up a lot 
hashtagsPerWeek_filtered<-hashtagsPerWeek%>%
                            filter(!(hashtags %in% party_string))
#Top Hashtags in general
top_hashtags<-hashtagsPerWeek_filtered%>%
                    select(hashtags,hashtagFreq)%>%
                        distinct()%>%
                          top_n(25)

topHashtagsPerWeek<-right_join(hashtagsPerWeek_filtered,top_hashtags,by="hashtags")%>%
                      select(weeksTillElection,hashtags,hashtagFreqPerWeek)

# There have to be length(hashtagsPerWeek$weekOfYear) = 50 x 10 = 500 values in that df
topHashtagsPerWeekComplete<-complete(topHashtagsPerWeek,weeksTillElection,hashtags,fill = list(hashtagFreqPerWeek = 0))
topHashtagsPerWeekComplete<-topHashtagsPerWeekComplete%>%mutate(logFreq=ifelse(hashtagFreqPerWeek>0,log(hashtagFreqPerWeek),0))%>%
                              group_by(hashtags)%>%mutate(avgLogFreq=mean(logFreq))%>%ungroup
# make the plot
ggplot(data=topHashtagsPerWeekComplete,mapping=aes(x=weeksTillElection,y=logFreq))+facet_wrap(~hashtags)+
                                            geom_line()+
                                                  scale_x_reverse()+
                                                          theme_bw(base_size = 16)


#another option would be to visualize as joyplot
ggplot(data=topHashtagsPerWeekComplete,mapping=aes(x=weeksTillElection,y=reorder(hashtags,-avgLogFreq),height=logFreq,group=hashtags,fill=avgLogFreq))+
                scale_fill_gradient(name="Durschn. Häufigkeit",low="#440154FF",high="#FDE725FF",breaks=c(1,4.5),labels=c("niedriger","höher"),guide = guide_colorbar(direction = "horizontal",title.position="top"))+
                geom_density_ridges2(stat="identity",scale=0.9,alpha=0.75)+scale_x_reverse()+theme_bw(base_size = 18)+
                xlab("Wochen bis zur Bundestagswahl")+ylab("Hashtags")+
                theme(legend.position = "bottom")
                
                  
                

################################
#-----
# Top20 Hashtags of each week 
topHashtagsPerWeek<-hashtagsPerWeek%>%select(weeksTillElection,hashtags,hashtagFreqPerWeek)%>%
                      filter(weeksTillElection<15)%>%
                      group_by(weeksTillElection)%>%
                          dplyr::top_n(20,wt=hashtagFreqPerWeek)%>%ungroup()%>%group_by(weeksTillElection)%>%
                              arrange(desc(hashtagFreqPerWeek))

ggplot(data=topHashtagsPerWeek,mapping=aes(x=reorder(hashtags,-hashtagFreqPerWeek),y=hashtagFreqPerWeek))+
                  geom_bar(stat="identity")+
                        facet_wrap(~weeksTillElection,scales="free")+
                           theme_bw()+
                              theme(axis.text.x=element_text(angle = -90,hjust=0))




###################################
#----------
# Does the ratio of Tweets with Hashtag rise with Election comming?

ratioOfHashtagsPerWeek<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                          select(weeksTillElection,hashtagCount)%>%
                          group_by(weeksTillElection)%>%mutate(tweetsTotal=n(),tweetsWithHashtags=sum(hashtagCount>0),
                                                                ratio=(tweetsWithHashtags/tweetsTotal)*100)
ggplot(data=ratioOfHashtagsPerWeek,mapping=aes(x=weeksTillElection,y=ratio))+
  geom_line(size=2)+scale_x_reverse()+theme_bw(base_size = 18)+xlab("Wochen bis zur Wahl")+ylab("% Tweets mit #")





#################################
#---------
# Annimation of Tweet Count per Week till Election
tweetVolume<-tweets.user.table%>%select(party,weeksTillElection)%>%
                      filter(weeksTillElection<16)%>%group_by(weeksTillElection)%>%
                      mutate(tweetsPerWeek=n())%>%
                      group_by(party,weeksTillElection)%>%
                        mutate(percentageOfTweets=(n()/tweetsPerWeek)*100)%>%distinct()

plot<-ggplot(data=tweetVolume,mapping=aes(x=party,y=percentageOfTweets,fill=party,frame=reorder(weeksTillElection,-weeksTillElection)))+
              geom_bar(stat="identity",position="identity")+
                    scale_fill_manual(values=party_colors,guide="none")+theme_bw(base_size=14)+
                      xlab("")+ylab("Prozent")
                  
require(gganimate)
gganimate(plot, "output.gif")


