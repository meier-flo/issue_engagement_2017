source("dependencies.R")
# use of hashtags by other parties 
# we need party use of party hashtags/use by week 


hashjacking<-tweets.user.table%>%group_by(party,weeksTillElection)%>%
                      filter(weeksTillElection<16,hashtagCount>0)%>%
                      mutate(numberOfTweetsPerWeek=n())%>%
                      ungroup()%>%
                        select(party,numberOfTweetsPerWeek,hashtags,weeksTillElection)%>%
                            unnest()%>%
                              mutate(hashtags=cleanHashtags(hashtags))%>%
                              filter(hashtags %in% party_string)%>%
                                group_by(party,hashtags,weeksTillElection)%>%
                                  mutate(numberOfHashjacksPerPartyPerWeek=n(),ratioHashjacks=numberOfHashjacksPerPartyPerWeek/numberOfTweetsPerWeek)%>%
                                    ungroup()%>%
                                      mutate(party=str_to_upper(party),hashtags=str_to_upper(hashtags))%>%
                                        filter(!party==hashtags)


#testi<-hashjacking%>%filter(weeksTillElection<16)

hashjacking<-complete(hashjacking,party,weeksTillElection,hashtags,fill=list(ratioHashjacks=0))

#After Completing we have to again strip the own mentions and make it unique
hashjacking<-hashjacking%>%mutate(party=ifelse(party=="DIE LINKE","LINKE",party))%>%
              filter(!party==hashtags)%>%unique

#Just for this test change the color 
party_colors_hashjack<-c(AFD="#1eaac2",CDU="#000000",CSU="#1804f6",LINKE="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")

ggplot(data=hashjacking,mapping=aes(x=weeksTillElection,y=ratioHashjacks*100,color=hashtags))+
                        geom_point()+
                          geom_smooth(span=0.8,se=FALSE)+
                              facet_wrap(~party,ncol=4)+
                                  scale_x_reverse()+
                                      theme_bw(base_size = 16)+
                                          scale_color_manual(values=party_colors_hashjack)+
                                            ylab("% Hashjacks")+xlab("Wochen bis zur Wahl")+labs(color="")



############### SENTIMENT TEST FOR FDP REGARDING SPD ########################
sentimentFDPSPD<-tweets.user.table%>%
  select(party,weeksTillElection,hashtags,positveSentimentScore,negativeSentimentScore)%>%
  unnest()%>%
  mutate(hashtags=cleanHashtags(hashtags),sentimentScore=as.integer(positveSentimentScore)+as.integer(negativeSentimentScore))%>%
  filter(party =="DIE LINKE")%>%
    mutate(spdTagged=ifelse(hashtags=="afd",TRUE,FALSE))%>%
  group_by(party,spdTagged,weeksTillElection)%>%summarise(meanSentiment=median(sentimentScore))


ggplot(data=sentimentFDPSPD,mapping=aes(x=weeksTillElection,y=meanSentiment,color=spdTagged))+
          geom_line()+scale_x_reverse()





