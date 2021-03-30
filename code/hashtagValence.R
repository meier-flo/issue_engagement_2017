source("dependencies.R")
# Calc political hashtag valence based on Conover2010
# We have to group partys according to RILE  
# CDU/CSU AfD FDP  CONSERVATIVE
# Left Gruene SPD LIBERAL

conservative<-c("AfD","CDU","CSU","FDP")

hashtagsPoliticalValence<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                            select(party,hashtags)%>%
                              unnest()%>%
                                mutate(hashtags=cleanHashtags(hashtags),RILE=ifelse((party %in% conservative),"conservative","liberal"))%>%
                                group_by(RILE)%>%
                                mutate(sumHashtagsUsed=n())%>%ungroup%>%
                                group_by(hashtags,RILE)%>%
                                mutate(countPerHashtagPerRILE=n())%>%ungroup()%>%
                                select(-party)%>%distinct()%>%
                                    mutate(tagProminence=(countPerHashtagPerRILE/sumHashtagsUsed))%>%
                                        select(hashtags,RILE,tagProminence)%>%spread(RILE,tagProminence,fill=0)%>%
                                          mutate(valence=(2*(conservative/(liberal+conservative)))-1)
  

testi<-hashtagsPoliticalValence%>%filter(valence>-0.1,valence<0.1)


#### trying to do the same for every week with map
valencePerWeek<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                      select(weeksTillElection,party,hashtags)%>%unnest()%>%
                         mutate(hashtags=cleanHashtags(hashtags),RILE=ifelse((party %in% conservative),"conservative","liberal"))%>%
                            select(-party)%>%
                              group_by(weeksTillElection)%>%nest()

# Function that calculates the valence on whatever time frame 
# can be passed to map
calcValence<-function(df){
      df%>%
        group_by(RILE)%>%
          mutate(sumHashtagsUsed=n())%>%ungroup%>%
          group_by(hashtags,RILE)%>%
          mutate(countPerHashtagPerRILE=n())%>%ungroup()%>%
          distinct()%>%
         mutate(tagProminence=(countPerHashtagPerRILE/sumHashtagsUsed))%>%
        select(hashtags,RILE,tagProminence)%>%spread(RILE,tagProminence,fill=0)%>%
         mutate(valence=(2*(conservative/(liberal+conservative)))-1)  
}   

valencePerWeek<-valencePerWeek%>%mutate(data=map(data,calcValence))
valencePerWeek<-valencePerWeek%>%unnest()


#whatever Hashtag you want to filter and plot 
bla<-valencePerWeek%>%filter(hashtags%in%c("dieselgate","sozialpolitik"))
ggplot(data=bla, mapping=aes(x=weeksTillElection,y=valence,color=hashtags))+geom_line()+
         scale_x_reverse()+
         theme_bw(base_size = 16)+
         #scale_color_manual(values=party_colors)+
         ylab("Political Valence")+xlab("Weeks Till Election")


testi<-valencePerWeek%>%filter(!hashtags%in%filterHashtags)%>%mutate(valenceSign=sign(valence))%>%
                        group_by(hashtags,valenceSign)%>%
                          summarise(freqSign=n())
testi<-testi%>%group_by(hashtags)%>%mutate(freq=n(),freqSum=sum(freqSign))


testi2<-valencePerWeek%>%filter(hashtags%in%top1000Hashtags$hashtags)%>%
                  filter(weeksTillElection<17)%>%
                  group_by(hashtags)%>%
                  mutate(avgValence=mean(valence),sdValence=sd(valence),upperOutlier=avgValence+sdValence+sdValence+sdValence,lowerOutlier=avgValence-sdValence-sdValence-sdValence,freq=n())%>%ungroup%>%
                  mutate(outlier=ifelse((valence>upperOutlier|valence<lowerOutlier),1,0))

nrow(testi2%>%filter(outlier==1))


testi2_1<-testi2%>%filter(outlier==1)
