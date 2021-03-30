require(tidyverse)
require(stringr)
require(igraph)
require(ggnetwork)
require(intergraph)
require(lubridate)

#Get the mean senitment score for each party and use of party hashtags
hashtagSentimentUse_sentiWS<-tweets.user.table%>%filter(weeksTillElection<17)%>%
    select(ID,party,hashtags,senti_value)%>%
          unnest()%>%mutate(hashtags=cleanHashtags(hashtags))%>%
                        filter(hashtags %in% party_string)%>%
                          mutate(party=ifelse(party=="DIE LINKE","LINKE",party))%>%
                            group_by(party,hashtags)%>%
                              summarise(senti_value=mean(senti_value))
#Sentistrength
hashtagSentimentUse_sentiStrength<-tweets.user.table%>%filter(weeksTillElection<17)%>%
  select(ID,party,hashtags,positveSentimentScore,negativeSentimentScore)%>%
  unnest()%>%mutate(hashtags=cleanHashtags(hashtags),senti_value=as.integer(positveSentimentScore)+as.integer(negativeSentimentScore))%>%
  filter(hashtags %in% party_string)%>%
  mutate(party=ifelse(party=="DIE LINKE","LINKE",party))%>%
  group_by(party,hashtags)%>%
  summarise(senti_value=mean(senti_value))


ggplot(data = hashtagSentimentUse_sentiWS, aes(x = party, y = hashtags))+
                 geom_tile(aes(fill = senti_value))+geom_text(aes(label=round(senti_value,2)))+
                  scale_fill_gradientn(colors=c("#453781FF","#f7f7f7","#FDE725FF"))+
                      theme_bw(base_size = 16)+xlab("Partei")+ylab("Tweet mit Hashtag")+labs(fill="Sentiment")

            
            
#hashtagSentimentUse_byWeek<-tweets.user.table%>%filter(weeksTillElection<17)%>%
#              select(ID,party,hashtags,weeksTillElection,positveSentimentScore,negativeSentimentScore)%>%
#              unnest()%>%mutate(hashtags=cleanHashtags(hashtags))%>%
#              mutate(senti_value_strength=as.integer(positveSentimentScore)+as.integer(negativeSentimentScore))%>%
#              filter(hashtags %in% party_string)%>%
#              group_by(party,hashtags,weeksTillElection)%>%
#              summarise(mean_senti_value=mean(senti_value_strength))
          
#testi<-ggplot(data = hashtagSentimentUse_byWeek, aes(x = party, y = hashtags, frame=reorder(weeksTillElection,-weeksTillElection)))+
#  geom_tile(aes(fill = mean_senti_value))+geom_text(aes(label=round(mean_senti_value,3)))+
#  scale_fill_gradientn(colors=c("red","white","green"))+
#  theme_bw(base_size = 16)
#gganimate(testi,"sentiment.gif")

###################
#### make a plot like in tidytext 

partyUseHashtagAFD<-hashtagSentimentUse_byWeek%>%filter(party=="AfD",hashtags!="afd")


ggplot(data=testi,mapping=aes(x=weeksTillElection,y=mean_senti_value,fill=party))+
          geom_col(show.legend = FALSE)+
          scale_x_reverse()+
            facet_wrap(~party)+
              scale_fill_manual(values=party_colors)+
                theme_bw(base_size = 16)+xlab("Wochen bis zur Wahl")+ylab("Mittleres Sentiment")+
                  ggtitle("Tweets der Parteien mit #AfD")




###############
##### compare/show trend in the senti_values 
compareSentiValues<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
      select(ID,text,positveSentimentScore,negativeSentimentScore,senti_value)%>%
        mutate(senti_strength_value=as.integer(positveSentimentScore)+as.integer(negativeSentimentScore))

#correlation ... not so optimal only 0.35
cor(compareSentiValues$senti_value,compareSentiValues$senti_strength_value)
# Plots der Senti-Werte 

ggplot(data=compareSentiValues,mapping=aes(x=jitter(senti_strength_value),y=senti_value))+theme_bw(base_size = 18)+
                geom_point()+geom_smooth()+ylab("SentiWS-Wert")+xlab("SentiStrength-Wert")+
                  annotate("rect", xmin = -5, xmax = 0, ymin = 0, ymax = 3,alpha = .2,color="red")+
                    annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = -5,alpha = .2,color="red")


# filter the ones where one is positive the other negative
testi<-compareSentiValues%>%filter(senti_strength_value>0&senti_value<0)
nrow(compareSentiValues%>%filter(senti_strength_value<0&senti_value>=0))/nrow(compareSentiValues)*100

