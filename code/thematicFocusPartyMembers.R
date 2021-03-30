#Entropy TEst 
#buys <- c("no","yes","maybe","not","today")
#(entropy.empirical(table(buys), unit="log2")/log2(length(buys)))
# Wenn jemand nur einen Hashtag hat den zweimal verwendet dann ist die Entropie 0  1-0 = 1 Thematischer Fokus hoch
# Wenn jemand zwei verschiedene Hashtags hat und beide verwendet ist die Entropie 1  1-1 = 0 Thematischer Fokus gering
# Wenn jemand nur einen Hashtag hat dann log2(1)= 0 -> NaN  eigentlich thematicFocus = 1
# Je mehr Hashtags gebraucht werden desto höher ist die Entropie und desto geringer der thematische Fokus 


# Oder müsste man die rausfiltern?!
#       "AfD",      "CDU",      "CSU",   "left"    "FDP",  "freien",  "GREEN"   "SPD"
#party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")
#party_filter<-c("FDP","CDU","CSU","SPD","DIE LINKE","GRUENE","AfD")

#we need tweets.with.topics df from topicByHashtagCoOc.R

topicalFocusByUser<-tweets.with.topics%>%
  select(twitter.handle,party,topic)%>%
  group_by(twitter.handle)%>%mutate(topicsUsedByUser=n(),normEntropy=(entropy.empirical(table(topic),unit="log2")/(log2(topicsUsedByUser))),
                            thematicFocus=ifelse(topicsUsedByUser>1,ifelse(1-normEntropy<0,0,1-normEntropy),1))%>%
                               select(twitter.handle,party,thematicFocus)%>%unique()

#CDF-Plot
ggplot(data=topicalFocusByUser,aes(x=thematicFocus,color=party))+
  stat_ecdf()+theme_bw(base_size = 16)+scale_color_manual(values=party_colors)+
      xlab("Thematischer Fokus")+ylab("ECDF")


#Boxplot
ggplot(data=topicalFocusByUser,aes(x=party,y=thematicFocus,color=party))+geom_boxplot()+
          theme_bw(base_size = 16)+scale_color_manual(values=party_colors)

# Density-Plot
#ggplot(data=topicalFocusByUser, aes(x=thematicFocus, ..density.., colour = party))+
#  geom_freqpoly(bins=10)+scale_color_manual(values=party_colors)+theme_bw(base_size = 16)


#####################
### earlier version with Hashtags only 
require(entropy)
testi<-tweets.user.table%>%
        select(userID,party,hashtags)%>%unnest()%>%
              mutate(hashtags=cleanHashtags(hashtags))%>%
                  group_by(userID)%>%mutate(hashtagSentByUser=n(),normEntropy=(entropy.empirical(table(hashtags),unit="log2")/(log2(hashtagSentByUser))),
                                                    thematicFocus=ifelse(hashtagSentByUser>1,ifelse(1-normEntropy<0,0,1-normEntropy),1))%>%
                                                       select(userID,party,thematicFocus)%>%unique()%>%
                                                        filter(party %in% party_filter)

ggplot(data=testi,aes(x=thematicFocus,color=party))+
          stat_ecdf()+theme_bw(base_size = 16)+scale_color_manual(values=party_colors)
