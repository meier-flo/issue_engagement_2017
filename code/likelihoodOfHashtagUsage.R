require(scales)
require(tidyr)
hashtagsPerParty<-tweets.user.table%>%
  select(party,hashtags)%>%
  unnest()%>%
  mutate(hashtags=cleanHashtags(hashtags))%>%
  group_by(party)%>%mutate(hashtagsPartyTotal=n())%>%
  ungroup%>%
  group_by(party,hashtags)%>%mutate(hashtagFreqPerParty=n(),ratioOfHashtagUse=hashtagFreqPerParty/hashtagsPartyTotal)%>%unique%>%
  ungroup

#
likelihoodOfHashtagUse<-hashtagsPerParty%>%
                          select(party,hashtags,ratioOfHashtagUse)%>%filter(party %in% party_filter)

# We have to spread first so every party has its own column
likelihoodOfHashtagUse_spread<-likelihoodOfHashtagUse%>%
                                  spread(party,ratioOfHashtagUse)
                                    

likelihoodOfHashtagUse_spread_CDU_LINKE<-likelihoodOfHashtagUse_spread%>%select(hashtags,CDU,`DIE LINKE`)
#na.omit(likelihoodOfHashtagUse_spread_AFD_LINKE)
ggplot(likelihoodOfHashtagUse_spread_CDU_LINKE, mapping=aes(x=CDU,y=`DIE LINKE`)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = hashtags), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


likelihoodOfHashtagUse_spread_CDU_CSU<-na.omit(likelihoodOfHashtagUse_spread%>%select(hashtags,CDU,CSU))
ggplot(likelihoodOfHashtagUse_spread_CDU_CSU, mapping=aes(x=CDU,y=CSU)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = hashtags), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

likelihoodOfHashtagUse_spread_AfD_CSU<-na.omit(likelihoodOfHashtagUse_spread%>%select(hashtags,AfD,CSU))
ggplot(likelihoodOfHashtagUse_spread_AfD_CSU, mapping=aes(x=AfD,y=CSU)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = hashtags), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

#now let’s find which words are more or less likely 
#to come from each person’s account using the log odds ratio. 
# to do this with every combination probably we have to use map from purrr ?! 
logOddsRatio<-hashtagsPerParty%>%select(party,hashtags,hashtagFreqPerParty)%>%
                                  filter(party %in%party_filter)%>%
                                    mutate(party=make.names(party))%>%
                                      filter(hashtagFreqPerParty>10)%>%
                                          spread(party,hashtagFreqPerParty,fill=0)%>%
                                              mutate_if(is.numeric,funs((.+1)/sum(.+1)))%>%
                                                mutate(logratioCDUSPD=log(SPD/CDU))

logOddsRatio%>%select(hashtags,logratioCDUSPD)%>%
                    group_by(logratioCDUSPD<0)%>%
                      top_n(15,abs(logratioCDUSPD))%>%ungroup%>%
                        mutate(hashtags=reorder(hashtags,logratioCDUSPD))%>%
                            ggplot(aes(hashtags, logratioCDUSPD, fill = logratioCDUSPD < 0)) +
                            geom_col() +
                            coord_flip() +
                            ylab("log odds ratio (SPD/CDU)") +
                              scale_fill_manual(values=c("red","black"),name = "", labels = c("SPD", "CDU"))+
                                theme_bw(base_size = 16)


####### Cosine Similarity of Party-Hashtag-Ratio-Vectors ############
require(lsa)

likelihoodOfHashtagUse_spread_cosine<-likelihoodOfHashtagUse%>%
  spread(party,ratioOfHashtagUse,fill=0)


cosineValuesPartysHashtags<-as.matrix(likelihoodOfHashtagUse_spread_cosine%>%select(-hashtags))

cosine_similarity_partys<-rownames_to_column(as.data.frame(cosine(cosineValuesPartysHashtags)))%>%gather(party2,"value",2:8)%>%unique

ggplot(data = cosine_similarity_partys, aes(x = rowname, y = party2))+
  geom_tile(aes(fill = value))+
    geom_text(aes(label = round(value, 2)))+
      scale_fill_viridis()+
        theme_bw(base_size = 16)

# Es ist schon eher überraschend, dass LINKE und CDU so eine hohe Ähnlichkeit aufweisen bei der Hashtag-Nutzung
#


                           