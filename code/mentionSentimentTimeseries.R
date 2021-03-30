#####
# Mention

MentionUseSentiment<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                     select(ID,party,mentionedUsers,positveSentimentScore,negativeSentimentScore)%>%
                       unnest()%>%mutate(mentionedUsers=str_replace(mentionedUsers,"@",""))%>%
                        left_join(user_table,by=c("mentionedUsers"="twitter.handle"))%>%
                          mutate(sentiment=as.numeric(positveSentimentScore)+ as.numeric(negativeSentimentScore))

#66% der Accounts sind unbekannt
MentionUseSentiment_Plot<-MentionUseSentiment%>%filter(!is.na(party.y))%>%
  filter(party.y %in% party_filter)%>%
  select(mentioner=party.x,mentioned=party.y,sentiment)%>%
  group_by(mentioner,mentioned)%>%summarise(mean_senti_value=mean(sentiment))

# make a heatmap
ggplot(data = MentionUseSentiment_Plot, aes(x = mentioner, y = mentioned))+
  geom_tile(aes(fill = mean_senti_value))+geom_text(aes(label=round(mean_senti_value,2)))+scale_fill_viridis()+
  theme_bw(base_size = 16)



##########################
## Mention TimeSeries 

mentionjacking<-tweets.user.table%>%
                filter(weeksTillElection>-1,mentionCount>0)%>%
                  #group_by(party,weeksTillElection)%>%
                   #  mutate(mentionsPerWeek=sum(mentionCount))%>%
                    #   ungroup()%>%
              select(party,mentionedUsers,weeksTillElection)%>%
               unnest()%>%
                mutate(mentionedUsers=str_replace(mentionedUsers,"@",""))%>%
                  left_join(user_table,by=c("mentionedUsers"="twitter.handle"))%>%
                    select(-c(mentionedUsers,id))%>%filter(!is.na(party.y))%>%
                     group_by(party.x,weeksTillElection)%>%
              mutate(mentionsPerWeek=n())%>%
                  group_by(party.x,party.y,weeksTillElection)%>%
              mutate(mentionsPerPartyPerWeek=n(),
                ratioMentionJacks=mentionsPerPartyPerWeek/mentionsPerWeek)%>%
                    ungroup()


mentionjacking<-complete(mentionjacking,party.x,weeksTillElection,party.y,fill=list(ratioMentionJacks=0))%>%unique()%>%
                    filter(party.y%in%party_filter)



#Just for this test change the color 
#party_colors_hashjack<-c(AFD="#1eaac2",CDU="#000000",CSU="#1804f6",LINKE="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")

mentionjacking<-mentionjacking%>%mutate(party.y=ifelse(party.y=="DIE LINKE","The Left",party.y),
                                                    party.y=ifelse(party.y=="GRUENE","Greens",party.y))



ggplot(data=mentionjacking,mapping=aes(x=weeksTillElection,y=ratioMentionJacks*100,color=party.x))+
               geom_smooth(span=0.8,se=FALSE)+
                 facet_wrap(~party.y,ncol=7,scales = "free")+
                    scale_x_reverse()+
                         scale_color_manual(values=party_colors_de,labels=c("AfD","CDU","CSU","Left","FDP","Greens","SPD"))+
  ylab("Party Mentions [%]")+xlab("Weeks Till Election")+labs(color="")+theme_ipsum_rc(base_size=12,axis_title_size = 12)




