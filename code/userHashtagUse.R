# we need the packages and the functions
source("dependencies.R")

# hashtagsPerUserunique
hashtagsPerUserUnique<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
  select(ID,userID,hashtags)%>%
  unnest()%>%select(userID,hashtags)%>%
  mutate(hashtags=cleanHashtags(hashtags))%>%group_by(userID,hashtags)%>%unique()%>%ungroup

# Wieviele Nutzer verwenden einen Hashtag
hashTagUsedByUsers<-hashtagsPerUserUnique%>%group_by(hashtags)%>%summarise(usedByUsers=n())
nrow(hashTagUsedByUsers%>%filter(usedByUsers<2))/nrow(hashTagUsedByUsers)
# 66% aller Hashtags werden nur von einem Nutzer gebraucht

#Das hier auch noch per Party? 

# We need hashtag and overall frequency to look 
hashtagsPerWeek<-tweets.user.table%>%
  select(ID,hashtags,weeksTillElection)%>%
  unnest()%>%
  mutate(hashtags=cleanHashtags(hashtags))%>%
  group_by(hashtags)%>%
  mutate(hashtagFreq=n())%>%
  ungroup%>%
  group_by(weeksTillElection,hashtags)%>%
  mutate(hashtagFreqPerWeek=n())%>%select(weeksTillElection,hashtags,hashtagFreq,hashtagFreqPerWeek)%>%unique()%>%
  ungroup()


testi<-full_join(hashTagUsedByUsers,hashtagsPerWeek%>%select(hashtags,hashtagFreq),by="hashtags")%>%unique
testi<-testi%>%mutate(ratio=usedByUsers/hashtagFreq)

