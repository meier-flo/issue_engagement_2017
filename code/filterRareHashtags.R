source("dependencies.R")

tweets.user.table<-twitter_data%>%
                      mutate(hashtags =  str_extract_all(text,hashtag_pattern))


# hashtagsPerUserunique
hashtagsPerUserUnique<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
                          select(ID,userID,hashtags)%>%
                            unnest()%>%select(userID,hashtags)%>%
           mutate(hashtags=cleanHashtags(hashtags))%>%group_by(userID,hashtags)%>%unique()%>%ungroup
# Wieviele Nutzer verwenden einen Hashtag
hashTagUsedByUsers<-hashtagsPerUserUnique%>%group_by(hashtags)%>%summarise(usedByUsers=n())

#How many hashtags are only used by a single user?
nrow(hashTagUsedByUsers%>%filter(usedByUsers<2))/nrow(hashTagUsedByUsers)
# sind 66,17% 
# 34 051 Hashtags 

# String for filtering out Hashtags which are only used by a single user
filterHashtags<-hashTagUsedByUsers%>%filter(usedByUsers<2)%>%select(hashtags)
filterHashtags<-filterHashtags$hashtags


#get the most used Hashtags
top1000Hashtags<-tweets.user.table%>%filter(weeksTillElection>-1)%>%
              select(hashtags)%>%unnest()%>%
                  mutate(hashtags=cleanHashtags(hashtags))%>%
                  group_by(hashtags)%>%summarise(freq=n())%>%top_n(1000)



