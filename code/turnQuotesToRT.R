 # we need to merge every tweet from tweets.user.table with
# which has a quote=1 with 

# we need complete urls from urls
# we need rtJoinHelper from RT analyse 

merge_RT<-urls_complete%>%select(X1,X3)

quote_RT<-tweets.user.table%>%filter(isQuote=='1')%>%
                select(ID,twitter.handle,isRetweet,isQuote)

merge_quote_URL_RT<-full_join(quote_RT,merge_RT,by=c('ID'='X1'))

#RT_test_1<-full_join(RT_test,tweetRtRelation,by=c('ID'='rtID'))
#RT_test_1<-full_join(RT_test_1,merge_RT,by=c('ID'='X1'))


# NA in original Tweet means it is not a RT 
# lets split the URL into columns on /
#testi<-RT_test_1%>%filter(isQuote=='1')

merge_quote_URL_RT<-separate(merge_quote_URL_RT,X3,into=c('A','B','C','D','E','F'),sep="/")%>%
            mutate(quoted_twitter.handle=D,quoted_ID=F)%>%
              select(ID,twitter.handle,isQuote,quoted_twitter.handle,quoted_ID)

merge_quote_URL_RT<-merge_quote_URL_RT%>%filter(quoted_twitter.handle!='i')


merge_quote_URL_RT<-left_join(merge_quote_URL_RT,rtJoinHelper_RTNetwork,by=c('ID'='ID'))
merge_quote_URL_RT<-left_join(merge_quote_URL_RT,rtJoinHelper_RTNetwork,by=c('quoted_ID'='ID'))

#In total the we observe  4.500 are among politicans 
# the others quotes from media outlets etc. 
#now we can filter out the true quotes and only those for politicans 
merge_quote_URL_RT_Final<-merge_quote_URL_RT%>%
                              filter(isQuote==1)%>%
                                filter(!is.na(twitter.handle))%>%
                                  select(ID,twitter.handle.x,party.x,quoted_ID,twitter.handle,party.y)


merge_quote_URL_RT_Final<-merge_quote_URL_RT_Final%>%distinct







