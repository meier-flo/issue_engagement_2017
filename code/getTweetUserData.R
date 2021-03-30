source("dependencies.R")
#readability and lang detect
#source("getReadabilityScore.R")


#####---------- get data from db --------------------
tweet.con<-dbConnect(SQLite(), dbname="tweets.sqlite")
dbListTables(tweet.con)

#get the tweet table
tweets<-dbGetQuery(tweet.con,"SELECT *, CAST(id as TEXT) as ID, 
                   CAST(createdAt as TEXT) as createdAT,
                   CAST(insertedAt as TEXT) as insertedAT,
                   CAST(userId as TEXT) as userID
                   FROM tweet WHERE createdAt > 1483228800000")
#drop the original ones
tweets<-select(tweets,-(one_of("id","createdAt","insertedAt","userId")))

#get the user table
#user<-dbGetQuery(tweet.con,"SELECT *, CAST(id as TEXT) as userID, 
#                   CAST(createdAt as TEXT) as createdAT
#                 FROM user")

#drop the original columns 
#user<-select(user,-c(id,createdAt))


dbDisconnect(tweet.con)
#-----------------------------------------


#cleaning going on here
#1. strip the apostrophs at the beginning and end 
tweets<-mutate_all(tweets,funs(str_replace_all(.,"\"","")))
#user<-mutate_each(user,funs(str_replace_all(.,"\"","")))


#----
#------
#---  
# Es müssen noch weitere Säuberungen vorgenommen werden
# Es gibt Tweets von 18 000 Nutzern - wie kann das sein?
# z.B. tauchen Tweets von Nutzern auf die von einem Politiker dem wir folgen geliked wurden
# Sicherlicher auch Werbung drin Daten nur nach JOIN mit Nutzertabelle nutzbar
# Viele (nicht alle) der RTs sind gecuttet so dass Hashtags, Links etc. fehlen
#----------
#load the user_table.csv if necessary 

#### join with user_table from csv

tweets.user.table<-inner_join(tweets,user_table,by=c("userID"="id"))



####### Further characteristics of tweets nChars, nHash, nURL etc. 
# we need reg expressions for counting/extracting the different entities 
# entities are also stored in column lists 
# we can use unnest() so unlist the lists
#media_pattern<-"twitter.com/i/web/status/"

#aus jedem Tweet muss noch RT @xyz: weggeparst werden falls RT = 1 #RT @ muss noch gestrippt werden
#bzw. muss bei jedme RT der Originaltweetgeparst werden für Vollständigkeit

#get the tweet-RT-relation table for replacing RT Text that is cropped
# CAUTION sourceID is the RT ID
# tweetID is the ID of the original Tweet 


#####---------- get data from db --------------------
tweet.con<-dbConnect(SQLite(), dbname="tweets.sqlite")
dbListTables(tweet.con)
tweetRtRelation<-dbGetQuery(tweet.con,"SELECT CAST(sourceId as TEXT) as rtID, 
                            CAST(tweetId as TEXT) as originalTweetID
                            FROM sources_to_tweets") 
dbDisconnect(tweet.con)
####--------------------------------------------------

# if the ID is a RT ID than it's a RT ... 
#join with the relation table and then again with user with X RT's Y

rtJoinHelper<-tweets.user.table%>%select(text,ID)

replaceTextWithOriginal<-left_join(rtJoinHelper,tweetRtRelation,by=c("ID"="rtID"))%>%
                          filter(!is.na(originalTweetID))
replaceTextWithOriginal<-left_join(replaceTextWithOriginal,rtJoinHelper,by=c("originalTweetID"="ID"))
replaceTextWithOriginal<-replaceTextWithOriginal%>%select(ID,originalText=text.y)

tweets.user.table<-left_join(tweets.user.table,replaceTextWithOriginal,by="ID")

tweets.user.table<-tweets.user.table%>%mutate(
                      #text=ifelse(isRetweet==1&(!is.na(originalText)),originalText,str_replace_all(text,strip_RT_pattern,replacement = "")),
                      createdAT=anytime((as.numeric(createdAT)/1000),tz = "Europe/Berlin"),
                      weeksTillElection=as.integer(difftime(as_date("2017-09-24"),createdAT,units="weeks")),                      
                      insertedAT=anytime((as.numeric(createdAT)/1000),tz = "Europe/Berlin"),
                      charCount= str_length(text),
                      tokenCount= str_count(text,"\\S+"),
                      urlCount = str_count(text,url_pattern),
                      hashtagCount = str_count(text,hashtag_pattern),
                      mentionCount = str_count(text,mention_pattern),
                      urls = str_extract_all(text,url_pattern),
                      hashtags =  str_extract_all(text,hashtag_pattern),
                      mentionedUsers = str_extract_all(text,mention_pattern),
                      party=ifelse(party=="FDP"|party=="FDP DVP","FDP",party))%>%
                      filter(party %in% party_filter)%>%filter(!(str_detect(text,pattern = "followed")))
%>%
                      #select(-originalText)

# Doppelhashtags entfernen  -> vielleicht das erst später

#############
#---------------------
# Here we add sentiment_values of eacht Tweet based on the remus_lexicon
sentiment_analysis<-tweets.user.table%>%select(ID,text)%>%
                        mutate(text=cleanTweetText(text))%>%
                          unnest_tokens(input=text,output=word,token=stringr::str_split,pattern=" ")                    
# Delete GERMAN Stopwords Delete ENGLISH Stopwords
    sentiment_analysis<-sentiment_analysis%>%anti_join(stopwords,by=c("word"="X1"))
# Get the sentimentvalues for every word and calc value for ID 
  sentiment_analysis<-inner_join(sentiment_analysis,remus_lexicon,by="word")
#nrow(sentiment_analysis%>%filter(is.na(value)))/nrow(sentiment_analysis)
  sentiment_analysis<-sentiment_analysis%>%group_by(ID)%>%summarise(senti_value=sum(value))
  
#join with original Tweets Table   
tweets.user.table<-left_join(tweets.user.table,sentiment_analysis,by="ID")  

# FIll the ones left NA with 0
tweets.user.table<-tweets.user.table%>%mutate(senti_value=ifelse(is.na(senti_value),0,senti_value))


