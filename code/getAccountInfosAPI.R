getAccountInfos<-function(senders){
  require(twitteR)
  require(httr)
  require(rjson)
  require(devtools)
  require(bit64)
  consumer_key<-c("************")
  consumer_secret<-c("*************")
  access_token<-c("****************")
  access_token_secret<-c("******************")
  
  setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)  
  
  users<-lookupUsers(senders)
  account.infos<- do.call("rbind", lapply(users, as.data.frame))
  #account.infos<-account.infos%>%select(sender_statusesCount=statusesCount,sender_followersCount=followersCount,sender_favoritesCount=favoritesCount,sender_friendsCount=friendsCount,sender_created=created,sender=screenName,sender_listedCount=listedCount)
  #account.infos$sender_created <- (Sys.Date() - as.Date(account.infos$sender_created))
  return(account.infos)
}


require(twitteR)

