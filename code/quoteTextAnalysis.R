 #we need the quotes edgelist as well as the quote text from 

quotes<-quote_response_time%>%select(ID:party.y)
    
#how many quotes per party 
quote_stats<-quotes%>%select(ID,party.x)%>%group_by(party.x)%>%
         summarise(count_per_party=n())%>%ungroup%>%
            mutate(ratio=round((count_per_party/nrow(quotes)*100),2))

####

testi<-bind_cols(rt_stats,quote_stats%>%select(count_per_party,ratio)) 
kable(testi, "latex", booktabs = T)



######################          
to_join_with_quote_text<-tweets.user.table%>%
                    select(ID,text,positveSentimentScore,negativeSentimentScore,charCount:mentionedUsers)


quote_text<-left_join(quotes,to_join_with_quote_text,by=c("ID"="ID"))
original_text<-left_join(quotes,to_join_with_quote_text,by=c("quoted_ID"="ID"))



nrow(quote_text%>%filter(mentionCount!=0))/nrow(quote_text)
# 20% of quotes_contain a mention

#what is the share of own @-mentions in quotes?
# is it used as a mechanism for confrontation or more as forwarding to own party members?

#how many quote RTs contain a mention 


quote_mentions<-quote_text%>%
                    select(ID,party.x,mentionedUsers,mentionCount)%>%unnest()%>%
                      mutate(mentionedUsers=str_replace(mentionedUsers,'@',''))

# some contain of more mentions but just one 
# nrow(quote_mentions) accounts get mentioned 

quote_mentions<-left_join(quote_mentions,user_table,by=c("mentionedUsers"="twitter.handle"))%>%
                      filter(!is.na(party))

nrow(quote_mentions%>%select(ID)%>%distinct)

#How many tweets are mentionig only for forwarding only for confrontation? Mix?
quote_mentions<-quote_mentions%>%distinct%>%filter(mentionCount==1)%>%
                    mutate(quote_use=ifelse(party.x!=party,"CONFRONT","FORWARD"))%>%ungroup%>%
                        #group_by(quote_use)%>%mutate(count_quote_use=n())%>%ungroup%>%
                            group_by(ID)%>%
                              mutate(tweet_quote_use=ifelse(length(unique(quote_use))==2,'MIX',quote_use))%>%
                                select(ID,tweet_quote_use)%>%distinct%>%
                                  group_by(tweet_quote_use)%>%
                                     mutate(count_tweets_quotes=n())

#How many are direct author reply?
author_reply<-quote_text%>%select(ID,twitter.handle,mentionCount,mentionedUsers)%>%
                rename(author=twitter.handle)%>%unnest()%>%filter(mentionCount>1)%>%
                      mutate(mentionedUsers=str_replace(mentionedUsers,'@',''))%>%
                        filter(author!=mentionedUsers)
# 364 are direct author replys 
nrow(author_reply%>%select(ID)%>%distinct)


#confrontation = 

############################################################
# Do a log-odds analysis of which words 
#are more likely to occur across and within party quotes

#Do this for comparison within and across party
quote_text_analysis<-quote_text%>%select(ID,party.x,party.y,text)%>%
                                            mutate(relation=ifelse(party.x==party.y,'WITHIN','ACROSS'))


quote_text_analysis<-quote_text_analysis%>%select(text,relation)%>%
                        mutate(text=cleanTweetText(text))%>%
                          unnest_tokens(input=text,output=tokens,token=stringr::str_split,pattern=" ")

quote_text_analysis<-quote_text_analysis%>%
                        mutate(tokens=na_if(tokens,""))%>%
                            filter(!is.na(tokens))
#get rid of the stopwords 
quote_text_analysis<-quote_text_analysis%>%anti_join(stopwords,by=c("tokens"="X1"))



# do the log-odds ratio 
# keep the @mentions and hashtags in 
# tokens need to have a frequency of 10 
quote_word_ratios <- quote_text_analysis%>%
                        count(tokens, relation)%>%
                           group_by(tokens)%>%
                           filter(sum(n) >= 10)%>%
                      ungroup() %>%
                    spread(relation, n, fill = 0) %>%
                    mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
                    mutate(logratio = log(ACROSS/WITHIN)) %>%
                    arrange(desc(logratio))

# do the log-odds plot
quote_word_ratios %>%
                      group_by(logratio < 0) %>%
                        top_n(15, abs(logratio)) %>%
                          ungroup() %>%
                            mutate(tokens = reorder(tokens, logratio)) %>%
                    ggplot(aes(tokens, logratio, fill = logratio < 0)) +
                      geom_col() + 
                       coord_flip() +
                        ylab("Log Odds Ratio") + xlab('Tokens')+
                             theme_ipsum_rc(base_size=36,axis_title_size = 38,plot_title_size = 38)+theme(legend.position="bottom")+
                                scale_fill_viridis(discrete = TRUE,end=0.7,name = "", labels = c("ACROSS Party", "WITHIN Party"))

############################################
######### do the sentiment plot 

quote_sentiment<-quote_text%>%select(party.x,party.y,positveSentimentScore,negativeSentimentScore)%>%
                      mutate(relation=ifelse(party.x==party.y,'WITHIN','ACROSS'))%>%
                        select(positveSentimentScore,negativeSentimentScore,relation)%>%
                             mutate(sentiment=as.numeric(negativeSentimentScore)+as.numeric(positveSentimentScore))%>% 
                                  select(relation,sentiment)

quote_sentiment%>%group_by(relation)%>%summarise(avg=mean(sentiment))


ggplot(quote_sentiment, aes(relation, sentiment))+
                geom_boxplot(aes(colour=relation),show.legend = FALSE)+
                   stat_summary(fun.y=mean, colour="red", geom="point", 
                                          shape=18, size=3)+
              ylab('Sentiment')+xlab('')+
                       theme_ipsum_rc(base_size=12,axis_title_size = 12,
                                plot_title_size = 12)+
                    scale_color_viridis(discrete = TRUE, end=0.7)



quote_sentiment_RILE<-quote_text%>%
                        select(ID,party.x,party.y,negativeSentimentScore,positveSentimentScore)%>%
                  mutate(RILE.x=ifelse((party.x %in% conservative),"conservative","liberal"),
                      RILE.y=ifelse((party.y %in% conservative),"conservative","liberal"),
                        relation=ifelse(RILE.x==RILE.y,'WITHIN-RILE','ACROSS-RILE'),
                          sentiment=as.numeric(negativeSentimentScore)+as.numeric(positveSentimentScore))%>%
                           select(sentiment,relation)


#across RILE is really rare only 883      
#only 20%
quote_sentiment_RILE%>%group_by(relation)%>%summarise(count=n())


ggplot(quote_sentiment_RILE, aes(relation,sentiment ))+geom_boxplot()+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show_guide = FALSE)


#########################################################
############ What about bigrams? 

#Do this for comparison within and across party
quote_text_analysis_bigram<-quote_text%>%select(ID,party.x,party.y,text)%>%
  mutate(relation=ifelse(party.x==party.y,'WITHIN','ACROSS'))


quote_text_analysis_bigram<-quote_text_analysis_bigram%>%select(text,relation)%>%
  mutate(text=cleanTweetText(text))%>%
  unnest_tokens(input=text,output=tokens,token="ngrams",n=2)

quote_text_analysis_bigram<-quote_text_analysis_bigram%>%
  mutate(tokens=na_if(tokens,""))%>%
  filter(!is.na(tokens))

#split the column in two and get rid of stop words
quote_text_analysis_bigram<-quote_text_analysis_bigram%>%separate(tokens,c("A","B"))

#get rid of the stopwords 
quote_text_analysis_bigram<-quote_text_analysis_bigram%>%anti_join(stopwords,by=c("A"="X1"))
quote_text_analysis_bigram<-quote_text_analysis_bigram%>%anti_join(stopwords,by=c("B"="X1"))

#join the column again 
quote_text_analysis_bigram<-quote_text_analysis_bigram%>%unite('tokens',A:B,sep=" ")

quote_word_ratios_bigram <- quote_text_analysis_bigram%>%
  count(tokens, relation)%>%
  group_by(tokens)%>%
  filter(sum(n) >= 5)%>%
  ungroup() %>%
  spread(relation, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(ACROSS/WITHIN)) %>%
  arrange(desc(logratio))

#############################################################
################ Compare Text of quotes across and within  nchar ntokens urls mentions hashtags 

quote_text_comparison<-quote_text%>%select(party.x,party.y,text,urlCount:mentionCount)%>%
                            mutate(relation=ifelse(party.x==party.y,'WITHIN','ACROSS'),
                                      urlCount=urlCount-1, text=str_replace(text,"http[s]?://t\\.co/[^ ]{10}",""),
                                   charCount= str_length(text),
                                   tokenCount= str_count(text,"\\S+"))

testi<-quote_text_comparison%>%
        select(relation,urlCount:mentionCount,charCount,tokenCount)%>%
            group_by(relation)%>%summarise_all(funs(med=median,avg=mean,max=max,sd=sd))

ggplot(quote_text_comparison, aes(relation,charCount))+geom_boxplot()+
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show_guide = FALSE)



  quote_text_comparison<-quote_text_comparison%>%
                            mutate(relation=as.factor(relation))
  
  sampling_distribution <- quote_text_comparison%>%
    specify(tokenCount ~ relation) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "diff in means", order = c("ACROSS","WITHIN"))
  
  conf_int <- sampling_distribution %>% 
    pull(stat) %>% 
    quantile(probs = c(0.025, 0.975)) 
  
  conf_int
  
  
require(tidyverse)
require(viridisLite)
require(hrbrthemes)
require(viridis)
  
  quote_word_ratios 
  
  log_odds%>%select(-X1)%>%
    group_by(log.odds < 0) %>%
    top_n(15, abs(log.odds)) %>%
    ungroup() %>%
    mutate(Tokens = reorder(Tokens, log.odds)) %>%
    ggplot(aes(Tokens, log.odds, fill = log.odds < 0)) +
    geom_col() + 
    coord_flip() +
    ylab("Log Odds Ratio") + xlab('Tokens')+
    theme_ipsum_rc(base_size=36,axis_title_size = 38,plot_title_size = 38)+theme(legend.position="bottom")+
    scale_fill_viridis(discrete = TRUE,end=0.7,name = "", labels = c("TEXT", "AUDIO"))
  