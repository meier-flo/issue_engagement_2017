source('dependencies.R')
source('cleanTweetText.R')
require(irlba)
require(broom)
require(hrbrthemes)
#we have the old function that cleans the text i.e. every 


tweets_tokens<-tweets.user.table%>%filter(!(ID%in%self_talk$ID))%>%
                      filter(weeksTillElection>-1,tokenCount>2)%>%
                        select(text,ID)%>%
                          mutate(text=cleanTweetText(text))%>%
                            unnest_tokens(input=text,output=tokens,token=stringr::str_split,pattern=" ")

tweets_tokens<-tweets_tokens%>%
                      mutate(tokens=na_if(tokens,""))%>%
                        filter(!is.na(tokens))
#get rid of the stopword 
tweets_tokens<-tweets_tokens%>%anti_join(stopwords,by=c("tokens"="X1"))

#self_talk data frame tells you 
#self_talk<-tweets_tokens%>%mutate(party=str_to_lower(party))
#self_talk<-self_talk%>%mutate(party=str_replace(party,'die ',""))
#self_talk<-self_talk%>%filter(party==tokens)



#do everything once for

#calc simple word probabilities
unigram_probs<-tweets_tokens%>%
                  count(tokens,sort = TRUE)%>%
                    mutate(p=n/sum(n))
                  
                    

#calc the probabilities of co-occurence
coocc_probs<-tweets_tokens%>%
                pairwise_count(tokens,ID,diag=FALSE,upper=FALSE,sort=TRUE)%>%
                  mutate(p=n/sum(n))



normalized_prob <- coocc_probs %>%
                     filter(n > 20) %>%
                        rename(word1 = item1, word2 = item2) %>%
                          left_join(unigram_probs %>%
                                select(word1 = tokens, p1 = p), by = "word1") %>%
                                 left_join(unigram_probs %>%
                                  select(word2 = tokens, p2 = p),by = "word2") %>%
                                    mutate(p_together = p / p1 / p2)
View(normalized_prob)
#cast into a sparse matrix
pmi_matrix <- normalized_prob %>%
                 mutate(pmi = log10(p_together)) %>%
                  cast_sparse(word1, word2, pmi)

#do the single value decomposition
pmi_svd <- irlba(pmi_matrix, 75, maxit = 1000)
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)



getCompletePartyDF<-function(word_vectors,query_list){
  complete_df<-tibble(token=vector("character"),
                        similarity=vector("double"),
                          party=vector("character"))
  
#This is the search function to get out the vectors and similarity
        search_synonyms <- function(word_vectors, selected_vector) {
              similarities <- word_vectors %*% selected_vector %>%
                tidy() %>%
                as_tibble() %>%
                rename(token = .rownames,
                       similarity = unrowname.x.)
              similarities %>%
                arrange(-similarity)    
            }
  
      for(i in query_list){
          print(i)
          dummy_df<- search_synonyms(word_vectors, word_vectors[i,])
          print(dummy_df)
          dummy_df<-dummy_df%>%mutate(party=i)
          complete_df<-bind_rows(complete_df,dummy_df)
       }
      
  complete_df<-complete_df%>%group_by(party)%>%
                  top_n(15, similarity)%>%mutate%>%ungroup%>%
                     mutate(token=reorder(token, similarity))
  
  return(complete_df)
}

query_list<-c("afd","cdu","csu","fdp","spd","gruene","linke")


#get the complete df
party_wordvector_df<-getCompletePartyDF(word_vectors,query_list)
View(party_wordvector_df)

party_wordvector_df<-party_wordvector_df%>%filter(token!=party)

#
ggplot(party_wordvector_df,aes(token, similarity, fill = party)) +
          geom_col(show.legend = FALSE) +
            facet_wrap(~party, scales = "free") +
              coord_flip()+
                theme(strip.text=element_text(hjust=0, family="Roboto-Bold", size=12)) +
                   scale_y_continuous(expand = c(0,0))+scale_fill_manual(values=party_colors)+
                      labs(x = NULL, title = "What word vectors are most related to partys? - When other talk about them",
                        subtitle = "Based on Tweets by politicians, calculated using counts and matrix factorization")+
                        theme_ipsum()
