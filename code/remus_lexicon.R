require("dependencies.R")
pos_words_lexicon<-SentiWS_v1_8c_Positive
names(pos_words_lexicon)<-c("pos","value","variations")
pos_words_lexicon<-pos_words_lexicon%>%mutate(word = str_sub(pos, 1, regexpr("\\|", .$pos)-1),
                      pos = str_sub(pos, start = regexpr("\\|", .$pos)+1))
pos_words_lexicon<-pos_words_lexicon%>%unite(col=words,word,variations,sep=",")
pos_words_lexicon<-pos_words_lexicon%>%unnest_tokens(input = words,output = word, token="words")%>%filter(!word=="na")%>%
                              select(word,value)%>%mutate(word=cleanHashtags(word))


neg_words_lexicon<-SentiWS_v1_8c_Negative
names(neg_words_lexicon)<-c("pos","value","variations")
neg_words_lexicon<-neg_words_lexicon%>%mutate(word = str_sub(pos, 1, regexpr("\\|", .$pos)-1),
                                              pos = str_sub(pos, start = regexpr("\\|", .$pos)+1))
neg_words_lexicon<-neg_words_lexicon%>%unite(col=words,word,variations,sep=",")
neg_words_lexicon<-neg_words_lexicon%>%unnest_tokens(input = words,output = word, token="words")%>%filter(!word=="na")%>%
  select(word,value)%>%mutate(word=cleanHashtags(word))


remus_lexicon<-bind_rows(pos_words_lexicon,neg_words_lexicon)
#as for social media data there are a lot of spelling errors and detecting POS (for german)
# is close to impossible I decided to eliminate doublets (e.g. trauer, trauer | verdient, verdient)
# the value is the mean of two values 
remus_lexicon<-remus_lexicon%>%group_by(word)%>%mutate(value=mean(value))%>%
                  select(word,value)%>%unique

write_csv(remus_lexicon,"remus_lexicon.csv")
