source("dependencies.R")
require(modelr)
require(purrr)
# we do 10-fold cross-validation with logistic regression
# we build a balanced classification dataset  of 2000 politician pairs with 1000 single link and 1000 no link
# we build a balanced classification dataset of 2000 with 1000 mutual link and 1000 no link 

set.seed(12345)
single_edge_df_true<-predictEdgesFeatureDF_total%>%filter(single_edge==1&mutual_edge==0)%>%sample_n(1000,replace = FALSE)
single_edge_df_false<-predictEdgesFeatureDF_total%>%filter(single_edge==0&mutual_edge==0)%>%sample_n(1000,replace = FALSE)
single_edge_df<-bind_rows(single_edge_df_true,single_edge_df_false)
#get rid of the user names 
single_edge_df<-single_edge_df%>%mutate(single_edge=as.factor(single_edge))%>%
                                  select(-c(user1,user2,mutual_edge))
                                         
#make a tibble of the six feature combinations                                          
single_edge_features_dfs<-tibble(feature_group=c("issue_usage","sim_measures","diff_rile","same_party","diff_party_same_RILE","all_rile","all_party","all_diff_party_same_RILE"),
                                 data_frames=list(single_edge_df%>%select(min_common_topic:n,single_edge),
                                                  single_edge_df%>%select(cosine_sim:manhattan_dist,single_edge),
                                                  single_edge_df%>%select(diff_RILE,single_edge),
                                                  single_edge_df%>%select(same_party,single_edge),
                                                  single_edge_df%>%select(diff_party_same_RILE,single_edge),
                                                  single_edge_df%>%select(min_common_topic:manhattan_dist,diff_RILE,single_edge),
                                                  single_edge_df%>%select(min_common_topic:manhattan_dist,same_party,single_edge),
                                                  single_edge_df%>%select(min_common_topic:manhattan_dist,diff_party_same_RILE,single_edge)
                                                  ))

mutual_edge_df_true<-predictEdgesFeatureDF_total%>%filter(mutual_edge==1)%>%sample_n(1000,replace = FALSE)
mutual_edge_df_false<-predictEdgesFeatureDF_total%>%filter(mutual_edge==0)%>%sample_n(1000,replace = FALSE)
mutual_edge_df<-bind_rows(mutual_edge_df_true,mutual_edge_df_false)
#get rid of the user names 
mutual_edge_df<-mutual_edge_df%>%
                            mutate(mutual_edge=as.factor(mutual_edge))%>%
                                select(-c(user1,user2,single_edge))

#make a tibble of the six feature combinations 
mutual_edge_features_dfs<-tibble(feature_group=c("issue_usage","sim_measures","diff_rile","same_party","diff_party_same_RILE","all_rile","all_party","all_diff_party_same_RILE"),
                                 data_frames=list(mutual_edge_df%>%select(min_common_topic:n,mutual_edge),
                                                  mutual_edge_df%>%select(cosine_sim:manhattan_dist,mutual_edge),
                                                  mutual_edge_df%>%select(diff_RILE,mutual_edge),
                                                  mutual_edge_df%>%select(same_party,mutual_edge),
                                                  mutual_edge_df%>%select(diff_party_same_RILE,mutual_edge),
                                                  mutual_edge_df%>%select(min_common_topic:manhattan_dist,diff_RILE,mutual_edge),
                                                  mutual_edge_df%>%select(min_common_topic:manhattan_dist,same_party,mutual_edge),
                                                  mutual_edge_df%>%select(min_common_topic:manhattan_dist,diff_party_same_RILE,mutual_edge,mutual_edge)
                                                  ))


calcAccuracy<-function(single_edge_df){
        trained.models<-single_edge_df%>%
                          crossv_kfold(10)%>%
                            mutate(model = purrr::map(train,~glm(single_edge ~ ., data=., family=binomial)))
        test.predictions<-trained.models %>%
                            unnest( fitted = map2(model, test, ~augment(.x, newdata = .y)),
                             pred = map2( model, test, ~predict( .x, .y, type = "response")) )
        prediction_result<-test.predictions %>%
                    select(.id, single_edge, pred ) %>%
                      mutate(pred = ifelse(pred >= 0.5, "1", "0"))%>%
                          group_by(.id, single_edge, pred) %>% tally()%>%
                            mutate(class = ifelse(single_edge == pred & pred == 1, "TP",
                                           ifelse(single_edge != pred & pred == 1, "FP",
                                           ifelse(single_edge == pred & pred == 0, "TN", "FN")))) %>%
                                            ungroup()%>%
                                              select(.id, n, class)%>%
                                                spread(class, n)%>%
                                                  replace_na(list(TP=0, TN=0, FP=0, FN=0))%>%
                                                    group_by(.id)%>%
                                                      summarise(Accuracy = (TP+TN)/(TP+TN+FP+FN))%>%
                                                        summarise(mean(Accuracy))
}


single_edge_features_dfs<-single_edge_features_dfs%>%
                        mutate(acc=unlist(map(data_frames,calcAccuracy)))

calcAccuracy_mutual<-function(mutual_edge_df){
  trained.models<-mutual_edge_df%>%
    crossv_kfold(10)%>%
    mutate(model = purrr::map(train,~glm(mutual_edge ~ ., data=., family=binomial)))
  test.predictions<-trained.models %>%
    unnest( fitted = map2(model, test, ~augment(.x, newdata = .y)),
            pred = map2( model, test, ~predict( .x, .y, type = "response")) )
  prediction_result<-test.predictions %>%
    select(.id, mutual_edge, pred ) %>%
    mutate(pred = ifelse(pred >= 0.5, "1", "0"))%>%
    group_by(.id, mutual_edge, pred) %>% tally()%>%
    mutate(class = ifelse(mutual_edge == pred & pred == 1, "TP",
                          ifelse(mutual_edge != pred & pred == 1, "FP",
                                 ifelse(mutual_edge == pred & pred == 0, "TN", "FN")))) %>%
    ungroup()%>%
    select(.id, n, class)%>%
    spread(class, n)%>%
    replace_na(list(TP=0, TN=0, FP=0, FN=0))%>%
    group_by(.id)%>%
    summarise(Accuracy = (TP+TN)/(TP+TN+FP+FN))%>%
    summarise(mean(Accuracy))
}


mutual_edge_features_dfs<-mutual_edge_features_dfs%>%
                            mutate(acc=unlist(map(data_frames,calcAccuracy_mutual)))

str(single_edge_df)

testi<-mutual_edge_df%>%select(-diff_RILE)

model <- glm(mutual_edge ~.,family=binomial(link='logit'),data=testi)
summary(model)




######################
## FFTree
######################
heart.test<-heart.test
heart.train<-heart.train

heart.fft <- FFTrees(formula = diagnosis ~ .,           # Criterion and (all) predictors
                     data = heart.train,                # Training data
                     data.test = heart.test,            # Testing data
                     main = "Heart Disease",            # General label
                     decision.labels = c("Low-Risk", "High-Risk")) 


edges.data<-predictEdgesFeatureDF_total%>%
                filter(single_edge==1)%>%select(-c(user1,user2,mutual_edge))

#%>%sample_n(500,replace=FALSE)

no.edges.data<-predictEdgesFeatureDF_total%>%filter(single_edge==0)%>%
                  select(-c(mutual_edge,user1,user2))%>%sample_n(nrow(edges.data),replace=FALSE)
predict_edges_data<-bind_rows(edges.data,no.edges.data)

set.seed(1234)
predict_edges_data_train<-predict_edges_data%>%sample_frac(size =0.7,replace=FALSE)
predict_edges_data_test<-anti_join(predict_edges_data,predict_edges_data_train)

edge.fft<-FFTrees(formula = single_edge~.,
                  data=predict_edges_data_train,
                  data.test=predict_edges_data_test,
                  main = "@-Mention Edge",
                  decision.labels = c("No @-Mention", "@-Mention")) 


plot(edge.fft,what="cues",data="test")

plot(edge.fft,data="test",what="cues",tree = 6,cue.cex = 1.2)
