#-------------Packages
require(tidyverse)
require(ggridges)
require(stringr)
require(stringi)
require(igraph)
require(tidygraph)
require(ggraph)
require(DBI)
require(RSQLite)
require(anytime)
require(readr)
#require(ggnetwork)
#require(intergraph)
require(lubridate)
require(viridis)
require(entropy)
require(lazyeval)
require(purrr)
require(ineq)
require(scales)
require(lsa)
require(widyr)
require(tidytext)
require(topicmodels)
require(rvest)
require(broom)
require(hrbrthemes)
#require(kableExtra)

url_pattern <- "http[s]?://t\\.co/[^ ]{10}"
hashtag_pattern <- "#([[:alnum:]]|[_])+"
mention_pattern <- "@([[:alnum:]]|[_])+"
strip_RT_pattern<-"RT\\s@([[:alnum:]]|[_])+:"


#--------------Scripts
source("getAccountInfosAPI.R")
source("cleanHashtags.R")
source("networkToLorenzDF.R")
source("networkToGiniValue.R")
source("networkToClusteringValues.R")
source("networkToAvgPathlength.R")
source("networkToKcore.R")
source("makeEdgelistFromUserHashtags.R")
source("cleanTweetText.R")
source("multiLouvain.R")
source("multiplot.R")
source("gridArrange.R")
source("listCommonTopics.R")
#---------------Strings

party_string<-c("cdu","csu","linke","gruene","afd","fdp","spd")
party_colors<-c(afd="#1eaac2",cdu="#000000",csu="#1804f6",linke="#f402b9",fdp="#ffe14d",gruene="#3fba16",spd="#f60410")
party_colors_de<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffe14d",GRUENE="#3fba16",SPD="#f60410")
party_colors_en<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6", Left="#f402b9",FDP="#ffe14d",Greens="#3fba16",SPD="#f60410")
party_filter<-c("FDP","CDU","CSU","SPD","DIE LINKE","GRUENE","AfD")

topic_number_name<-data.frame(number=c(12,114,15,40,36,59,2,101,18,1,23,100,14,58,104,108,44,54,31,22,73,107,116,113,111,13),
                              name=c("Foreign Policy","Right-Left-Conflict","Environmental Policy",
                                      "Chancelor Debate","Berlin","Regional Elections NRW","Social Justice",
                                      "G20 Summit","CDU in Brandenburg","News & Media","Emission Scandal",
                                      "Education Policy","Bavaria","Baden Wuerrtemberg","Bremen","Sexual Equality","German Football",
                                      "Asylum Policy","Digitalisation","Election Federal President","Economic Policy","Religion & Culture",
                                      "Public Events","Saxony & Saxony-Anhalt","Green Campaign Berlin","Miscellaneous"))

topic_number_name_new<-data.frame(number=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
                                  name=c("Foreign Policy","Right-Left-Conflict","Environmental Policy",
                                         "Chancelor Debate","Berlin","Regional Elections NRW","Social Justice",
                                         "G20 Summit","CDU in Brandenburg","News & Media","Emission Scandal",
                                         "Education Policy","Bavaria","Baden Wuerrtemberg","Bremen","Sexual Equality","German Football",
                                         "Asylum Policy","Digitalisation","Election Federal President","Economic Policy","Religion & Culture",
                                         "Public Events","Saxony & Saxony-Anhalt","Green Campaign Berlin","Miscellaneous"))

#---------- Import politicans
user_table <- read_csv("user_table.csv", 
                                     col_types = cols(id = col_character()), 
                                     comment = "//")

#---------- Import Stopword list 
stopwords<-read_csv("stopwords.txt", col_names = FALSE, comment = "//")

#--------- Import Stentiment Lexicon
remus_lexicon<-read_csv("remus_lexicon.csv")
