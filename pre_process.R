library(tm)
library(topicmodels)
fb_data <- readRDS('fb_data.rds')
aa<-unlist(fb_data[[1]])
docs <- Corpus(VectorSource(aa))
#start preprocessing
#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))
#remove potentially problematic symbols
noLink <- content_transformer(function(x) { 
  return (gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x))})
docs <- tm_map(docs, noLink)
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "_")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "#")
docs <- tm_map(docs, toSpace, "<.*?>")
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
myStopwords <- c("can", "say","one","way","use",
                  "also","however","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","ve ",
                  "re ","anoth","put","set","new","news","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "little","ever","moreov","though","found","able",
                  "enough","far","earli","away","achieve","draw",
                  "last","never","brief","bit","entire","brief",
                  "great","lot","can", "three", "just", "abc", "fox", "cnn",
                 "week", "now","york", "photo", "photos", "timeline","today",
                 "times", "breaking", "people", "calls", "called", "make", "made",
                 "man","told","year","many","best","every","former","back","tuesday",
                 "wins","won","says","don","going","know","saying","today","tonight",
                 "doesn", "check", "face", "check", "time", "talk", "making", "city",
                 "old", "big", "takes", "ahead", "real","got","years","took")
docs <- tm_map(docs, removeWords, myStopwords)
dtm <- DocumentTermMatrix(docs)
saveRDS(dtm, "dtm.rds")

