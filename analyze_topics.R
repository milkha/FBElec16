library(tm)
library(topicmodels)
lda.trump <- readRDS("t10.rds")
lda.clinton <- readRDS("c10.rds")

clinton.topics <- as.matrix(topics(lda.clinton))
clinton.terms <- as.matrix(terms(lda.clinton,20))
trump.topics <- as.matrix(topics(lda.trump))
trump.terms <- as.matrix(terms(lda.trump,20))

int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
int.hist(clinton.topics)
int.hist(trump.topics)


## Clinton topic evolutions
library(tm)
library(topicmodels)
fb_data <- readRDS('fb_data.rds')
months <- (unlist(fb_data[[4]]))
dtm <- readRDS("dtm.rds")
idx<-unlist(lapply(which(grepl('clinton', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
cidx <- sort(unique(dtm$i[idx]))
months <- months[cidx]
posts<-fb_data[[1]]
p_id <- array(NA, length(unlist(posts)))
p_id[1:length(posts[[1]])] <- 1
last_id <- length(posts[[1]])
for (i in 2:10) {
  p_id[(last_id+1):(last_id+length(posts[[i]]))] <- i;
  last_id <- last_id+length(posts[[i]]);
}

p_id <- p_id[cidx]

c_topics <- array(NA, c(11, 10))
for (m in 1:11)
  for (t in 1:10)
    c_topics[m,t] <- sum(clinton.topics[months==m]==t)/length(clinton.topics[months==m]);
library(ggplot2)
library(reshape2)
library(scales)
library(ggthemes)
ct <- data.frame(t(c_topics))
rownames(ct) <- sprintf("Topic %2d", 1:10)
colnames(ct) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov")
datm <- melt(cbind(ct, Topics = rownames(ct)), id.vars = c('Topics'))
png ("c_topic_evol.png", height=5, width=7, units = 'in', res = 200)
ggplot(datm,aes(x = variable, y = value, fill=Topics)) + 
       geom_bar(position = "fill",stat = "identity") + 
       scale_y_continuous(labels = percent_format()) +
       scale_fill_manual("Clinton", 
                         values = c("forestgreen", "orange", "darkblue","red", 
                                    "gold4", "mediumpurple4","cyan4", "darkorange3", 
                                    "blue","firebrick4")) +
     labs(x="",y="Coverage percentage") +
  theme_set(theme_gray(base_size = 12))
dev.off()



c_topics <- array(NA, c(10, 10))
for (m in 1:10)
  for (t in 1:10)
    c_topics[m,t] <- sum(clinton.topics[p_id==m]==t)/length(clinton.topics[p_id==m]);
library(ggplot2)
library(reshape2)
library(scales)
library(ggthemes)
ct <- data.frame(t(c_topics))
rownames(ct) <- sprintf("Topic %2d", 1:10)
colnames(ct) <- c("ABC", "BBC","CBS", "CNN", "Fox", "NBC", 
                  "NPR", "NYT", "WP", "WSJ")
datm <- melt(cbind(ct, Topics = rownames(ct)), id.vars = c('Topics'))
png ("c_topic_news.png", height=5, width=7, units = 'in', res = 200)
ggplot(datm,aes(x = variable, y = value, fill=Topics)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual("Clinton", 
                    values = c("forestgreen", "orange", "darkblue","red", 
                               "gold4", "mediumpurple4","cyan4", "darkorange3", 
                               "blue","firebrick4")) +
  labs(x="",y="Coverage percentage") +
  theme_set(theme_gray(base_size = 12))
dev.off()



## Trump topic evolutions
library(tm)
library(topicmodels)
fb_data <- readRDS('fb_data.rds')
months <- (unlist(fb_data[[4]]))
dtm <- readRDS("dtm.rds")
idx<-unlist(lapply(which(grepl('trump', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
tidx <- sort(unique(dtm$i[idx]))
months <- months[tidx]
dtm.trump <- dtm[ sort(unique(dtm$i[idx])),]
dtm.trump <- removeSparseTerms(dtm.trump,0.995)
ui = unique(dtm.trump$i)
months <- months[ui]

posts<-fb_data[[1]]
p_id <- array(NA, length(unlist(posts)))
p_id[1:length(posts[[1]])] <- 1
last_id <- length(posts[[1]])
for (i in 2:10) {
  p_id[(last_id+1):(last_id+length(posts[[i]]))] <- i;
  last_id <- last_id+length(posts[[i]]);
}

p_id <- p_id[tidx]
p_id <- p_id[ui]

t_topics <- array(NA, c(11, 10))
for (m in 1:11)
  for (t in 1:10)
    t_topics[m,t] <- sum(trump.topics[months==m]==t)/length(trump.topics[months==m]);

library(reshape2)
library(scales)
library(ggthemes)
ct <- data.frame(t(t_topics))
rownames(ct) <- sprintf("Topic %2d", 1:10)
colnames(ct) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov")
datm <- melt(cbind(ct, Topics = rownames(ct)), id.vars = c('Topics'))
png ("t_topic_evol.png", height=5, width=7, units = 'in', res = 200)
ggplot(datm,aes(x = variable, y = value, fill=Topics)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual("Trump", values = c("forestgreen", "orange", "darkblue","red", "gold4", "mediumpurple4","cyan4", "darkorange3", "blue","firebrick4")) +
  labs(x="",y="Coverage percentage") +
  theme_set(theme_gray(base_size = 18))
dev.off()



t_topics <- array(NA, c(10, 10))
for (m in 1:10)
  for (t in 1:10)
    t_topics[m,t] <- sum(trump.topics[p_id==m]==t)/length(trump.topics[p_id==m]);
library(ggplot2)
library(reshape2)
library(scales)
library(ggthemes)
ct <- data.frame(t(t_topics))
rownames(ct) <- sprintf("Topic %2d", 1:10)
colnames(ct) <- c("ABC", "BBC","CBS", "CNN", "Fox", "NBC", 
                  "NPR", "NYT", "WP", "WSJ")
datm <- melt(cbind(ct, Topics = rownames(ct)), id.vars = c('Topics'))
png ("t_topic_news.png", height=5, width=7, units = 'in', res = 200)
ggplot(datm,aes(x = variable, y = value, fill=Topics)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual("Trump", 
                    values = c("forestgreen", "orange", "darkblue","red", 
                               "gold4", "mediumpurple4","cyan4", "darkorange3", 
                               "blue","firebrick4")) +
  labs(x="",y="Coverage percentage") +
  theme_set(theme_gray(base_size = 38))
dev.off()

