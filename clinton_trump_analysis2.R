library(tm)
library(topicmodels)
dtm<-readRDS("dtm.rds")
idx<-unlist(lapply(which(grepl('clinton', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
dtm.clinton <- dtm[ sort(unique(dtm$i[idx])),]
dtm.clinton <- removeSparseTerms(dtm.clinton,0.995)
ui.clinton = unique(dtm.clinton$i)
dtm.clinton = dtm.clinton[ui.clinton,]
idx<-unlist(lapply(which(grepl('trump', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
dtm.trump <- dtm[ sort(unique(dtm$i[idx])),]
dtm.trump <- removeSparseTerms(dtm.trump,0.995)
ui.trump = unique(dtm.trump$i)
dtm.trump = dtm.trump[ui.trump,]

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 6000
thin <- 20
seed <-c(1:10)
nstart <- 10
best <- TRUE
#Number of topics
k <- 20

# Clinton
lda.clinton <-LDA(dtm.clinton,k, method="Gibbs", 
                  control=list(nstart=nstart, seed = seed, 
                               best=best, burnin = burnin, iter = iter, thin=thin))
clinton.topics <- as.matrix(topics(lda.clinton))
clinton.terms <- as.matrix(terms(lda.clinton,20))

# Trump
lda.trump <-LDA(dtm.trump,k, method="Gibbs", 
                control=list(nstart=nstart, seed = seed, 
                             best=best, burnin = burnin, iter = iter, thin=thin))
trump.topics <- as.matrix(topics(lda.trump))
trump.terms <- as.matrix(terms(lda.trump,20))

int.hist = function(x,ylab="Frequency",...) {
  barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);axis(1)
}
