library(tm)
library(topicmodels)
dtm <- readRDS("dtm.rds")
lda.clinton <- readRDS("c10.rds")
clinton.topics <- as.matrix(topics(lda.clinton))
clinton.terms <- as.matrix(terms(lda.clinton,20))
idx<-unlist(lapply(which(grepl('clinton', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
cidx <- sort(unique(dtm$i[idx]))
fb_data <- readRDS('fb_data.rds')
posts<-fb_data[[1]]
engagement <- (unlist(fb_data[[3]]))
p_id <- array(NA, length(engagement))
p_id[1:length(posts[[1]])] <- 1
last_id <- length(posts[[1]])
for (i in 2:10) {
  p_id[(last_id+1):(last_id+length(posts[[i]]))] <- i;
  last_id <- last_id+length(posts[[i]]);
}
engagement <- engagement[cidx];
nposts <- length(engagement)
npages <- 10;
ntopics <- 10;
p_id <- p_id[cidx];
t_id <- array(0, c(nposts, ntopics))
for (i in 1:nposts)
  t_id[i, clinton.topics[i]] <- 1;
for (i in 1:npages)
  engagement[p_id==i] = log(engagement[p_id==i]) - mean(log(engagement[p_id==i]))
library(rstan)
rstan_options(auto_write = TRUE);
options(mc.cores = parallel::detectCores());
sm <- stan_model("engagement_topics.stan");
fit_clinton <- sampling(sm,iter=500)
saveRDS(fit_clinton,"fit_clinton.rds")

### PLOTS
credplot.gg <- function(d){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  require(ggplot2)
  p <- ggplot(d, aes(x=x))+
    geom_pointrange(aes(y=y, ymin=ylo, ymax=yhi), fatten = 1, size = 1, color='darkblue')+
    geom_hline(yintercept = 0, linetype=2)+
    coord_flip()+
    xlab('')+
    ylab(d$source) +
    theme_set(theme_gray(base_size = 15))
  return(p)
}
source("multiplot.R")
sf <- summary(fit_clinton)$summary
topic_coef<-c();
for (i in 1:10)
  topic_coef[[i]]<-sf[sprintf("t_coef[%d,%d]", i,1:10),c(5,6,7)]
p_names <- c("ABC", "BBC","CBS", "CNN", "Fox News", "NBC", 
                   "NPR", "New York Times", "Washington Post", "Wall Street Journal")


png ("clinton_topics.png", height=10, width=7, units = 'in', res = 200)
plots<-c()
for (i in 1:10) {
  df_plot<-data.frame(list(ylo=topic_coef[[i]][,1], yhi=topic_coef[[i]][,3], y=topic_coef[[i]][,2], 
                           x=sprintf("topic%2d",1:10), source=p_names[i]))
  plots[[i]]<-credplot.gg(df_plot)
}
multiplot(plotlist = plots, cols = 2)
invisible(dev.off())

#### trump

library(tm)
library(topicmodels)
dtm <- readRDS("dtm.rds")
lda.trump <- readRDS("t10.rds")
trump.topics <- as.matrix(topics(lda.trump))
trump.terms <- as.matrix(terms(lda.trump,20))
idx<-unlist(lapply(which(grepl('trump', dtm$dimnames$Terms)),  function(x) which(dtm$j %in% x)))
tidx <- sort(unique(dtm$i[idx]))
fb_data <- readRDS('fb_data.rds')
posts<-fb_data[[1]]
engagement <- log(unlist(fb_data[[3]]))
p_id <- array(NA, length(engagement))
p_id[1:length(posts[[1]])] <- 1
last_id <- length(posts[[1]])
for (i in 2:10) {
  p_id[(last_id+1):(last_id+length(posts[[i]]))] <- i;
  last_id <- last_id+length(posts[[i]]);
}
engagement <- engagement[tidx];
npages <- 10;
ntopics <- 10;
p_id <- p_id[tidx];
dtm.trump <- dtm[ sort(unique(dtm$i[idx])),]
dtm.trump <- removeSparseTerms(dtm.trump,0.995)
ui = unique(dtm.trump$i)
engagement<-engagement[ui]
p_id <- p_id[ui]
nposts <- length(engagement)
t_id <- array(0, c(nposts, ntopics))
for (i in 1:nposts)
  t_id[i, trump.topics[i]] <- 1;
for (i in 1:npages)
  engagement[p_id==i] = log(engagement[p_id==i]) - mean(log(engagement[p_id==i]))
library(rstan)
rstan_options(auto_write = TRUE);
options(mc.cores = parallel::detectCores());
sm <- stan_model("engagement_topics.stan");
fit_trump <- sampling(sm,iter=500)
saveRDS(fit_trump,"fit_trump.rds")

### PLOTS
credplot.gg <- function(d){
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  require(ggplot2)
  p <- ggplot(d, aes(x=x))+
    geom_pointrange(aes(y=y, ymin=ylo, ymax=yhi), fatten = 1, size = 1, color='darkred')+
    geom_hline(yintercept = mean(d$y), linetype=2)+
    coord_flip()+
    ylab(d$source) +
    xlab('')+
    theme_set(theme_gray(base_size = 15))
  return(p)
}

sf <- summary(fit_trump)$summary
topic_coef<-c();
for (i in 1:10)
  topic_coef[[i]]<-sf[sprintf("t_coef[%d,%d]", i,1:10),c(5,6,7)]
p_names <- c("ABC", "BBC","CBS", "CNN", "Fox News", "NBC", 
             "NPR", "New York Times", "Washington Post", "Wall Street Journal")


png ("trump_topics.png", height=10, width=7, units = 'in', res = 200)
plots<-c()
for (i in 1:10) {
  df_plot<-data.frame(list(ylo=topic_coef[[i]][,1], yhi=topic_coef[[i]][,3], y=topic_coef[[i]][,2], 
                           x=sprintf("topic%2d",1:10), source=p_names[i]))
  plots[[i]]<-credplot.gg(df_plot)
}
multiplot(plotlist = plots, cols = 2)
invisible(dev.off())






