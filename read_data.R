Sys.setlocale('LC_ALL','C') 
fb_page_names <- c("abc.csv", "bbc.csv","cbs.csv", "cnn.csv", "fox.csv", "nbc.csv", 
                   "npr.csv", "nyt.csv", "wap.csv", "wsj.csv")
fb_post <- list();
fb_reaction <- list();
fb_total_reaction <- list();
fb_month <- list();
fb_day <- list();
for (i in 1:length(fb_page_names)) {
  data<-read.csv(paste("data/",fb_page_names[i],sep=""),
                 stringsAsFactors=FALSE, header=FALSE, skip=1,sep=",")
  post <- paste(data[,6], data[,8], data[,10])
  reaction <- matrix(NA, length(post), 9)
  for (j in 1:9) {
    reaction[,j] <- as.numeric(data[,(j*2 + 16)])
  }
  all_dates <- as.Date(data[,40], format="%m/%d/%Y")
  all_years <- as.numeric(format(all_dates,"%Y")) 
  all_months <- as.numeric(format(all_dates,"%m"))
  all_days <- as.numeric(format(all_dates,"%d"))
  all_years[is.na(all_years)] <- 0;
  all_months[is.na(all_months)] <- 0;
  all_days[is.na(all_days)] <- 0;
  post <- iconv(post, to="UTF-8")
  idx <- (!is.na(post)) & (all_years==16) & (all_months>0) & (all_days>0)
  post <- post[idx]
  all_years <- all_years[idx]
  all_months <- all_months[idx]
  all_days <- all_days[idx]
  reaction <- reaction[idx,]
  total_reaction <- rowSums(reaction)
  fb_post[[i]] <- post
  fb_reaction[[i]] <- reaction
  fb_total_reaction[[i]] <- total_reaction
  fb_month[[i]] <- all_months
  fb_day[[i]] <- all_days   
}
fb_data <- list(fb_post, fb_reaction, fb_total_reaction, fb_month, fb_day)
saveRDS(fb_data,'fb_data.rds')