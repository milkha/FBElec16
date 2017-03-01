library(Hmisc)
cs<-summary(fit_clinton)$summary
ctsigma <- cs[c("t_sigma[1]","t_sigma[2]","t_sigma[3]","t_sigma[4]","t_sigma[5]",
               "t_sigma[6]","t_sigma[7]","t_sigma[8]","t_sigma[9]","t_sigma[10]"),
             c(1,3,5,6,7)]

ctsigma<-format(round(ctsigma, 2), nsmall = 2)
rownames(ctsigma)<-sprintf("Topic %2d", 1:10)
ctsigma<- ctsigma[order(ctsigma[,1]),]
latex(ctsigma,fil="")


cs<-summary(fit_trump)$summary
ctsigma <- cs[c("t_sigma[1]","t_sigma[2]","t_sigma[3]","t_sigma[4]","t_sigma[5]",
                "t_sigma[6]","t_sigma[7]","t_sigma[8]","t_sigma[9]","t_sigma[10]"),
              c(1,3,5,6,7)]

ctsigma<-format(round(ctsigma, 2), nsmall = 2)
rownames(ctsigma)<-sprintf("Topic %2d", 1:10)
ctsigma<- ctsigma[order(ctsigma[,1]),]
latex(ctsigma,fil="")

##########

cs<-summary(fit_clinton)$summary
ctsigma <- cs[c("e_sigma[1]","e_sigma[2]","e_sigma[3]","e_sigma[4]","e_sigma[5]",
                "e_sigma[6]","e_sigma[7]","e_sigma[8]","e_sigma[9]","e_sigma[10]"),
              c(1,3,5,6,7)]

ctsigma<-format(round(ctsigma, 2), nsmall = 2)
rownames(ctsigma)<-c("ABC", "BBC","CBS", "CNN", "Fox", "NBC", 
                     "NPR", "NYT", "WP", "WSJ")
ctsigma<- ctsigma[order(ctsigma[,1]),]
latex(ctsigma,fil="")


cs<-summary(fit_trump)$summary
ctsigma <- cs[c("e_sigma[1]","e_sigma[2]","e_sigma[3]","e_sigma[4]","e_sigma[5]",
                "e_sigma[6]","e_sigma[7]","e_sigma[8]","e_sigma[9]","e_sigma[10]"),
              c(1,3,5,6,7)]

ctsigma<-format(round(ctsigma, 2), nsmall = 2)
rownames(ctsigma)<-c("ABC", "BBC","CBS", "CNN", "Fox", "NBC", 
                     "NPR", "NYT", "WP", "WSJ")
ctsigma<- ctsigma[order(ctsigma[,1]),]
latex(ctsigma,fil="")
