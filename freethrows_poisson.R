#import dataset 
ft<-read.csv(file.choose(),header=TRUE,fileEncoding="latin1")
head(ft)

#poisson basics
ppois(17,lambda=20) #probability of getting 17 personal fouls in 48 miniutes? 29.7%

#import libraries 
library(ggplot2)
library(plyr)
library(dplyr)
library(sandwich)
library(msm)

# barplot
pf_season<-ddply(ft,.(Season),summarize,mean_pf=mean(pf))
ggplot(pf_season,aes(x=Season,y=mean_pf))+geom_bar(stat="identity")

#2016-17 personal foul histogram
pf_2017<-subset(ft,Season=="16-17")
qplot(pf_2017$pf,geom="histogram",binwidth=0.5)+xlab("personal fouls")

#foul histogram (three-year span)
qplot(ft$pf,geom="histogram",binwidth=0.35)+xlab("personal fouls")+xlab("Average Personal Fouls per Game")+
ggtitle("Straight Up Defense in Charlotte")+theme(plot.title = element_text(hjust = 0.5))
  
#long to wide (dataframe)
library(tidyr)
wide_pf<-spread(ft, key = ï..TEAM, value = pf)

#Poisson regression model 
model_poisson<-glm(pf_round~FGA.0.5+FGA.5.9+FGA.10.14+
                             FGA.15.19+FGA.20.24+FGA.25.29,family=poisson,data=ft)
summary(model_poisson)

#predict personal fouls per game
ft$pred_pf<-predict(model_poisson,ft,response="TRUE")
ft$pf_diff<-ft$pf-ft$pred_pf #+ more than expected, - less than expected 

#t-test test personal fouls per game between two seasons
pf_season_mean<-ddply(ft,.(Season),summarize,mean_pf=mean(pf))
pf_season_mean
pf_season_sd<-ddply(ft,.(Season),summarize,sd_pf=sd(pf))
pf_season_sd

#scale variables 
ft$scale_pf<-scale(ft$pf,center=TRUE,scale=TRUE)
#Z-scores -1.645 to 1.645 (signifcant at 0.05 level)
ft$pf_z_score<-(ft$scale_pf-mean(ft$scale_pf))/sd(ft$scale_pf)

#poisson calculation (takes non-negative integers)
ppois(18,lambda=20) 

#one sample t-test (95% confidence interval) 
t.test(ft$pf,mu=20)

# cha (16-17) 29.7% probability of getting 16.6 personal fouls in 48 minutes?
# sas(15-16) 38%
# chi (16-17) 38%
# phx (16-17) 11.2%
# den (14-15) 21.3%
# phx(15-16) 21.3%



