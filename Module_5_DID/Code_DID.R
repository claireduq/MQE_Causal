

#Loading needed packages

library(lfe)
library(dplyr)
library(ggplot2)
library(stargazer)




##############################
#Simple DID simulation
##############################

      
set.seed(6000)
scores<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scores)<-c("class")
scores <- fastDummies::dummy_cols(scores, select_columns = "class")
      
scores$error<-rnorm(300, mean=0, sd=10)
      
#suppose teachers in the better performing classes (classes, 7,8,9,10) select to participate in the book club program
scores$treat<-0
scores$treat[scores$class%in%c(7,8,9,10)]<-1
      
tau<-10
      
#the data generating process
scores$read4<-(85+tau*scores$treat
               +(-10)*scores$class_1+(-15)*scores$class_2+(-5)*scores$class_3
               +(-8)*scores$class_4+(-7)*scores$class_5+(-13)*scores$class_6
               +(11)*scores$class_7+(8)*scores$class_8+(10)*scores$class_9
               +(12)*scores$class_10
                +scores$error)
      
scores$year<-"2001"
      
scores01<-scores
      
rm(scores)
      
scores<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scores)<-c("class")
scores <- fastDummies::dummy_cols(scores, select_columns = "class")
      
scores$error<-rnorm(300, mean=0, sd=10)
      
scores$treat<-0
scores$treat[scores$class%in%c(7,8,9,10)]<-1
      
#the data generating process
scores$read4<-(78
               +(-10)*scores$class_1+(-15)*scores$class_2+(-5)*scores$class_3
               +(-8)*scores$class_4+(-7)*scores$class_5+(-13)*scores$class_6
               +(11)*scores$class_7+(8)*scores$class_8+(10)*scores$class_9
               +(12)*scores$class_10
               +scores$error)
      
scores$year<-"2000"
      
scores00<-scores
rm(scores)
      
scores<-rbind(scores01, scores00)
      
regnodid<-felm(read4~treat,scores[scores$year=="2001",])
      
scores$post<-0
scores$post[scores$year=="2001"]<-1
regdid<-felm(read4~post+treat+post*treat,scores)
      
      
regdidfe<-felm(read4~post+treat+post*treat|class,scores)
      
      
stargazer(regnodid, regdid,regdidfe, type="latex")
      
      
##############################

      
      
    
##############################
#Testing Parallel trends
##############################

set.seed(1999)
scoresbase<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scoresbase)<-c("class")
scoresbase <- fastDummies::dummy_cols(scoresbase, select_columns = "class")

#suppose teachers in the better performing classes (classes, 7,8,9,10) select to participate in the book club program
scoresbase$treat<-0
scoresbase$treat[scoresbase$class%in%c(7,8,9,10)]<-1

yr<-c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005)
tauyr<-c(0,0,0,0,0,0,10,10,10,10,10)
yrfe<-c(72,77,75,79,81,79,83,77,82,84,81)

for(i in 1:11){
  name<-paste("scores", yr[i], sep="_")
  scores<-scoresbase
  scores$error<-rnorm(300, mean=0, sd=10)
  tau<-tauyr[i]
  yearfe<-yrfe[i]
  #the data generating process
  scores$read4<-(yearfe+tau*scores$treat+scores$error
                +(-10)*scores$class_1+(-15)*scores$class_2+(-5)*scores$class_3
               +(-8)*scores$class_4+(-7)*scores$class_5+(-13)*scores$class_6
               +(11)*scores$class_7+(8)*scores$class_8+(10)*scores$class_9
               +(12)*scores$class_10)

  scores$year<-yr[i]
  assign(name, scores)
  rm(scores)
}

allscores<-rbind(scores_1995,scores_1996,scores_1997,scores_1998,scores_1999,
                 scores_2000,scores_2001,scores_2002,scores_2003,scores_2004,scores_2005)

allscores$post<-0
allscores$post[allscores$year%in%c(2001,2002,2003,2004,2005)]<-1

allscores <- fastDummies::dummy_cols(allscores, select_columns = "year")

regdidall2<-felm(read4~treat
                 +year_1995*treat+year_1996*treat+year_1997*treat+year_1998*treat
                 +year_1999*treat+year_2001*treat+year_2002*treat+year_2003*treat
                 +year_2004*treat+year_2005*treat,
                 allscores)

stargazer( regdidall2, type="latex",no.space=TRUE)

##############################



##############################
#plotting Parallel trends
##############################


#start with plot of group means

#calculateing the mean score for each year by treatment status
grp_mean<-allscores%>%
    group_by(year,treat)%>%
    dplyr::summarize(groupmean = mean(read4, na.rm=TRUE))

grp_mean$treat<-as.factor(grp_mean$treat)

#difference in means plot
didmeans<-ggplot(grp_mean, aes(year, groupmean, group=treat, color = treat)) +
    stat_summary(geom = 'line') +
    geom_vline(xintercept = 2000) +
    theme( axis.text.x = element_blank())

didmeans

#plot of differences coefficients

res<-coef(summary(regdidall2))
res<-as.data.frame(res)

res<-res[13:22,]

a<-c(0,0,0,0)

res<-rbind(res,a)

year<-c(1995,1996,1997,1998,1999,2001,2002,2003,2004,2005,2000)
res<-cbind(res,year)
res$ci<-1.96*res$`Std. Error`

names(res)<-c("Estimate","se", "t",  "p", "year", "ci")

# Use 95% confidence interval instead of SEM
didplot2<-ggplot(res, aes(x=year, y=Estimate)) + 
    geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci),width=.1) +
    geom_vline(xintercept = 2000)+
    geom_hline(yintercept = 0)+
    geom_point()

didplot2



















regdidall3<-felm(read4~treat
                 +year_1995*treat+year_1996*treat+year_1997*treat+year_1998*treat
                 +year_1999*treat+year_2001*treat+year_2002*treat+year_2003*treat
                 +year_2004*treat+year_2005*treat
                 |0
                 |0
                 |class,
                 allscores)

stargazer( regdidall2,regdidall3, type="latex",no.space=TRUE)











set.seed(123456)
scores<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scores)<-c("class")
scores <- fastDummies::dummy_cols(scores, select_columns = "class")

scores$error<-rnorm(300, mean=0, sd=10)
scores$aftsch<-rbinom(300,1,0.5)
#suppose teachers in the better performing classes (classes, 7,8,9,10) select to participate in the book club program
scores$treat<-0
scores$treat[scores$class%in%c(7,8,9,10)]<-1

scores$treatnotaftsch<-0
scores$treatnotaftsch[scores$treat==1 & scores$aftsch==0]<-1

tau<-10

#the data generating process
scores$read4<-(85+13*scores$aftsch+tau*scores$treatnotaftsch+scores$error
               +(-10)*scores$class_1+(-15)*scores$class_2+(-5)*scores$class_3
               +(-8)*scores$class_4+(-3)*scores$class_5+(3)*scores$class_6
               +(5)*scores$class_7+(8)*scores$class_8+(10)*scores$class_9
               +(12)*scores$class_10)

scores$year<-"2001"

scores01<-scores

rm(scores)

scores<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scores)<-c("class")
scores <- fastDummies::dummy_cols(scores, select_columns = "class")

scores$error<-rnorm(300, mean=0, sd=10)
scores$aftsch<-rbinom(300,1,0.5)

scores$treat<-0
scores$treat[scores$class%in%c(7,8,9,10)]<-1

scores$treatnotaftsch<-0
scores$treatnotaftsch[scores$treat==1 & scores$aftsch==0]<-1

#the data generating process
scores$read4<-(78+18*scores$aftsch+scores$error
               +(-10)*scores$class_1+(-15)*scores$class_2+(-5)*scores$class_3
               +(-8)*scores$class_4+(-3)*scores$class_5+(3)*scores$class_6
               +(5)*scores$class_7+(8)*scores$class_8+(10)*scores$class_9
               +(12)*scores$class_10)

scores$year<-"2000"

scores00<-scores
rm(scores)

scores<-rbind(scores01, scores00)

scores$post<-0
scores$post[scores$year=="2001"]<-1
regdid3d<-felm(read4~post+treat+post*treat|0|0|class,scores)

regdid3dinter<-felm(read4~post+treat+aftsch+post*treat+treat*aftsch+post*aftsch+post*aftsch*treat|0|0|class,scores)

stargazer( regdid3d,regdid3dinter,  type="latex")
##############################



