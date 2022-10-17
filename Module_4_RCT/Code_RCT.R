
#Loading needed packages
library(lfe)
library(stargazer)
library(MASS)
library(ggplot2)



##############################
#Simple RCT simulation
##############################

set.seed(1999)

scores5<-as.data.frame(rep(c(1,2,3,4,5,6,7,8,9,10),times=30))
names(scores5)<-c("class")
scores5 <- fastDummies::dummy_cols(scores5, select_columns = "class")

scores5$error<-rnorm(300, mean=0, sd=10)

#treatment indicator
scores5$treat<-rbinom(300,1,0.3)



#mean reading score
alpha=75

#treatment effect
tau=10

#the data generating process: notice the class does affect a students score
scores5$read4<-(alpha+tau*scores5$treat+scores5$error
                +4*scores5$class_1+(-6)*scores5$class_2+8*scores5$class_3
                +(-4)*scores5$class_4+7*scores5$class_5+(-2)*scores5$class_6
                +5*scores5$class_7+(-10)*scores5$class_8+8*scores5$class_9
                +4*scores5$class_10)

rct1<-felm(read4~treat,scores5)

stargazer(rct1, type="latex")
##############################





##############################
#Generating correlated covariate data
##############################

#simulating covariates

#third grade test scores. Notice I am generateing simulated 
#academic scores that have a correlation to their "untreated"
#performance in 4th grade reading
scores5$read3<-alpha+scores5$error+rnorm(300,3,2)
scores5$math3<-alpha+scores5$error+rnorm(300,15,2)
scores5$hist3<-alpha+scores5$error+rnorm(300,5,2)
scores5$pe3<-rnorm(300,90,2)

#other 4th grade test scores: notice I am generating scores that 
#correlated with their subject performance in 3rd grade. 
#Also, the treatment is affecting other 4th grade academic scores
scores5$hist4<-4*scores5$treat+scores5$hist3+rnorm(300,-2,2)
scores5$pe4<-scores5$pe3+rnorm(300,0,5)
scores5$math4<-2*scores5$treat+scores5$math3+rnorm(300,-5,3)

#student characteristics
scores5$female<-rbinom(300,1,0.5)
scores5$age<-runif(300,9,10)
scores5$height<-rnorm(300,1.3,0.2)

scoresmini<-scores5[,c("treat", "read4", "read3", "math3","hist3",
                       "pe3","hist4","pe4","math4","female", "age", "height")]

cor(scoresmini)
#as you can see, we have simulated some complex 
#interrelationships between theses variables.
##############################


##############################
#Balance test: using loop apply (lapply)
##############################

#Balance test: I generate a loop to run all the covariate regressions.

namevec<-names(scores5)
namevec<-namevec[!namevec%in%c("class","error", "treat","read4")]

allModelsList <- lapply(paste(namevec,"~treat"), as.formula)
allModelsResults <- lapply(allModelsList, function(x) lm(x, scores5))  


stargazer(allModelsResults[[1]],allModelsResults[[2]],allModelsResults[[3]],
          allModelsResults[[4]], allModelsResults[[5]], type="latex")

stargazer(allModelsResults[[6]],allModelsResults[[7]],allModelsResults[[8]],
          allModelsResults[[9]], allModelsResults[[10]], type="latex")

stargazer(allModelsResults[[11]],allModelsResults[[12]],allModelsResults[[13]],
          allModelsResults[[14]], allModelsResults[[15]], type="latex")

stargazer(allModelsResults[[16]],allModelsResults[[17]],allModelsResults[[18]],
          allModelsResults[[19]], allModelsResults[[20]], type="latex")

##############################


##############################
#RCT estimates with covariates
##############################

rct1<-felm(read4~treat,scores5)
rct2<-felm(read4~treat+read3+female+pe3+math3+hist3,scores5)
rct3<-felm(read4~treat+read3+female+pe3+math3+hist3|class,scores5)

stargazer(rct1, rct2, rct3, type="latex")
##############################




##############################
#RCT with heterogenious treatment effects
##############################

rct1<-felm(read4~treat,scores5)
rcthet1<-felm(read4~treat+female+female*treat,scores5)

#simulating different types of heterogenious treatment effects

nf<-20
#the data generating process: notice the class does affect a students score
scores5$read4het1<-(nf*scores5$treat+(-20)*scores5$female*scores5$treat+
                      alpha+scores5$error
                    +4*scores5$class_1+(-6)*scores5$class_2
                    +8*scores5$class_3+(-4)*scores5$class_4+7*scores5$class_5
                    +(-2)*scores5$class_6+5*scores5$class_7+(-10)*scores5$class_8
                    +8*scores5$class_9+4*scores5$class_10)

rct2<-felm(read4het1~treat,scores5)
rcthet2<-felm(read4het1~treat+female+female*treat,scores5)

nf2<-30

scores5$read4het2<-(nf2*scores5$treat+(-40)*scores5$female*scores5$treat+
                alpha+scores5$error+4*scores5$class_1+(-6)*scores5$class_2
                    +8*scores5$class_3+(-4)*scores5$class_4+7*scores5$class_5
                    +(-2)*scores5$class_6+5*scores5$class_7+(-10)*scores5$class_8
                    +8*scores5$class_9+4*scores5$class_10)

rct3<-felm(read4het2~treat,scores5)
rcthet3<-felm(read4het2~treat+female+female*treat,scores5)

stargazer(rct1,rcthet1,rct2,rcthet2,rct3,rcthet3, type="latex", omit.stat="ser")
##############################




##############################
#RCT with attrition
##############################

scores5$read4miss<-NA
scores5$read4miss[scores5$read4>75]<-scores5$read4[scores5$read4>75]

scores5$obsnew<-0
scores5$obsnew[scores5$treat==1 & scores5$read4>75 & scores5$read4<85]<-1

rctmiss<-felm(read4miss~treat,scores5)
rctmiss2<-felm(read4miss~treat,scores5[scores5$obsnew==0,])


stargazer(rctmiss, rctmiss2, type="latex")

##############################












