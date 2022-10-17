
library(stargazer)
library(lfe)
library(dplyr)


#####################################################
#SIMULATION
####################################################

#I generate a vector of class identifiers
class<-c(1,2,3,4)
scores<-as.data.frame(class)
scores<-rbind(scores,scores,scores,scores,scores,scores,scores,scores,scores,scores)

#and a random error term
scores$error<-rnorm(40, mean=0, sd=5)

#I create two indicator variables that are set to one(treatment) with different probabilities 
scores$treat1<-rbinom(40,1,0.2)
scores$treat2<-rbinom(40,1,0.8)
#the treatment probability for students in classes 1 and 2 will be 0.2 and 0.8 for students in classes 3 and 4
scores$treat[scores$class%in%c(1,2)]<-scores$treat1[scores$class%in%c(1,2)]
scores$treat[scores$class%in%c(3,4)]<-scores$treat2[scores$class%in%c(3,4)]

#keeping the variables I need
scores<-scores%>%select(class,error,treat)

#generating dummy variables for student classes
scores <- fastDummies::dummy_cols(scores, select_columns = "class")

#simulating the DGP (data generating process). The treatment effect is 15
#and the average score in each class differs quite substantially
scores$score<-80+15*scores$treat+85*scores$class_2+ -30*scores$class_3+
  -35*scores$class_4+scores$error

# estimating without fixed effects
nofe<-felm(score~treat,scores)

#estimating with dummy variables
dummies<-felm(score~treat+class_2+class_3+class_4, scores)

#estimating with fixed effects
fe<-felm(score~treat|class,scores)

#presenting results in a formatted table
stargazer(nofe, dummies, fe, type='text')
#############################################################

#############################################################
#FIXED EFFECTS AS DEMEANED DATA
#############################################################

#calculateing the mean score in each classroom
cl_mean<-scores %>%
  group_by(class) %>%
  dplyr::summarize(Classmean = mean(score, na.rm=TRUE), treatmean=mean(treat, na.rm=TRUE))

#merging the means into full data
scores<-left_join(scores, cl_mean, by = "class")

#calculating the demeaned score
scores$demeansc<-scores$score-scores$Classmean
scores$demeantrt<-scores$treat-scores$treatmean

#running the basic regression on the demaned scores
regdemean<-felm(demeansc~demeantrt, scores)

#adding this to our formatted table
stargazer(nofe, dummies, fe, regdemean, type='text')
#############################################################


#############################################################
#UNDERSTANDING VARIATION
#############################################################

library(dplyr)
library(lfe)

#Generating new simulated data where treatment entierly depends on the class you are in
class2<-c(1,2,3,4)
scores2<-as.data.frame(class2)
scores2<-rbind(scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2)
scores2$error<-rnorm(40, mean=0, sd=5)

scores2$treat[scores2$class%in%c(1,2)]<-0
scores2$treat[scores2$class%in%c(3,4)]<-1

scores2<-scores2%>%select(class2,error,treat)
scores2 <- fastDummies::dummy_cols(scores2, select_columns = "class2")

scores2$score<-80+15*scores2$treat+85*scores2$class2_2+ -30*scores2$class2_3+ -35*scores2$class2_4+scores2$error

#estimating no fe
nofe2<-felm(score~treat,scores2)

#estimating with dummy variables
dummies2<-felm(score~treat+class2_2+class2_3+class2_4, scores2)

#attempting to estimate with fixed effects
#fe2<-felm(score~treat|class2,scores2)

#presenting results in a formatted table
stargazer(nofe2, dummies2,  type='latex')
#############################################################


#############################################################
#FIXED EFFECTS WITH NON SIMULATED DATA
#############################################################

#install.packages("wooldridge")
library(wooldridge)
library(lfe)
#note: this dataset comes from the wooldridge textbook. Conveniently there is an R package that 
#includes all the wooldridge datasets. 

#loading the data from the wooldridge package
crime<-data('crime2')
crime<-crime2

#simple estimation using one year of data
regcrime<-felm(crmrte~unem, crime[crime$year=="87",])
summary(regcrime)


#simple estimation using one year of data and controls
regcrime2<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc, crime[crime$year=="87",])
summary(regcrime2)


#note: the data does not have a unique city identifier. I am assuming the area of the city
#is 1)time-invariant and 2) uniquely identifies the 46 cities. 
#The line of code below generates a unique identifier
crime <- transform(crime,city=as.numeric(factor(area)))
#I check that my assumptions were correct by seeing if I have 2 observations for 46 cities.
table(crime$city)

#adding city fixed effects
regcrime3<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city, crime)
summary(regcrime3)

#adding city and year fixed effects
regcrime4<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city+year, crime)
summary(regcrime4)

#thinking about variation

#I create a identifier that uniquely identifies observations that
#happen in a particular city in a particular year
crime$city_year<-paste(crime$city, crime$year, sep="_")

#Note: the following regression will not run!
#regcrime5<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city_year, crime)
#summary(regcrime5)

############################################################################