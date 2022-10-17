
#loading required packages
library(lfe)
library(stargazer)
library(haven)
library(here)
library(dplyr)
library(MASS)
library(ggplot2)


getwd()



##############################################
#INCOME AND EDUCATION EXCERCISE
##############################################
library(haven)

#loading CPS data
mydata<-read.csv("../../data/data_M1_OVB/cps_clean.csv")

#regressing income on education
reg1<-lm(inctot~edu,mydata[mydata$age>22,])
summary(reg1)

#visualizing scatterplot
plot(mydata[mydata$age>22 & mydata$inctot<300000,]$edu, 
     mydata[mydata$age>22 & mydata$inctot<300000,]$inctot)

#constructing College graduate indicator
mydata$collgrad<-0
mydata$collgrad[mydata$edu>=16]<-1

#regressing income on college graduation
reg2<-lm(inctot~collgrad,mydata[mydata$edu>=12 & mydata$age>22,])
summary(reg2)
##############################################


##############################################
#INCOME AND HEALTH OVB EXCERCISE
##############################################
library(haven)
library(stargazer)

#loading CPS data
mydata<-read.csv("../../data/data_M1_OVB/cps_clean.csv")

#regressing income on health
reghealth<-lm(inctot~health,mydata)
summary(reghealth)

#regressing income on health and age
reghealth2<-lm(inctot~health+age ,mydata)
summary(reghealth2)

#regressing income on health, age and education
reghealth3<-lm(inctot~health+age+edu ,mydata)
summary(reghealth3)

#presenting results in a formated output table
stargazer(reghealth,reghealth2, reghealth3, 
          out = "code_output/tableovb.txt", type="text", header=FALSE, 
          title = "Income and health", omit.stat=c("f", "ser"))

##############################################

##############################################
# OVB SIMULATION EXCERCISE
##############################################
library(MASS)
library(ggplot2)

#setting base number so new run gives same results
set.seed(1999)

#generating two standard normal variables with a 0.5 correlation
out <- as.data.frame(mvrnorm(1000, mu = c(0,0), 
                             Sigma = matrix(c(1,0.5,0.5,1), ncol = 2), 
                             empirical = TRUE))

#checking the two variable are correlated as desired
cor(out)

#plotting the data I just generated
plot(out)

#generating a random error term (also standard normal)
out$error<-rnorm(1000, mean=0, sd=1)

#setting the values of the true beta coefficients
B1<-5
B2<-7

#Creating the data generating process
out$Y<-out$V1*B1+out$V2*B2+out$error

#estimating the correct model
sim1<-lm(Y~V1+V2, data=out)

#estimating the underspacified model
sim2<-lm(Y~V1, data=out)

#reporting results in a table
stargazer(sim1,sim2,out = "code_output/tableovb_sim.txt", type="text",  header=FALSE, 
          title="Omitted Variable Bias Simulation", omit.stat=c("f", "ser"))

#generating adjusted oucome variable
out$adjY<-out$Y-B2*out$V2

#regressing with the adjusted outcome variable
sim3<-lm(adjY~V1, data=out)

#reporting results in a table
stargazer(sim1,sim2,sim3,out = "code_output/tableovb_sim2.txt",  type="text", header=FALSE, 
          title="Omitted Variable Bias Simulation 2", omit.stat=c("f", "ser"))

#plotting the two sets of outcome variables
plotted<-ggplot(out, aes(V1, y = value, color = variable)) + 
  geom_point(aes(y = Y, col = "Y")) + 
  geom_point(aes(y = adjY, col = "adjY"))+
  geom_smooth(method='lm', aes(y = Y, col = "Y"))+
  geom_smooth(method='lm', aes(y = adjY, col = "adjY"))

plotted
##############################################



##############################################
# Arseneaux Gerber and Green (2006)
##############################################

#loading the data
agg_data<-read_dta("../../data/data_M1_OVB/IA_MI_merge040504.dta")
nrow(agg_data)

##scalling the vote02 variable to remove excess 0's from tables
agg_data$vote02<-100*as.numeric(agg_data$vote02)

#note: basic controls are included since the randomization happened at the state level
#and to distinguish between competitive and un-competitive races in each state.
regols1<-felm(vote02~contact+state+comp_mi+comp_ia,agg_data)

#Getting an unbiased estimate using insturumental variables approach
regexp1<-felm(vote02~state+comp_mi+comp_ia|0|(contact~treat_real+state+comp_mi+comp_ia),agg_data)

#presenting results in a table
stargazer(regols1,regexp1, out = "code_output/agg1.txt",  type="text", se = list(regols1$rse,regexp1$rse),
          header=FALSE,  title="AGG replication 1",omit.stat=c("f", "ser"), single.row = TRUE)

#old regression with controls
regols2<-felm(vote02~contact+state+comp_mi+comp_ia+persons+age+
                female2+newreg+vote00+vote98+fem_miss|county+st_hse+st_sen,agg_data)

#experimental IV estimates
regexp2<-felm(vote02~state+comp_mi+comp_ia+persons+age+
                female2+newreg+vote00+vote98+fem_miss|county+st_hse+st_sen|
                (contact~treat_real+state+comp_mi+comp_ia+persons+age
                 +female2+newreg+vote00+vote98+fem_miss),agg_data)

#presenting results in a table
stargazer(regols2,regexp2, out = "code_output/agg2.txt",  type="text", se = list(regols2$rse,regexp2$rse),
          header=FALSE,  title="AGG replication 2",omit.stat=c("f", "ser"), single.row = TRUE)
##############################################


