

#Loading needed packages
library(lfe)
library(stargazer)
library(MASS)
library(ggplot2)


##############################################
#Simple IV simulation
##############################################

#generating the correlation matrix (MASS package)
sigmaMat<-matrix(c(1,0.75,0.25,0.75,1,0,0.25,0,1), nrow=3)
sigmaMat

#setting the seed
set.seed(3221)

#generating the data
ivdat<- as.data.frame(mvrnorm(10000, mu = c(0,0,0), 
                     Sigma = sigmaMat))

names(ivdat)<-c("x_1","x_2","z")
cor(ivdat)

#generating the random error term
ivdat$error<-rnorm(10000, mean=0, sd=1)

#The data generating process
B1<-10
B2<-(-20)

ivdat$Y<-ivdat$x_1*B1+ivdat$x_2*B2+ivdat$error

#estimating the true and underspecified model and presenting results
simiv1<-lm(Y~x_1+x_2, data=ivdat)
simiv2<-lm(Y~x_1, data=ivdat)
stargazer(simiv1, simiv2,  type='latex')

#understanting the source of the bias
cor(ivdat$z, ivdat$x_1)

#note: we can test this correlation because I am working with simulated data and observe x_2.
#In the wild x_2 would be unobservable and you would have to argue that this condition holds.
ivdat$nu<-B2*ivdat$x_2+ivdat$error

cor(ivdat$z, ivdat$nu)

#estimating the IV estimate
simiv3<-felm(Y~1|0|(x_1~z),ivdat)
stargazer(simiv1, simiv2, simiv3,  type='latex')

#getting the first stage
sim2slsfs<-felm(x_1~z,ivdat)
summary(sim2slsfs)

#the second stage
hatgamma0<-sim2slsfs$coefficients[1]
hatgamma1<-sim2slsfs$coefficients[2]

ivdat$hatx_1<-hatgamma0+hatgamma1*ivdat$z

sim2slsss<-felm(Y~hatx_1,ivdat)
stargazer(simiv1, simiv2, simiv3,sim2slsss,  type='latex')

#the reduced form
sim2slsrf<-felm(Y~z,ivdat)
stargazer(sim2slsfs, sim2slsss, sim2slsrf,  type='latex')
##############################################



##############################################
#A weak instrument
##############################################

#generating the correlation matrix
sigmaMat<-matrix(c(1,0.75,0.03,0.75,1,0.01,0.03,0.01,1), nrow=3)
sigmaMat

#generating the simulated data
set.seed(5000)
ivdatwk<- as.data.frame(mvrnorm(10000, mu = c(0,0,0), 
                                Sigma = sigmaMat))

names(ivdatwk)<-c("x_1","x_2","z")
cov(ivdatwk)

ivdatwk$error<-rnorm(10000, mean=0, sd=1)
ivdatwk$nu=(-20)*ivdatwk$x_2+ivdatwk$error

#The data generating process
B1<-10
B2<-(-20)
ivdatwk$Y<-ivdatwk$x_1*B1+ivdatwk$x_2*B2+ivdatwk$error

simivweakfs<-lm(x_1~z,ivdatwk)
simivweak<-felm(Y~1|0|(x_1~z),ivdatwk)
stargazer(simivweakfs,simivweak,  type='latex')
##############################################



##############################################
#IV with control variables example
##############################################

sigmaMat<-matrix(c(1,0.75,0.25,0.2,0.75,1,0,0,0.25,0,1,0,0.2,0,0,1 ), nrow=4)
sigmaMat

set.seed(5000)
ivc<- as.data.frame(mvrnorm(10000, mu = c(0,0,0,0), 
                     Sigma = sigmaMat))

names(ivc)<-c("x_1","x_2","z", "c")
ivc$error<-rnorm(10000, mean=0, sd=1)

ivc$nu=(-20)*ivc$x_2+ivc$error

#The data generating process
B1<-10
B2<-5
B3<-(-20)

ivc$Y<-ivc$x_1*B1+ivc$x_2*B3+B2*ivc$c+ivc$error

simivc<-felm(Y~c|0|(x_1~z+c),ivc)
stargazer(simivc,  type='latex')
##############################################







##############################################
#IV with multiple instruments
##############################################

sigmaMat<-matrix(c(1,0.75,0.25,0.5,0.75,1,0,0,0.25,0,1,0.3,0.5,0,0.3,1 ), nrow=4)
sigmaMat


set.seed(5000)
ivmi<- as.data.frame(mvrnorm(10000, mu = c(0,0,0,0), 
                     Sigma = sigmaMat))

names(ivmi)<-c("x_1","x_2","z_1", "z_2")
ivmi$error<-rnorm(10000, mean=0, sd=1)

ivmi$nu=(-20)*ivmi$x_2+ivmi$error

#The data generating process
B1<-10
B2<-(-20)

ivmi$Y<-ivmi$x_1*B1+ivmi$x_2*B2+ivmi$error

simivmifs<-felm(x_1~z_1+z_2,ivmi)
simivmi<-felm(Y~1|0|(x_1~z_1+z_2),ivmi)
stargazer(simivmifs, simivmi,  type='latex')
##############################################



##############################################
#IV with multiple endogenous variables and multiple instruments
##############################################

sigmaMat<-matrix(c(1,0.75,0.25,0.1,0.2,
                   0.75,1,0,0,0.4,
                   0.25,0,1,0.3,0.15,
                   0.1,0,0.3,1,0.35,
                   0.2,0.4,0.15,0.35,1), nrow=5)
sigmaMat


set.seed(5500)
ivme<- as.data.frame(mvrnorm(10000, mu = c(0,0,0,0,0), 
                     Sigma = sigmaMat))

names(ivme)<-c("x_1","x_2","z_1", "z_2","x_3")
ivme$error<-rnorm(10000, mean=0, sd=1)

ivme$nu=(-20)*ivme$x_2+ivme$error

#The data generating process
B1<-10
B2<-(-20)
B3<-(-30)

ivme$Y<-ivme$x_1*B1+ivme$x_2*B2+ivme$x_3*B3+ivme$error


simivmefs1<-felm(x_1~z_1+z_2,ivme)
simivmefs2<-felm(x_3~z_1+z_2,ivme)
#Underidentified
simivmeunder1<-felm(Y~1|0|(x_1|x_3~z_2),ivme)
simivmeunder2<-felm(Y~1|0|(x_1|x_3~z_1),ivme)
simivme<-felm(Y~1|0|(x_1|x_3~z_1+z_2),ivme)
stargazer(simivmefs1,simivmefs2,simivmeunder1,simivmeunder2, simivme,  type='latex')
################################################################

################################################################
#IV in AAG(2006)
################################################################

library(haven)
library(lfe)
library(dplyr)

agg_data<-read_dta("../../data/data_M3_IV/IA_MI_merge040504.dta")
nrow(agg_data)

#scalling the vote02 variable to remove excess 0's from tables
agg_data$vote02<-100*as.numeric(agg_data$vote02)


regols1<-felm(vote02~contact+state+comp_mi+comp_ia,agg_data)
regiv1<-felm(vote02~state+comp_mi+comp_ia|0|(contact~treat_real+state+comp_mi+comp_ia),agg_data)

stargazer(regols1,regiv1,type='latex', se = list( regols1$rse, regiv1$rse), header=FALSE)
################################################################

