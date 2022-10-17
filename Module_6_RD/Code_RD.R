

library(dplyr)
library(ggplot2)
library(lfe)
library(stargazer)


# Sharp RD


set.seed(7000)

sharp<-rnorm(5000, mean=80, sd=5)
sharp<-as.data.frame(sharp)

names(sharp)<-c("read3")
sharp$error<-rnorm(5000, mean=0, sd=5)
sharp$pe3<-rnorm(5000, mean=90, sd=4)
sharp$height<-rnorm(5000, mean=130, sd=15)

sharp$treated<-0
sharp$treated[sharp$read3<=75]<-1

tau=10
#the DGP
sharp$read4<-(-6)+0.8*sharp$read3+tau*sharp$treated+sharp$error

#selecting observations that fall in our bandwidth
sharp<-sharp[sharp$read3<78 & sharp$read3>72,]






## Sharp RD: Treatment status graph


#I will break up the data into 60 bins (30 above and 30 below the threshold)
cuts<-c(72,72.1,72.2,72.3,72.4,72.5,72.6,72.7,72.8,72.9,73,
        73.1,73.2,73.3,73.4,73.5,73.6,73.7,73.8,73.9,74,
        74.1,74.2,74.3,74.4,74.5,74.6,74.7,74.8,74.9,75,
        75.1,75.2,75.3,75.4,75.5,75.6,75.7,75.8,75.9,76,
        76.1,76.2,76.3,76.4,76.5,76.6,76.7,76.8,76.9,77,
        77.1,77.2,77.3,77.4,77.5,77.6,77.7,77.8,77.9,78)
midpoints<-cuts[2:61]-0.05

sharp$bins <- cut(sharp$read3, 
                  breaks=cuts, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=midpoints)


sharp_mean<-sharp %>%
  group_by(bins) %>%
  dplyr::summarize(outbinmean = mean(read4, na.rm=TRUE),
                   treatbinmean=mean(treated, na.rm=TRUE), 
                   pebinmean=mean(pe3, na.rm=TRUE),
                   heightbinmean=mean(height, na.rm=TRUE), numb=n())

sharp_mean$bins<-as.numeric(as.character(sharp_mean$bins))

plot1shp<-ggplot(sharp_mean, aes(x=bins, y=treatbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)


plot1shp


plot2shp<-ggplot(sharp_mean, aes(x=bins, y=outbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)


plot2shp



plot3shp<-ggplot(sharp_mean, aes(x=bins, y=pebinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)


plot3shp



plot4shp<-ggplot(sharp_mean, aes(x=bins, y=heightbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)
plot4shp




plot5shp<-ggplot(sharp_mean, aes(x=bins, y=numb))+ 
  geom_point()+
  geom_vline(xintercept = 75)

plot5shp



sharp$runminc<-sharp$read3-75
shpestim<-felm(read4~treated+runminc+treated*runminc, sharp)

stargazer(shpestim, type="latex", header=FALSE)

stylized<-ggplot(sharp_mean, aes(x=runminc, y=outbinmean))+ 
  geom_vline(xintercept = 0)+
  geom_segment(aes(x = 0, xend = 3, 
                   y = shpestim$coefficients[1],
                   yend = shpestim$coefficients[1]
                   +3*shpestim$coefficients[3]))+
  geom_segment(aes(x = -3, xend = 0,
                   y = shpestim$coefficients[1]
                   + shpestim$coefficients[2]
                   +(-3*( shpestim$coefficients[3]+ shpestim$coefficients[4])), 
                   yend = shpestim$coefficients[1]
                   + shpestim$coefficients[2]))+
  #adding some labeling for course notes:
  annotate("text", x = 0.75, y = 64.8,
           label = "Treated~Intercept~is" ,parse = TRUE)+
  annotate("text", x = -0.6, y = 54, 
           label = "Untreated~Intercept~is" ,parse = TRUE)+
  annotate("text", x = -2, y = 64,
           label = "Treated~slope~is" ,parse = TRUE)+
  annotate("text", x = 2, y = 54, 
           label = "Untreated~slope~is" ,parse = TRUE)+
  annotate("text", x = -1, y = 58, 
           label = "TREATED" ,parse = TRUE)+
  annotate("text", x = 1, y = 58, 
           label = "UNTREATED" ,parse = TRUE) 

stylized


sharp_mean$runminc<-sharp_mean$bins-75

plot6shp<-ggplot(sharp_mean, aes(x=runminc, y=outbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_segment(aes(x = 0, xend = 3, 
                   y = shpestim$coefficients[1],
                   yend = shpestim$coefficients[1]
                   +3*shpestim$coefficients[3]))+
  geom_segment(aes(x = -3, xend = 0,
                   y = shpestim$coefficients[1]
                   + shpestim$coefficients[2]
                   +(-3*( shpestim$coefficients[3]+ shpestim$coefficients[4])), 
                   yend = shpestim$coefficients[1]
                   + shpestim$coefficients[2]))+
  #adding some labeling for course notes:
  annotate("text", x = 0.75, y = 64.8,
           label = "Intercept~is~alpha~+~tau" ,parse = TRUE)+
  annotate("text", x = -0.6, y = 54, 
           label = "Intercept~is~alpha" ,parse = TRUE)+
  annotate("text", x = -2, y = 64,
           label = "Slope~is~beta~+~gamma" ,parse = TRUE)+
  annotate("text", x = 2, y = 54, 
           label = "Slope~is~beta" ,parse = TRUE)+
  annotate("text", x = -1, y = 58, 
           label = "TREATED" ,parse = TRUE)+
  annotate("text", x = 1, y = 58, 
           label = "UNTREATED" ,parse = TRUE) 



plot6shp




  
  ## Fuzzy RD Simulation:


set.seed(2000)

fuzzy<-rnorm(5000, mean=80, sd=5)
fuzzy<-as.data.frame(fuzzy)

names(fuzzy)<-c("read3")
fuzzy$error<-rnorm(5000, mean=0, sd=5)
fuzzy$pe3<-rnorm(5000, mean=90, sd=4)
fuzzy$height<-rnorm(5000, mean=130, sd=15)



fuzzy$lowprob<-rbinom(5000,1,0.3)
fuzzy$highprob<-rbinom(5000,1,0.8)
fuzzy$treated<-NA
fuzzy$treated[fuzzy$read3>75]<-fuzzy$lowprob[fuzzy$read3>75]
fuzzy$treated[fuzzy$read3<=75]<-fuzzy$highprob[fuzzy$read3<=75]

tau=10

#the DGP
fuzzy$read4<-(-6)+0.8*fuzzy$read3+tau*fuzzy$treated+fuzzy$error

fuzzy<-fuzzy[fuzzy$read3<78 & fuzzy$read3>72,]



#I will break up the data into 60 bins (30 above and 30 below the threshold)

cuts<-c(72,72.1,72.2,72.3,72.4,72.5,72.6,72.7,72.8,72.9,73,
        73.1,73.2,73.3,73.4,73.5,73.6,73.7,73.8,73.9,74,
        74.1,74.2,74.3,74.4,74.5,74.6,74.7,74.8,74.9,75,
        75.1,75.2,75.3,75.4,75.5,75.6,75.7,75.8,75.9,76,
        76.1,76.2,76.3,76.4,76.5,76.6,76.7,76.8,76.9,77,
        77.1,77.2,77.3,77.4,77.5,77.6,77.7,77.8,77.9,78)
midpoints<-cuts[2:61]-0.05

fuzzy$bins <- cut(fuzzy$read3, 
                  breaks=cuts, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=midpoints)


fuzzy_mean<-fuzzy %>%
  group_by(bins) %>%
  dplyr::summarize(outbinmean = mean(read4, na.rm=TRUE),
                   treatbinmean=mean(treated, na.rm=TRUE),
                   pebinmean=mean(pe3, na.rm=TRUE),
                   heightbinmean=mean(height, na.rm=TRUE), numb=n())

fuzzy_mean$bins<-as.numeric(as.character(fuzzy_mean$bins))

plot1fuz<-ggplot(fuzzy_mean, aes(x=bins, y=treatbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)

plot1fuz




plot2fuz<-ggplot(fuzzy_mean, aes(x=bins, y=outbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)




plot2fuz





plot3fuz<-ggplot(fuzzy_mean, aes(x=bins, y=pebinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)


plot3fuz


plot4fuz<-ggplot(fuzzy_mean, aes(x=bins, y=heightbinmean))+ 
  geom_point()+
  geom_vline(xintercept = 75)
plot4fuz


plot5fuz<-ggplot(fuzzy_mean, aes(x=bins, y=numb))+ 
  geom_point()+
  geom_vline(xintercept = 75)



plot5fuz





fuzzy$runminc<-fuzzy$read3-75

#first stage
fuzzy$ittgroup<-0
fuzzy$ittgroup[fuzzy$read3<=75]<-1

fuzfs<-felm(treated~ittgroup+runminc+ittgroup*runminc,fuzzy)

#reduced form
fuzrf<-felm(read4~ittgroup+runminc+ittgroup*runminc,fuzzy)


fuzzy$interedog<-fuzzy$treated*fuzzy$runminc
fuzzy$interinst<-fuzzy$ittgroup*fuzzy$runminc
#IV
fuziv<-felm(read4~runminc|0|(treated|interedog~ittgroup+runminc+interinst),fuzzy)

stargazer(fuzfs, fuzrf, fuziv, type="latex", header=FALSE)




