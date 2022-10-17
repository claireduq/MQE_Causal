
library(dplyr)
library(ggplot2)
library(lfe)
library(stargazer)
library(robustbase)
library(wooldridge)
library(magick)
library(lm.beta)

  
###########################
## Regression Interpretation 
###########################

## Scaling

regsleep1<-lm(sleep~totwrk+educ, sleep75)
summary(regsleep1)

## Scaling the dependent variable

sleep75$sleephrs<-sleep75$sleep/60

regsleep2<-lm(sleephrs~totwrk+educ, sleep75)
summary(regsleep2)

                                   
## Scaling the independent variable
sleep75$totwrkhrs<-sleep75$totwrk/60
                                   
regsleep3<-lm(sleephrs~totwrkhrs+educ, sleep75)
summary(regsleep3)

                                   
     
#Standardizing           
                     
reg1<-lm(bwght~mage+fage, bwght2)
reg2<-lm(scale(bwght)~scale(mage)+scale(fage), bwght2)
reg3<-lm(scale(bwght)~mage+fage, bwght2)
reg4<-lm(bwght~scale(mage)+scale(fage), bwght2)
                                   
meandep1<-round(mean(bwght2$bwght),2)
meandep2<-round(mean(scale(bwght2$bwght)),2)
sddep1<-round(sd(bwght2$bwght),2)
sddep2<-round(sd(scale(bwght2$bwght)),2)
            
stargazer(reg1,reg2, reg3, reg4, type = "latex" , header=FALSE,omit.stat = "all",
          add.lines=list(c("Mean",meandep1,meandep2, meandep2, meandep1 ),
                        c("SD",sddep1,sddep2, sddep2, sddep1 )))
                                
                         
    

## Quadratics

sleep75$age2<-sleep75$age*sleep75$age
regquad<-lm(sleep~age+age2, sleep75)

summary(regquad)



## Interactions with continuous variables

reginter<-lm(sleep~age+educ+age*educ, sleep75)

summary(reginter)


######################################
#Binairy dependent variables
#####################################
                           
## Linear Probability Models
                         
                         
                           
mroz$exper2<-mroz$exper^2
reg1<-lm(inlf~nwifeinc+educ+exper+exper2+age+kidslt6+kidsge6, mroz)
summary(reg1)
                                   
                                  
                                   
                                   
                               




## Logits

#Looking at labor force participation using the `mroz` data. 

#1) estimate a logistic regression with the `glm()` function. 

#2) Running `summary()` will then get us the log-odds.

reglogit1<-glm(inlf~nwifeinc+educ+exper+exper2+age+kidslt6+kidsge6, mroz, family="binomial")
summary(reglogit1)

#To get the marginal effects:


library(margins)
marg_reglogit1<-margins(reglogit1)
summary(marg_reglogit1)


DF <- data.frame(age=30, 
                 nwifeinc=50,
                 exper=5,
                 exper2=25,
                 kidsge6=0,
                 kidslt6=1,
                 educ=12,
                 stringsAsFactors=FALSE)

marg_specific <- margins(reglogit1, data = DF)

summary(marg_specific)

## Contrasting a logit to a linear probability model (LPM)


lpm<-felm(favwin~spread+favhome+fav25+und25, data=pntsprd)
summary(lpm, robust=TRUE)

logit<-glm(favwin~spread+favhome+fav25+und25, 
           data=pntsprd, family="binomial")
marg_logit<-margins(logit)
summary(marg_logit)


#To see the difference, we plot the predicted probabilities
#generating the fitted values for both models
df<- mutate(pntsprd, lpm_prob=lpm$fitted.values,
            logit_prob=logit$fitted.values)

plot<-df%>%
  ggplot(aes(x=spread))+
  geom_point(aes(y=lpm_prob, colour="LPM"), alpha=0.6)+
  geom_point(aes(y=logit_prob, colour="Logit"), alpha=0.6)+
  geom_hline(yintercept = 1, alpha=0.7)+
  geom_hline(yintercept = 0,alpha=0.7)+
  scale_colour_manual("Model",
                      breaks=c("LPM", "Logit"),
                      values=c("blue", "forestgreen"))+
  lims(y=c(0,1.4))+
  labs(title="Predicted Win Probabilities, LPM",
       x="Spread",
       y="Probability")

## Logit vs. linear probability model 

plot

################################
## Non-standard standard errors
################################

## Robust standard errors
#Regress price on carats and depth.

reg1<-felm(price~carat+depth, diamonds)
summary(reg1)

myPlot <- ggplot(data = diamonds, aes(y = price, x = carat)) +
geom_point(color = "gray50", shape = 21) 

myPlot

reg1<-felm(price~carat+depth, diamonds)

summary(reg1, robust=TRUE)

stargazer(reg1, type = "latex" , se =  list(reg1$rse), header=FALSE)



## Clustered standard errors

nox <- as.data.frame(NOxEmissions) %>%mutate(ones = 1)
noClusters <- felm(data = nox, LNOx ~ sqrtWS )
Clusters <- felm(data = nox, LNOx ~ sqrtWS |0|0| julday)
stargazer(noClusters,Clusters, type = "latex" , header=FALSE, omit.stat = "all")
#########################################







#########################################
## Confidence intervals for predictions
#########################################


#using the bwght data from the wooldridge package
reg1<-lm(bwght~lfaminc+motheduc+parity, bwght)

summary(reg1)




## Confidence intervals for predictions: for a specific average 

#Step 1: generate new variables
bwght$lfaminc_0<-bwght$lfaminc-2.674
bwght$motheduc_0<-bwght$motheduc-12
bwght$parity_0<-bwght$parity-3

#step 2: run the new regression
reg2<-lm(bwght~lfaminc_0+motheduc_0+parity_0,bwght)

summary(reg2)



## Confidence Interval for prediction: a specific unit

#Step 1: generate new variables
bwght$lfaminc_0<-bwght$lfaminc-2.674
bwght$motheduc_0<-bwght$motheduc-12
bwght$parity_0<-bwght$parity-3

#step 2: run the new regression
reg2<-lm(bwght~lfaminc_0+motheduc_0+parity_0,bwght)
summary(reg2)

#step 4: get the estimate of the variance
summary(lm(bwght~lfaminc_0+motheduc_0+parity_0,bwght))$sigma^2
####################################



####################################
# Spatial Data in R
####################################


library(raster)
#library(vctrs)
#library(tibble)
library(GISTools)
#library(readr)
library(dplyr)
library(lubridate)
#library(xtable)
library(ggplot2)
library(rgeos)
library(rgdal)
library(maptools)
library(broom)



counties <- shapefile("data/PA_counties.shp")
counties

class(counties)

slotNames(counties)

names(counties@data) <- tolower(names(counties@data))
head(counties@data)

counties@proj4string


#1) we extract the shapefile data so we can use it later
#2) we convert our polygon into a data frame that `ggplot2` can handle, using `broom`'s `tidy()` function. 
#3) we merge, or `join()`, the data that came with the shapefile, into this new dataframe.

#We are going to do this process several times, so we'll write a function to take care of it for us:

mapToDF <- function(shapefile) {
  # first assign an identifier to the main dataset (each row gets a unique identifier)
  shapefile@data$id <- rownames(shapefile@data)
  # now "tidy" our data to convert it into a dataframe that
  #is usable by ggplot2
  mapDF <- tidy(shapefile) %>%
    # and join this data onto the information attached to the shapefile
    left_join(., shapefile@data, by = "id") %>%
    as.data.frame()
  return(mapDF)
}

paCounties <- mapToDF(counties)


#I start by defining a `ggplot` theme for my maps and then plot the data:


myMapThemeStuff <- theme(panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "gray5"),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    legend.key = element_blank()
)

paMap <- ggplot(data = paCounties, aes(x = long, y = lat, group = id)) +
    geom_polygon(color = "black", fill = "white") +
    myMapThemeStuff + 
    ggtitle("Pennsylvania's counties") +
    xlab("Longitude") + 
    ylab("Latitude")

paMap



wells <- read.csv("data/PA_wells.csv") %>%as.data.frame()

names(wells) <- tolower(names(wells))
head(wells)



#the coordinates() function sets spatial coordinates to define a spatial object
coordinates(wells) <-~longitude + latitude
class(wells)

#the proj4string() function retreives the projection attributes of the wells object
proj4string(wells)

#The website did not specify a projection so we select WGS84 (a common global projection) and then re-project it to match our county data's.

# assign a projection (WGS84)... check out https://rspatial.org/raster/spatial/6-crs.html 
# for more info on coordinate reference systems
proj4string(wells) <- CRS("+proj=longlat +datum=WGS84")
# re-project this to match the county data
wells <- spTransform(wells, CRS(proj4string(counties)))
#check
proj4string(wells)

#convert back to a dataframe  so that ggplot2 can handle it
wellsDF <- as.data.frame(wells)

paMap <- ggplot() +
    geom_polygon(data = paCounties, aes(x = long, y = lat, group = id),
                 color = "black", fill = "white") +
    geom_point(data = wellsDF, aes(x = longitude, y = latitude),
               shape = 21, color = "gray50") +
    myMapThemeStuff + 
    ggtitle("Unconventional Drilling in Pennsylvania") +
    xlab("Longitude") + 
    ylab("Latitude")

paMap

#Let's plot different colors by year of well drilling. 
#1) convert the spud date (drill date) variable to date format, 
#2) extract the year (let's actually make pairs of years)
#3) convert this into a factor. 


wellsDF <- mutate(wellsDF, date = mdy(spud_date), year = year(date)) %>%
                 mutate(year = 2*(floor(year / 2)))

wellsDF <- mutate(wellsDF, year = as.factor(year))


paMap <- ggplot() +
    geom_polygon(data = paCounties, aes(x = long, y = lat, group = id),color = "black", fill = "white") +
    geom_point(data = wellsDF, aes(x = longitude, y = latitude, color = year),shape = 21) +
    scale_color_brewer(palette="Blues") +
    myMapThemeStuff + 
    ggtitle("Unconventional Drilling in Pennsylvania") +
    xlab("Longitude") + 
    ylab("Latitude") +
    labs(color = "Year")
paMap

#Let's bring in some new data from the EIA: a shapefile of the Marcellus shale play - the rock formation from which you can extract hydrocarbons.

playBdry <- shapefile("data/ShalePlay_Marcellus_Boundary_EIA_Aug2015_v2.shp")
playBdry

playBdry@proj4string


#This file is WGS84. We'll have to convert it:

playBdry <- spTransform(playBdry, CRS(proj4string(counties)))

#Use our mapToDF() function from earlier to convert this into a dataframe:

bdryDF <- mapToDF(playBdry)


bigPlot <- ggplot(data = bdryDF, aes(x = long, y = lat)) +
        geom_polygon(data = paCounties, aes(x = long, y= lat, group = id), color = "gray75", fill = "NA") +
        geom_path(data = bdryDF, aes(x = long, y = lat), color = "red") +
        geom_point(data = wellsDF, aes(x = longitude, y = latitude, color = year),shape = 21) +
        scale_color_brewer(palette="Blues") +
      # put in a bounding box to restrict ourselves to the
      # part of the play in PA
        xlim(counties@bbox[1,1], counties@bbox[1, 2]) +
        ylim(counties@bbox[2,1], counties@bbox[2, 2]) +
        ggtitle("Unconventional Drilling in Pennsylvania") +
        xlab("Longitude") + 
        ylab("Latitude") +
        myMapThemeStuff+
        labs(color = "Year")

bigPlot


#We can count wells using the `poly.counts()` function from the GISTools package.

# return the number of wells in a county
wellsInCty <- poly.counts(wells, counties) %>%
      as.data.frame() %>%
      mutate(id = rownames(counties@data))

names(wellsInCty) <- c("wells", "id")

wellsInCty <- mutate(wellsInCty, wells = ifelse(is.na(wells) == TRUE, 0, wells))
head(wellsInCty)

paCounties <- left_join(paCounties, wellsInCty, by = "id")

countyPlot <- ggplot(data = bdryDF, aes(x = long, y = lat)) +
    #set the fill color to be the number of wells
      geom_polygon(data = paCounties, aes(x = long, y = lat, group = id, fill = wells), color = "black")+ 
      geom_path(data = bdryDF, aes(x = long, y = lat), color = "red") +
      geom_point(data = wellsDF, aes(x = longitude, y = latitude, color = year),shape = 21) +
      scale_color_brewer(palette="Blues") +
      xlim(counties@bbox[1,1], counties@bbox[1, 2]) +
      ylim(counties@bbox[2,1], counties@bbox[2, 2]) +
      ggtitle("Unconventional Drilling in Pennsylvania") +
      xlab("Longitude") + 
      ylab("Latitude") +
      scale_fill_gradient(low = "white", high = "deepskyblue3") +
      labs(fill = "Number of wells") +
      myMapThemeStuff+
      labs(color = "Year")

countyPlot

countyData <- counties@data %>%
      as.data.frame() %>%
      mutate(id = rownames(counties@data)) %>%
    # for easier merging later
      rename(fips = countyfpec)


#Let's merge this with our well counts, and only keep the few variables that we need:
countyWells <- left_join(wellsInCty, countyData, by="id")
countyWells <-countyWells[,c("fips", "id", "wells")]

#let's bring in our (long-promised) demographic data:
countyDemogs <- read.csv("data/PA_county_data.csv")%>%
        # remove the row for the whole state
          filter(NAME != "Pennsylvania")

countyDemogs<-countyDemogs[,c('COUNTY', 'NAME', 'Total.Population',
                              'Median.Age', 'Average.Household.Size')] 

countyDemogs<-countyDemogs%>%rename(fips = 'COUNTY', name = 'NAME', totp = 'Total.Population',
                  medage = 'Median.Age', avghhsize = 'Average.Household.Size')

#We need to combine this with our wells data, using the FIPS code:
countyWells$fips<-as.numeric(as.character(countyWells$fips))

analysisData <- left_join(countyDemogs, countyWells, by = "fips") 

head(analysisData)

#We can finally run a regression!
myReg <- lm(wells~totp+medage+avghhsize, analysisData)
summary(myReg)





#Let's load it into R, and make sure that R knows that it's a raster.
# make the raster layer
lights <- readAsciiGrid("data/palights.txt") %>%
      raster()
# create a dataframe version
lightsDF <- readAsciiGrid("data/palights.txt") %>%
      as.data.frame()
names(lightsDF) <- c("dn", "long", "lat")


lightsPlot <- ggplot(data = lightsDF, aes(x = long, y = lat)) +
        geom_raster(aes(fill = dn)) +
        geom_polygon(data = paCounties,aes(x = long, y = lat, group = id), 
                     color = "deepskyblue2", fill = "NA") +
        myMapThemeStuff + 
        labs(fill = "Digital Number") +
        scale_fill_gradient(low = "black", high = "white")


# Nightlights

lightsPlot




#calculate the average lights value for each county (this might take a little while to run):

countyLights <- extract(lights, counties, fun = mean, na.rm = T, df = T) %>%
        as.data.frame()

countyLights <- mutate(countyLights, id = as.character(ID)) 

countyLights<-countyLights[,c("palights.txt", "id")]
names(countyLights) <- c("dn", "id")


#And merge into our county data:

analysisData <- left_join(analysisData, countyLights, by = "id") %>%
        na.omit()
head(analysisData)


#Now we can run our regression:

myReg2 <- lm(wells~dn, analysisData)
summary(myReg2)




####################################
