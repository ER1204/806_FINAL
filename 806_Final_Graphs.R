###806_Final

#Load libraries and data
library(tidyverse)
library(dplyr)
library(readr)
library(gapminder)
clean806b<-read.csv("HW/clean806b.CSV")
view(clean806b)

#Clean data
trimdat <- rawdata %>% 
  select(-one_of('EI_ID', 'Li_ID'))
head(trimdat)

avedat<- trimdat %>%
  group_by(Site, TreeNo, Treatment, Innoculation, Harvest.Time) %>%
  summarize(meanEi = mean(Ei_Lesion.Area..mm.), meanLi = mean(Li_Lesion.Area..mm.))
head(avedat)

#New .csv
write_csv(avedat, "C:/Users/ereil/OneDrive/Desktop/806/clean806b.CSV")






FourSites <- clean806b %>% group_by(Site, Treatment, Harvest.Time, Innoculation) %>% summarize(TotalEi = mean(meanEi), TotalLi = mean(meanLi)) %>% drop_na()
view(FourSites)



#plotting treatment, Innoculation, and Harvest.time agaisnt TotalEi and TotalLi
library(ggplot2)

#TotalEi, site, treatment
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Site)) + geom_bar(stat = 'identity', position = 'dodge')

#TotalLi, site, treatment
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Site)) + geom_bar(stat = 'identity', position = 'dodge')

#totalEi, treatment, innoculaiton
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Innoculation)) + geom_bar(stat = 'identity', position = 'dodge')

#TotalLi, Treatment, Innoculaiton
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Innoculation)) + geom_bar(stat = 'identity', position = 'dodge')

#TotalEi, treatment, harvest.time
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Harvest.Time)) + geom_bar(stat = 'identity', position = 'dodge')

#TotalLi, treatment, harvest.time
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Harvest.Time)) + geom_bar(stat = 'identity', position = 'dodge')


#Anova

#Treatment
Ei_Tr <- aov(TotalEi~Treatment + Site, data = FourSites)
summary(Ei_Tr)
    #Site is significant with a P-value of 0.00344**
Li_Tr <- aov(TotalLi~Treatment + Site, data = FourSites)
summary(Li_Tr)
    #Site is significant with a P-value of 0.032*

#Innoculation
Ei_In <- aov(TotalEi~Site + Innoculation, data = FourSites)
summary(Ei_In)
    #Site is significant with a p-value of 2.85e-05***
    #Inn is significant with a p-value of 1.37e-09***
Li_In <- aov(TotalLi~ Site + Innoculation, data = FourSites)
summary(Li_In)
    #Site is significant with a p-vlaue of 0.000793***
    #Inn is significant with a p-value of 8.44e-10***

#Harvest.Time
Ei_HT <- aov(TotalEi~Site + Harvest.Time, data = FourSites) 
summary(Ei_HT)
    #Site is significant with a p-value of 0.00455**

Li_HT <- aov(TotalLi~Site + Harvest.Time, data = FourSites)
summary(Li_HT)
    #Site is significant with a P-value of 0.0322*













