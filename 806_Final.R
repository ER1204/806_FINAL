###806_Final

#Load libraries and data
library(tidyverse)
library(dplyr)
rawdata<-read.csv("C:/Users/ereil/OneDrive/Desktop/806/806.CSV")


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
