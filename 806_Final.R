###806_Final

#Load libraries and data
library(tidyverse)
library(maps)
library(sf)
library(ggplot2)
library(tigris)
library(leaflet)

rawdata<-read.csv("C:/Users/ereil/OneDrive/Desktop/806/806.CSV")


#Clean data
trimdat <- rawdata %>% 
  select(-one_of('EI_ID', 'Li_ID'))
head(trimdat)

avedat<- trimdat %>%
  group_by(Site, TreeNo, Treatment, Innoculation, Harvest.Time) %>%
  summarize(meanEi = mean(Ei_Lesion.Area..mm.), meanLi = mean(Li_Lesion.Area..mm.))
head(avedat)
avedat<-avedat[-c(1),]
head(avedat)

#New .csv
write_csv(avedat, "C:/Users/ereil/OneDrive/Desktop/806/clean806b.CSV")

# Map of study sites

#New Hampshire!
nh<-map_data("county", region = "new hampshire")
siteloc<-read.csv("C:/Users/ereil/OneDrive/Documents/806_1/siteloc.CSV")
stcount<- nh %>%
  filter(subregion=="strafford")
stsiteloc<- siteloc[-c(2),]

#Mapping site locations
ggplot(nh, aes(long, lat))+
  geom_polygon(aes(group = group))+
  geom_path(color="gray", aes(group=group))+
  geom_point(data = siteloc, aes(x = long, y = lat, color = Site))+
  scale_color_manual("Site", values = c("green", "red", "blue"))+
  theme(legend.position = 'right')+ 
  coord_quickmap()+
  labs(title = "Study Site Locations")+xlab("Longitude")+ylab("Latitude")

ggplot(stcount, aes(long, lat))+
  geom_polygon(aes(group = group))+
  geom_path(color="gray", aes(group=group))+
  geom_point(data = stsiteloc, aes(x = long, y = lat, color = Site))+
  scale_color_manual("Site", values = c("red", "blue"))+
  theme(legend.position = 'right')+
  coord_quickmap()+
  labs(title = "Study Site Locations: Strafford County")+xlab("Longitude")+ylab("Latitude")



# Overall comparison of DUR/MES/CTRL across all sites

# Ei ANOVA
avedat<-avedat[-c(1),]
str(avedat)
avedat$Site<-as.factor(avedat$Site)
avedat$Treatment<-as.factor(avedat$Treatment)
avedat$Innoculation<-as.factor(avedat$Innoculation)
avedat$Harvest.Time<-as.factor(avedat$Harvest.Time)
str(avedat)
avedat<-avedat %>%
  drop_na()

avemodEi<-lm(meanEi ~ Treatment + Site + Harvest.Time + Treatment:Site + Treatment:Harvest.Time + Site:Harvest.Time, avedat)
anova(avemodEi)

#TESTING ASSUMPTIONS

#Generate residual and predicted values
avedat$resids <- residuals(avemodEi)
avedat$preds <- predict(avemodEi)
avedat$sq_preds <- avedat$preds^2
head(avedat)

### Plot
plot(resids ~ preds, data = avedat,
     xlab = "Predicted Values",
     ylab = "Residuals")

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(avedat$resids)

#p-value = <2.2e-16; resids not normal

### Perform Levene's Test for homogenity of variances
#install.packages("car")
library(car)
leveneTest(meanEi ~ Treatment, data = avedat) #nonsig - good
leveneTest(meanEi ~ Site, data = avedat) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df<-lm(meanEi ~ Treatment + Site + sq_preds, avedat)
anova(avemod_1df) #look at sq_preds p-value - sig :[

#Transformation attemot 1 - sqrt
avedat$trans_means<-sqrt(0.5 + avedat$meanEi)






avemodLi<-lm(meanLi ~ Treatment + Site + Harvest.Time, avedat)
anova(avemodLi)




# Comparison of DUR/MES/CTRL each site

# Comparison of NEi to Ei (ALTON ONLY)

# Comparison of Alton to Durham/Madbury sites

#Lesion size difference total

avedat_lesions<-avedat %>%
  drop_na()

ggplot(avedat_lesions, aes(x = Site, y = meanEi))+
  geom_bar(stat = "identity")
ggplot(avedat_lesions, aes(x = Site, y = meanLi))+
  geom_bar(stat = "identity")

ggplot(avedat_lesions, aes(x = meanLi, y = meanEi, color = Treatment, shape = Site)) +
  geom_point()



avedat2<- avedat %>%
  group_by(Site, Treatment) %>%
  summarize(meanEitotal = mean(meanEi), meanLitotal = mean(meanLi))
head(avedat2)



