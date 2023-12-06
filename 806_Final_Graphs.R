###806_Final

#Load libraries and data
library(tidyverse)
library(dplyr)
library(readr)
library(gapminder)
library(ggplot2)
library(car)
clean806b<-read.csv("HW/clean806b.CSV")


### ALL SITES
FourSites <- clean806b %>% 
  group_by(Site, Treatment, Harvest.Time, Innoculation) %>% 
  summarize(TotalEi = mean(meanEi), TotalLi = mean(meanLi))
str(FourSites)
FourSites$Site<-as.factor(FourSites$Site)
FourSites$Treatment<-as.factor(FourSites$Treatment)
FourSites$Innoculation<-as.factor(FourSites$Innoculation)
FourSites$Harvest.Time<-as.factor(FourSites$Harvest.Time)
str(FourSites)

# plotting treatment, Innoculation, and Harvest.time agaisnt TotalEi and TotalLi

#TotalEi, site, treatment
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Site)) + 
  geom_boxplot()+
  labs(y = "Average Early Inoculation Lesion Area (mm)")

#TotalLi, site, treatment
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Site)) +
  geom_boxplot()+
  labs(y = "Average Late Inoculation Lesion Area (mm)")

#totalEi, treatment, innoculaiton
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Innoculation)) +
  geom_boxplot()+
  labs(y = "Average Early Inoculation Lesion Area (mm)")

#TotalLi, Treatment, Innoculaiton
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Innoculation)) +
  geom_boxplot()+
  labs(y = "Average Late Inoculation Lesion Area (mm)")

#TotalEi, treatment, harvest.time
ggplot(data = FourSites, aes(x= Treatment, y = TotalEi, fill = Harvest.Time)) + 
  geom_boxplot()+
  labs(y = "Average Early Inoculation Lesion Area (mm)")

#TotalLi, treatment, harvest.time
ggplot(data = FourSites, aes(x= Treatment, y = TotalLi, fill = Harvest.Time)) + 
  geom_boxplot()+
  labs(y = "Average Late Inoculation Lesion Area (mm)")
  
###Anova

anov_fs<- FourSites %>%
 drop_na()
str(anov_fs)

##TRT+SITE

# Ei Treatment
Ei_Tr <- aov(TotalEi~Treatment + Site, data = anov_fs)
summary(Ei_Tr)
    #Site is significant with a P-value of 0.00344**

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Ei_Tr)
anov_fs$preds <- predict(Ei_Tr)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vase shape -> sqrt trans can improve test results but all tests nonsig

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids)

#p-value nonsig - good

### Perform Levene's Test for homogenity of variances
leveneTest(TotalEi ~ Treatment, data = anov_fs) #nonsig - good
leveneTest(TotalEi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df<-lm(TotalEi ~ Treatment + Site + sq_preds, anov_fs)
anova(avemod_1df) #look at sq_preds p-value - nonsig - good

# Li Treatment

Li_Tr <- aov(TotalLi~Treatment + Site, data = anov_fs)
summary(Li_Tr)
    #Site is significant with a P-value of 0.032*

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Li_Tr)
anov_fs$preds <- predict(Li_Tr)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vaguely u shape -> log trans can improve test results but all tests nonsig

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids) 

#p-value MARGINALLY nonsig - good

### Perform Levene's Test for homogenity of variances
leveneTest(TotalLi ~ Treatment, data = anov_fs) #nonsig - good
leveneTest(TotalLi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df2<-lm(TotalLi ~ Treatment + Site + sq_preds, anov_fs)
anova(avemod_1df2) #look at sq_preds p-value - nonsig - good

##INOCULATIONS+SITE

# Ei Inoc

Ei_In <- aov(TotalEi~Treatment + Innoculation, data = anov_fs)
summary(Ei_In)
    #Site is significant with a p-value of 2.85e-05***
    #Inn is significant with a p-value of 1.37e-09***

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Ei_In)
anov_fs$preds <- predict(Ei_In)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vaguely vase shape -> sqrt trans can improve test results

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids) #marginally sig - BAD!


### Perform Levene's Test for homogenity of variances
leveneTest(TotalEi ~ Innoculation, data = anov_fs) #sig - BAD!
leveneTest(TotalEi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df4<-lm(TotalEi ~ Innoculation + Site + sq_preds, anov_fs)
anova(avemod_1df4) #look at sq_preds p-value - sig - BAD!

# Li Inoc

Li_In <- aov(TotalLi~ Treatment + Innoculation, data = anov_fs)
summary(Li_In)
    #Site is significant with a p-vlaue of 0.000793***
    #Inn is significant with a p-value of 8.44e-10***

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Li_In)
anov_fs$preds <- predict(Li_In)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vaguely ?? shape -> ?? trans can improve test results

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids) 

#p-value SIG - BAD!

### Perform Levene's Test for homogenity of variances
leveneTest(TotalLi ~ Innoculation, data = anov_fs) #marignally sig - BAD!
leveneTest(TotalLi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df5<-lm(TotalLi ~ Innoculation + Site + sq_preds, anov_fs)
anova(avemod_1df5) #look at sq_preds p-value - sig - BAD!

#HARVEST+TRT

# HT Ei

#Harvest.Time
Ei_HT <- aov(TotalEi~Treatment + Harvest.Time, data = anov_fs) 
summary(Ei_HT)
    #HT is significant with a p-value of 0.0093**

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Ei_HT)
anov_fs$preds <- predict(Ei_HT)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vaguely vase shape -> sqrt trans can improve test results

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids) 

#p-value marginally nonsig - good

### Perform Levene's Test for homogenity of variances
leveneTest(TotalEi ~ Harvest.Time, data = anov_fs) #very marginally sig - bad
leveneTest(TotalEi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df6<-lm(TotalEi ~ Harvest.Time + Site + sq_preds, anov_fs)
anova(avemod_1df6) #look at sq_preds p-value - very marginally sig - bad

# HT Li

Li_HT <- aov(TotalLi~Treatment + Harvest.Time, data = anov_fs)
summary(Li_HT)
    #NEITHER Treatment nor HT are significant

#TESTING ASSUMPTIONS

#Generate residual and predicted values
anov_fs$resids <- residuals(Li_HT)
anov_fs$preds <- predict(Li_HT)
anov_fs$sq_preds <- anov_fs$preds^2
head(anov_fs)

### Plot
plot(resids ~ preds, data = anov_fs,
     xlab = "Predicted Values",
     ylab = "Residuals")
#vaguely vase shape -> sqrt trans can improve test results

### Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(anov_fs$resids) 

#p-value marginally sig - BAD!

### Perform Levene's Test for homogenity of variances
leveneTest(TotalLi ~ Harvest.Time, data = anov_fs) #nonsig - good
leveneTest(TotalLi ~ Site, data = anov_fs) #Unnecessary, because not comparing Blocks

### Perform a Tukey 1-df Test for Non-additivity
avemod_1df7<-lm(TotalLi ~ Harvest.Time + Site + sq_preds, anov_fs)
anova(avemod_1df7) #look at sq_preds p-value - nonsig - good

### TRANSFORMATIONS!


