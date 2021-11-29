#Load package data

library("nlme")
library("readxl")
library("ggeffects")
library("ggplot2")
library("effects")
library("tidyverse")


#Set working data
setwd("/Users/alistairperry/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/Output/LMM")


#Load connectivity weights data

TenPercWeights <- read_excel("/Users/alistairperry/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/Output/Interaction_EdgeWeights7.5_10_12.5%thresholdingWithoutAgeAsCovariate_Sub-groupData01.09.20.xlsx", sheet = 1, range = cell_rows(2:368))


#And full demographics to make life easier for me

Dems <- read.csv2("/Users/alistairperry/Documents/Brisbane/CNHRLongitudinal_dropbox/Misc/alln183DEMS.txt", header = TRUE, sep = ",")


#Ensure its numeric, and groups are factors

Dems<-data.frame(lapply(Dems,as.numeric))


Dems$Group<-as.factor(Dems$Group)

Dems$ID<-as.factor(Dems$ID)


#Combine them together

PosNetworkWeights <- subset(TenPercWeights, select = "Average")

Data <- cbind2(Dems, PosNetworkWeights)

Data$Age2 <- Data$Age^2

CNData <- subset(Data, Group == 1)
HRData <- subset(Data, Group == -1)



#Run LMMs together

#Controlling for group

#Example from website: lme(fixed = normexam ~ standLRT, data = Exam, random = ~ 1 | school)

AgeMod <- lme(fixed = Average ~ Age + Group, data = Data, random = ~ 1 | ID)

sumAgeMod <- summary(AgeMod)



#And non-linear variant

Age2Mod <- lme(fixed = Average ~ Age + Age2 + Group, data = Data, random = ~ 1 | ID)

sumAge2Mod <- summary(Age2Mod)



#Print and save results
sink("LMEres__grptimeint_posnet_Age.txt")
sumAgeMod
sink()

sink("LMEres__grptimeint_posnet_Age2.txt")
sumAge2Mod
sink()



#Age prediction slopes

#Predict over all individuals

##extract predicted values from model first

PredAgeSlp <- as.data.frame

PredAgeSlp = predict(AgeMod)
effects_Age <- effects::effect(term="Age", mod=AgeMod)
summary(effects_Age)
x_Age <- as.data.frame(effects_Age)


posall <- ggplot(data = Data, aes(x = Age, y = Average)) +

geom_point(size = 2, aes(color = Group)) + #colour points by group
  
geom_path(aes(group = ID, color = Group)) +

  
#Note HR will be first (-1)    
scale_colour_manual(values = c("DarkOrange", "DodgerBlue")) +
   
geom_line(data=x_Age, aes(x=Age, y=fit), size = 1, color='black') + 
  
geom_ribbon(data=x_Age, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25) +
  
ylab("Mean Connectivity") +

xlab("Age (years)") + 
  
theme_classic()

posall


figfname<- 'LMM_grptimeint_agepred.png'

ggsave(figfname, width = 6, height = 4)


#Run LMM for groups separately 
#First controls

AgeModCN <- lme(fixed = Average ~ Age, data = CNData, random = ~ 1 | ID)

effects_AgeCN <- effects::effect(term="Age", mod=AgeModCN)
summary(effects_AgeCN)
x_AgeCN <- as.data.frame(effects_AgeCN)


#Now HR

AgeModHR <- lme(fixed = Average ~ Age, data = HRData, random = ~ 1 | ID)

effects_AgeHR <- effects::effect(term="Age", mod=AgeModHR)
summary(effects_AgeHR)
x_AgeHR <- as.data.frame(effects_AgeHR)


sumAgeModCN <- summary(AgeModCN)
sumAgeModHR <- summary(AgeModHR)


#Write out table results
sink("LMEres__grptimeint_posnet_Age_CN.txt")
sumAgeModCN
sink()

sink("LMEres__grptimeint_posnet_Age_HR.txt")
sumAgeModHR
sink()


#Plot them together

posplotsep <- ggplot(data = Data, aes(x = Age, y = Average)) +
  
  geom_point(size = 2, aes(color = Group)) + #colour points by group
  
  geom_path(aes(group = ID, color = Group)) +
  
  
  #Note HR will be first (-1)    
  scale_colour_manual(values = c("DarkOrange", "DodgerBlue")) +
  
  geom_line(data=x_AgeCN, aes(x=Age, y=fit), size = 1, color='DodgerBlue') +
  
  geom_line(data=x_AgeHR, aes(x=Age, y=fit), size = 1, color='DarkOrange') +
  
  geom_ribbon(data=x_AgeCN, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DodgerBlue") +
  
  geom_ribbon(data=x_AgeHR, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DarkOrange") +
  
  
  ylab("Mean Connectivity") +
  
  xlab("Age (years)") + 
  
  theme_classic()
  
posplotsep


figfname<- 'LMM_grptimeint_possep_agepred.png'

ggsave(figfname, width = 6, height = 4)


#Lastly, Group x Age Interaction

AgeModGrpInt <- lme(fixed = Average ~ Age + Group + Age*Group, data = Data, random = ~ 1 | ID)

sumAgeModGrpInt<-summary(AgeModGrpInt)

sink("LMEres_grptimeint_posnet_AgeGrpInt.txt")
sumAgeModGrpInt
sink()


#And control for time

AgeModGrpIntwT <- lme(fixed = Average ~ Age + Group + Time + Age*Group, data = Data, random = ~ 1 | ID)

sumAgeModGrpIntwT<-summary(AgeModGrpIntwT)

sink("LMEres_grptimeint_posnet_AgeGrpInt_wT.txt")
sumAgeModGrpIntwT
sink()