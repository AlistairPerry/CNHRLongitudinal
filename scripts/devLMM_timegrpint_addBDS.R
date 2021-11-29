#Load package data

library("nlme")
library("readxl")
library("ggeffects")
library("ggplot2")
library("effects")
library("tidyverse")
library("plyr")


#Set working data
setwd("/Users/alistairperry/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/AddingBDS/Output/LMM")


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


#Now load in BD data

TenPercWeightsBD <- read_excel("/Users/alistairperry/Documents/Brisbane/CNHRLongitudinal_dropbox/NBS_interaction/AddingBDS/Interaction_EdgeWeights7.5_10_12.5%25thresholdingWithoutAgeAsCovariate_Sub-groupData-IncludesBDsubjects_12.04.21_onlyn52BDs.xlsx", sheet = 1, range = cell_rows(370:422))

BDData <- subset(TenPercWeightsBD, select = c("values_Average", "Age"))

colnames(BDData)<-c("Average", "Age")


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


#But obviously only a linear model for BDs

AgeModBD <- lm(Average ~ Age, data = BDData)

effects_AgeBD <- effects::effect(term="Age", mod=AgeModBD)
summary(effects_AgeBD)
x_AgeBD <- as.data.frame(effects_AgeBD)



#Pull them into one matrix for plotting purposes

exCN<-subset(CNData, select = c("Average", "Age"))
exHR<-subset(HRData, select = c("Average", "Age"))

exCN$Group<-as.factor(1)
exHR$Group<-as.factor(2)
BDData$Group<-as.factor(3)

NewData <- rbind(exCN, exHR, BDData)



#Plot the 3 groups all together

posplotsep <- ggplot(data = NewData, aes(x = Age, y = Average)) +
  
  geom_point(size = 2, aes(color = Group)) + #colour points by group

  
  #Note HR will be first (-1)    
  scale_colour_manual(values = c("DodgerBlue", "DarkOrange", "DarkGreen")) +
  
  geom_line(data=x_AgeCN, aes(x=Age, y=fit), size = 1, color='DodgerBlue') +
  
  geom_line(data=x_AgeHR, aes(x=Age, y=fit), size = 1, color='DarkOrange') +

  geom_line(data=x_AgeBD, aes(x=Age, y=fit), size = 1, color='DarkGreen') +
  
  
  geom_ribbon(data=x_AgeCN, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DodgerBlue") +
  
  geom_ribbon(data=x_AgeHR, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DarkOrange") +
  
  geom_ribbon(data=x_AgeBD, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DarkGreen") +
  
  
  ylab("Mean Connectivity") +
  
  xlab("Age (years)") + 
  
  theme_classic()


posplotsep


figfname<- 'LMM_grptimeint_possep_agepred_addBDS.tiff'

ggsave(figfname, width = 6, height = 4)


#3-Interaction on FW data (in HR and BD)

#First ensure group ids are correct
CNHRData_FW <- subset(Data, Time == -1, select = c("Group", "Average", "Age"))

CNHRData_FW$Group <- as.character(CNHRData_FW$Group)

CNHRData_FW$Group[CNHRData_FW$Group =="-1"] <- "2"

BDData$Group <- as.character(BDData$Group)


#Now join

comCNHRFWBD <- rbind2(CNHRData_FW, BDData)

comCNHRFWBD$Group <- as.factor(comCNHRFWBD$Group)

fit2 = aov(Average~Group+Age+Group*Age, comCNHRFWBD)


#Print and save results
sink("LMres_CNHRatFW_agepred_addBDS.txt")
summary(fit2)
sink()
 

#For Completeness, Fig2C only using FW data

#First controls

CNData_FW <- subset(Data, Group == 1 & Time == -1, select = c("Group", "Average", "Age"))

AgeModCN_FW <- lm(Average ~ Age, data = CNData_FW)

effects_AgeCNFW <- effects::effect(term="Age", mod=AgeModCN_FW)
summary(effects_AgeCNFW)
x_AgeCNFW <- as.data.frame(effects_AgeCNFW)


#Now HR

HRData_FW <- subset(Data, Group == -1 & Time == -1, select = c("Group", "Average", "Age"))

AgeModHR_FW <- lm(Average ~ Age, data = HRData_FW)

effects_AgeHRFW <- effects::effect(term="Age", mod=AgeModHR_FW)
summary(effects_AgeHRFW)
x_AgeHRFW <- as.data.frame(effects_AgeHRFW)


#Now Plot

#Pull them into one matrix for plotting purposes

exCN<-subset(CNData_FW, select = c("Average", "Age"))
exHR<-subset(HRData_FW, select = c("Average", "Age"))

exCN$Group<-as.factor(1)
exHR$Group<-as.factor(2)
BDData$Group<-as.factor(3)

NewData <- rbind(exCN, exHR, BDData)


#Plot the 3 groups all together

posplotCNHRFW <- ggplot(data = NewData, aes(x = Age, y = Average)) +
  
  geom_point(size = 2, aes(color = Group)) + #colour points by group
  
  
  #Note HR will be first (-1)    
  scale_colour_manual(values = c("DodgerBlue", "DarkOrange", "DarkGreen")) +
  
  geom_line(data=x_AgeCNFW, aes(x=Age, y=fit), size = 1, color='DodgerBlue') +
  
  geom_line(data=x_AgeHRFW, aes(x=Age, y=fit), size = 1, color='DarkOrange') +
  
  geom_line(data=x_AgeBD, aes(x=Age, y=fit), size = 1, color='DarkGreen') +
  
  
  geom_ribbon(data=x_AgeCNFW, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DodgerBlue") +
  
  geom_ribbon(data=x_AgeHRFW, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DarkOrange") +
  
  geom_ribbon(data=x_AgeBD, aes(y = fit, ymin=lower, ymax=upper), linetype=2, alpha=0.25, fill = "DarkGreen") +
  
  
  ylab("Mean Connectivity") +
  
  xlab("Age (years)") + 
  
  theme_classic()


posplotCNHRFW


figfname<- 'LM_grptimeint_possepCNHRFW_agepred_addBDS.tiff'

ggsave(figfname, width = 6, height = 4)


