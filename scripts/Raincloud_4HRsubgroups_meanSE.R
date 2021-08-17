# longitunial raincloud plots with group mean +/-SEM over the top
# 4 plots for multi-panel figure of sub-groups within clinical cohort
# 1. HR+ any new DSM diagnosis
# 2. HR  existing mood episode at baseline
# 3. HR any new mood episode
# 4. HR converted to BD diagnosis (manic+dep episode)

####### Credits
# this version was written by Megan EJ Campbell - 2021
# it was based on Alistair Perry's 2020 edited version of XX
###### Refs? 

# #Load packages
library("plyr")
library("lattice")
library("ggplot2")
library("dplyr")
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")
library("readxl")

# width and height variables for saved plots - leave it as default for now
w = 5
h = 5

# Set dirs
setwd(">>> enter in base directory <<<")
TenPercCons <- read_excel(">>> select data source file and adjust range as needed<<< .xlsx", sheet = 1, range = cell_rows(2:368))
FigOutDir <- (">>> enter in figure output directory<<<")

##############################
## plot 1:  
# subgroup (red colour) code that indicates those with a any DSM diagnosis since baseline

HR1 <- subset(TenPercCons, Group == -1 & Time == 1)
HR1dat <- HR1$Average
nhr <- length(HR1dat)

# 1=HR who had a mood ep before Time 1 (baseline mood ep), 2=CONs, 3=Remainder HR (didn't have a mood ep at Time 1)
#Time 1 is 1, Timepoint 2 is -1

HR1wDSM <- subset(TenPercCons, Group == -1 & Time == 1 & `NewOnsetAnyDsm-IV` == 1, select=c(ID_Order, Average))
HR2wDSM <- subset(TenPercCons, Group == -1 & Time == -1 & `NewOnsetAnyDsm-IV` == 1, select=c(ID_Order, Average))

HR1woutDSM <- subset(TenPercCons, Group == -1 & Time == 1 & `NewOnsetAnyDsm-IV` == 3, select=c(ID_Order, Average))
HR2woutDSM <- subset(TenPercCons, Group == -1 & Time == -1 & `NewOnsetAnyDsm-IV` == 3, select=c(ID_Order, Average))


HR1wDSMdat <- HR1wDSM$Average
HR2wDSMdat <- HR2wDSM$Average

HR1woutDSMdat <- HR1woutDSM$Average
HR2woutDSMdat <- HR2woutDSM$Average

# HR1DSMdat <- HR1DSM$Average
# HR2DSMdat <- HR2DSM$Average

nwDSM <- length(HR1wDSMdat)
nwoutDSM <- length(HR1woutDSMdat)

nwDSMscans <- nwDSM*2
nwoutDSMscans <- nwoutDSM*2
totalscans <- nwDSMscans+nwoutDSMscans

x1 <- as.integer(totalscans)
x1[1:nwDSMscans] <- rep(c(1,2), each=nwDSM)
x1[(nwDSMscans+1):totalscans] <- rep(c(3,4), each=nwoutDSM)


wDSMids <- rep(HR1wDSM$ID_Order,2)
woutDSMids <-rep(HR1woutDSM$ID_Order,2)

#wDSM <- rep(HR1wDSM$ID_Order,2)

d <- data.frame(y = c(HR1wDSMdat, HR2wDSMdat, HR1woutDSMdat, HR2woutDSMdat), x = x1, ID_Order = as.factor(c(wDSMids, woutDSMids)))


#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,1,2) # group by the time?

score_mean_1 <- mean(HR1wDSMdat)
score_mean_2 <- mean(HR2wDSMdat)
score_mean_3 <- mean(HR1woutDSMdat)
score_mean_4 <- mean(HR2woutDSMdat)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)

## change error bars from 95CI to SE)
mean_se_1 <- mean_se(HR1wDSMdat, mult =1)
mean_se_2 <- mean_se(HR2wDSMdat, mult =1)
mean_se_3 <- mean_se(HR1woutDSMdat, mult =1)
mean_se_4 <- mean_se(HR2woutDSMdat, mult =1)

score_se_1 <- score_mean_1-mean_se_1$ymin
score_se_2 <- score_mean_2-mean_se_2$ymin
score_se_3 <- score_mean_3-mean_se_3$ymin
score_se_4 <- score_mean_4-mean_se_4$ymin
se <- c(score_se_1, score_se_2, score_se_3, score_se_4)

summary_df <- data.frame(group, score_mean, se)


#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above

#scale limits
y_lim_min <- min(d$y)
y_lim_max <- max(d$y)


#Start plotting

set.seed(321)

x1fk <- numeric((nhr)*2)

x1fk[1:nwDSMscans] <- rep(c(1,2), each=nwDSM)
x1fk[(nwDSMscans+1):totalscans] <- rep(c(1,2), each=nwoutDSM)

xj <- jitter(x1fk, amount = .09)


d$xj <- xj


d$ID_Order[1]

#shifted means to edges to not obscure things
xx1 = 0.9
xx2 = 2.1

############################ Note plotting things should be the same regardless of subgroup (data now in "d" and "score_mean") 
f1 <- ggplot(data = d, aes(y = y)) +
  
  #Add geom_() objects
  # individual means at baseline and followup for 2 groups
  geom_point(data = d %>% filter(x =="3"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  
  ## join means with line
  geom_line(data = d %>% filter(x =="3" | x =="4"), aes(x = xj, group = ID_Order), color = 'darkgray', alpha = 0.5) +
  
  geom_line(data = d %>% filter(x =="1" | x =="2"), aes(x = xj, group = ID_Order), color = 'red', alpha = 0.5) +
  
  #clouds for rainclouds!
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = -2.5), 
    side = "l", fill = 'darkgray', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "r", fill = 'darkgray', alpha = .6) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'red', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = .5), 
    side = "r", fill = 'red', alpha = .6) + 
  
  
  ##Group means #Add lines connecting the two means
  geom_line(data = summary_df[3:4,], aes(x = c((xx1-0.05),(xx2+0.05)), y = score_mean),  # nudged grey  means a bit wider so they don't overlap red
            color = 'darkgray', size = 1.5, alpha = 1) + 
  geom_line(data = summary_df[1:2,], aes(x = c(xx1,xx2), y = score_mean),
            color = 'red', size = 1.5, alpha = 1) +
  
  
  # group means with shape = 16, filled circle & SEM error bars
  # baseline red
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1], ymin = score_mean[1]-se[1], ymax = score_mean[1]+se[1]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  
  
  # #baseline grey
  geom_errorbar(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3], ymin = score_mean[3]-se[3], ymax = score_mean[3]+se[3]),
                position = position_nudge(-0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3]),
             position = position_nudge(-0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  #position = position_nudge(x = 0), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  #followup red
  
  geom_errorbar(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2], ymin = score_mean[2]-se[2], ymax = score_mean[2]+se[2]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  # follow-up grey
  geom_errorbar(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4], ymin = score_mean[4]-se[4], ymax = score_mean[4]+se[4]),
                position = position_nudge(0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4]),
             position = position_nudge(x = 0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  # formatting 
  
  scale_x_continuous(breaks=c(1,2), labels=c("Baseline", "Follow Up"))+
  
  ylab("Mean Connectivity") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max))


f1  ########## 
# export and save as 500x500
figfname<-paste(FigOutDir, '/GrpTimeInt_newDSM.png', sep="")
ggsave(figfname, width = w, height = h)

################################################
## Plot 2 
# 1=HR who had a mood ep before Time 1 (baseline mood ep), 2=CONs, 3=Remainder HR (didn't have a mood ep at Time 1)
#Time 1 is 1, Timepoint 2 is -1

HR1wBaseMood <- subset(TenPercCons, Group == -1 & Time == 1 & `Pre-BaselineMoodEp` == 1, select=c(ID_Order, Average))
HR2wBaseMood <- subset(TenPercCons, Group == -1 & Time == -1 & `Pre-BaselineMoodEp` == 1, select=c(ID_Order, Average))

HR1woutBaseMood <- subset(TenPercCons, Group == -1 & Time == 1 & `Pre-BaselineMoodEp` == 3, select=c(ID_Order, Average))
HR2woutBaseMood <- subset(TenPercCons, Group == -1 & Time == -1 & `Pre-BaselineMoodEp` == 3, select=c(ID_Order, Average))


HR1wBaseMooddat <- HR1wBaseMood$Average
HR2wBaseMooddat <- HR2wBaseMood$Average

HR1woutBaseMooddat <- HR1woutBaseMood$Average
HR2woutBaseMooddat <- HR2woutBaseMood$Average

# HR1basemooddat <- HR1basemood$Average
# HR2basemooddat <- HR2basemood$Average

nwBaseMood <- length(HR1wBaseMooddat)
nwoutBaseMood <- length(HR1woutBaseMooddat)

nwBaseMoodscans <- nwBaseMood*2
nwoutBaseMoodscans <- nwoutBaseMood*2
totalscans <- nwBaseMoodscans+nwoutBaseMoodscans

x1 <- as.integer(totalscans)
x1[1:nwBaseMoodscans] <- rep(c(1,2), each=nwBaseMood)
x1[(nwBaseMoodscans+1):totalscans] <- rep(c(3,4), each=nwoutBaseMood)


wBaseMoodids <- rep(HR1wBaseMood$ID_Order,2)
woutBaseMoodids <-rep(HR1woutBaseMood$ID_Order,2)

# wBasemood <- rep(HR1basemood$ID_Order,2)

d <- data.frame(y = c(HR1wBaseMooddat, HR2wBaseMooddat, HR1woutBaseMooddat, HR2woutBaseMooddat), x = x1, ID_Order = as.factor(c(wBaseMoodids, woutBaseMoodids)))

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,1,2) # group by the time

score_mean_1 <- mean(HR1wBaseMooddat)
score_mean_2 <- mean(HR2wBaseMooddat)
score_mean_3 <- mean(HR1woutBaseMooddat)
score_mean_4 <- mean(HR2woutBaseMooddat)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)
score1 <- mean_se(HR1wBaseMooddat, mult =1)
score_ci_1 <- CI(HR1wBaseMooddat, ci = 0.95)
score_ci_2 <- CI(HR2wBaseMooddat, ci = 0.95)

score_ci_3 <- CI(HR1woutBaseMooddat, ci = 0.95)
score_ci_4 <- CI(HR2woutBaseMooddat, ci = 0.95)

ci <- c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]), (score_ci_3[1] - score_ci_3[3]), (score_ci_4[1] - score_ci_4[3]))

summary_df <- data.frame(group, score_mean, ci)


#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above

#scale limits
y_lim_min <- min(d$y)
y_lim_max <- max(d$y)


#Start plotting

set.seed(321)

x1fk <- numeric((nhr)*2)

x1fk[1:nwBaseMoodscans] <- rep(c(1,2), each=nwBaseMood)
x1fk[(nwBaseMoodscans+1):totalscans] <- rep(c(1,2), each=nwoutBaseMood)

xj <- jitter(x1fk, amount = .09)


d$xj <- xj


d$ID_Order[1]

#shifted means to edges to not obscure things
xx1 = 0.9
xx2 = 2.1
########## plot stuff
f2 <- ggplot(data = d, aes(y = y)) +
  
  #Add geom_() objects
  # individual means at baseline and followup for 2 groups
  geom_point(data = d %>% filter(x =="3"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  
  ## join means with line
  geom_line(data = d %>% filter(x =="3" | x =="4"), aes(x = xj, group = ID_Order), color = 'darkgray', alpha = 0.5) +
  
  geom_line(data = d %>% filter(x =="1" | x =="2"), aes(x = xj, group = ID_Order), color = 'red', alpha = 0.5) +
  
  #clouds for rainclouds!
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = -2.5), 
    side = "l", fill = 'darkgray', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "r", fill = 'darkgray', alpha = .6) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'red', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = .5), 
    side = "r", fill = 'red', alpha = .6) + 
  
  
  ##Group means #Add lines connecting the two means
  geom_line(data = summary_df[3:4,], aes(x = c((xx1-0.05),(xx2+0.05)), y = score_mean),  # nudged grey  means a bit wider so they don't overlap red
            color = 'darkgray', size = 1.5, alpha = 1) + 
  geom_line(data = summary_df[1:2,], aes(x = c(xx1,xx2), y = score_mean),
            color = 'red', size = 1.5, alpha = 1) +
  
  
  # group means with shape = 16, filled circle & SEM error bars
  # baseline red
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1], ymin = score_mean[1]-se[1], ymax = score_mean[1]+se[1]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  
  # #baseline grey
  geom_errorbar(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3], ymin = score_mean[3]-se[3], ymax = score_mean[3]+se[3]),
                position = position_nudge(-0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3]),
             position = position_nudge(-0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  #position = position_nudge(x = 0), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  #followup red
  
  geom_errorbar(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2], ymin = score_mean[2]-se[2], ymax = score_mean[2]+se[2]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  # follow-up grey
  geom_errorbar(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4], ymin = score_mean[4]-se[4], ymax = score_mean[4]+se[4]),
                position = position_nudge(0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4]),
             position = position_nudge(x = 0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  # formatting 
  
  scale_x_continuous(breaks=c(1,2), labels=c("Baseline", "Follow Up"))+
  
  ylab("Mean Connectivity") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max))

f2

figfname<-paste(FigOutDir, '/GrpTimeInt_preexistMood.png', sep="")
ggsave(figfname, width = w, height = h)

###########################################
# Pot 3. HR any new mood episode
# 1=HR Any mood ep since baseline Ep , 2=CONs, 3=Remainder HR (no new mood ep)
# from CODEBOOK: "  This subgroup splits the HR condition according to whether they've had a new onset of a 
# mood episode since baseline, i.e. sometime between baseline and FW2 the participant had their first onset 
# of a type of mood episode they didn't have prior to baseline

HR1wNewMood <- subset(TenPercCons, Group == -1 & Time == 1 & `NewOnsetAnyNewKindMoodEp` == 1, select=c(ID_Order, Average))
HR2wNewMood <- subset(TenPercCons, Group == -1 & Time == -1 & `NewOnsetAnyNewKindMoodEp` == 1, select=c(ID_Order, Average))

HR1woutNewMood <- subset(TenPercCons, Group == -1 & Time == 1 & `NewOnsetAnyNewKindMoodEp` == 3, select=c(ID_Order, Average))
HR2woutNewMood <- subset(TenPercCons, Group == -1 & Time == -1 & `NewOnsetAnyNewKindMoodEp` == 3, select=c(ID_Order, Average))

#average for each group
HR1wNewMood_dat <- HR1wNewMood$Average
HR2wNewMood_dat <- HR2wNewMood$Average

HR1woutNewMood_dat <- HR1woutNewMood$Average

HR2woutNewMood_dat <- HR2woutNewMood$Average

#samplesize for each group
nwNewMood <- length(HR1wNewMood_dat)
nwoutNewMood <- length(HR1woutNewMood_dat)

nwNewMoodscans <- nwNewMood*2
nwoutNewMoodscans <- nwoutNewMood*2
totalscans <- nwNewMoodscans+nwoutNewMoodscans

x1 <- as.integer(totalscans)
x1[1:nwNewMoodscans] <- rep(c(1,2), each=nwNewMood)
x1[(nwNewMoodscans+1):totalscans] <- rep(c(3,4), each=nwoutNewMood)


wNewMoodids <- rep(HR1wNewMood$ID_Order,2)
woutNewMoodids <-rep(HR1woutNewMood$ID_Order,2)

d <- data.frame(y = c(HR1wNewMood_dat, HR2wNewMood_dat, HR1woutNewMood_dat, HR2woutNewMood_dat), x = x1, ID_Order = as.factor(c(wNewMoodids, woutNewMoodids)))

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,1,2)

score_mean_1 <- mean(HR1wNewMood_dat)
score_mean_2 <- mean(HR2wNewMood_dat)
score_mean_3 <- mean(HR1woutNewMood_dat)
score_mean_4 <- mean(HR2woutNewMood_dat)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)


## change error bars from 95CI to SE)
mean_se_1 <- mean_se(HR1wNewMood_dat, mult =1)
mean_se_2 <- mean_se(HR2wNewMood_dat, mult =1)
mean_se_3 <- mean_se(HR1woutNewMood_dat, mult =1)
mean_se_4 <- mean_se(HR2woutNewMood_dat, mult =1)

score_se_1 <- score_mean_1-mean_se_1$ymin
score_se_2 <- score_mean_2-mean_se_2$ymin
score_se_3 <- score_mean_3-mean_se_3$ymin
score_se_4 <- score_mean_4-mean_se_4$ymin
se <- c(score_se_1, score_se_2, score_se_3, score_se_4)


summary_df <- data.frame(group, score_mean, se)


#First we must again define the x-coordinates of the means.
x_tick_dats_x <- c(.87, 2.13) #same as above


y_lim_min <- min(d$y)
y_lim_max <- max(d$y)

#Start plotting

set.seed(321)

x1fk <- numeric((nhr)*2)

x1fk[1:nwNewMoodscans] <- rep(c(1,2), each=nwNewMood)
x1fk[(nwNewMoodscans+1):totalscans] <- rep(c(1,2), each=nwoutNewMood)

xj <- jitter(x1fk, amount = .09)

d$xj <- xj

#shifted means to edges to not obscure things
xx1 = 0.9
xx2 = 2.1
##################

f3 <- ggplot(data = d, aes(y = y)) +
  #Add geom_() objects
  # individual means at baseline and followup for 2 groups
  geom_point(data = d %>% filter(x =="3"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  
  ## join means with line
  geom_line(data = d %>% filter(x =="3" | x =="4"), aes(x = xj, group = ID_Order), color = 'darkgray', alpha = 0.5) +
  
  geom_line(data = d %>% filter(x =="1" | x =="2"), aes(x = xj, group = ID_Order), color = 'red', alpha = 0.5) +
  
  #clouds for rainclouds!
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = -2.5), 
    side = "l", fill = 'darkgray', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "r", fill = 'darkgray', alpha = .6) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'red', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = .5), 
    side = "r", fill = 'red', alpha = .6) + 
  
  
  ##Group means #Add lines connecting the two means
  geom_line(data = summary_df[3:4,], aes(x = c((xx1-0.05),(xx2+0.05)), y = score_mean),  # nudged grey  means a bit wider so they don't overlap red
            color = 'darkgray', size = 1.5, alpha = 1) + 
  geom_line(data = summary_df[1:2,], aes(x = c(xx1,xx2), y = score_mean),
            color = 'red', size = 1.5, alpha = 1) +
  
  
  # group means with shape = 16, filled circle & SEM error bars
  # baseline red
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1], ymin = score_mean[1]-se[1], ymax = score_mean[1]+se[1]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  
  # #baseline grey
  geom_errorbar(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3], ymin = score_mean[3]-se[3], ymax = score_mean[3]+se[3]),
                position = position_nudge(-0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3]),
             position = position_nudge(-0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  #position = position_nudge(x = 0), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  #followup red
  
  geom_errorbar(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2], ymin = score_mean[2]-se[2], ymax = score_mean[2]+se[2]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  # follow-up grey
  geom_errorbar(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4], ymin = score_mean[4]-se[4], ymax = score_mean[4]+se[4]),
                position = position_nudge(0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4]),
             position = position_nudge(x = 0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  # formatting 
  
  scale_x_continuous(breaks=c(1,2), labels=c("Baseline", "Follow Up"))+
  
  ylab("Mean Connectivity") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max))

f3

figfname<-paste(FigOutDir, '/GrpTimeInt_NewMood.png', sep="")
ggsave(figfname, width = w, height = h)

##################################
# Plot 4
HR1wConv <- subset(TenPercCons, Group == -1 & Time == 1 & `Converter` == 1, select=c(ID_Order, Average))
HR2wConv <- subset(TenPercCons, Group == -1 & Time == -1 & `Converter` == 1, select=c(ID_Order, Average))
HR1woutConv <- subset(TenPercCons, Group == -1 & Time == 1 & `Converter` == 3, select=c(ID_Order, Average))
HR2woutConv <- subset(TenPercCons, Group == -1 & Time == -1 & `Converter` == 3, select=c(ID_Order, Average))

#average for each group
HR1wConvdat <- HR1wConv$Average
HR2wConvdat <- HR2wConv$Average

HR1woutConvdat <- HR1woutConv$Average
HR2woutConvdat <- HR2woutConv$Average

#samplesize for each group
nwConv <- length(HR1wConvdat)
nwoutConv <- length(HR1woutConvdat)

nwConvscans <- nwConv*2
nwoutConvscans <- nwoutConv*2
totalscans <- nwConvscans+nwoutConvscans

x1 <- as.integer(totalscans)
x1[1:nwConvscans] <- rep(c(1,2), each=nwConv)
x1[(nwConvscans+1):totalscans] <- rep(c(3,4), each=nwoutConv)


wConvids <- rep(HR1wConv$ID_Order,2)
woutConvids <-rep(HR1woutConv$ID_Order,2)

d <- data.frame(y = c(HR1wConvdat, HR2wConvdat, HR1woutConvdat, HR2woutConvdat), x = x1, ID_Order = as.factor(c(wConvids, woutConvids)))

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,1,2)

score_mean_1 <- mean(HR1wConvdat)
score_mean_2 <- mean(HR2wConvdat)
score_mean_3 <- mean(HR1woutConvdat)
score_mean_4 <- mean(HR2woutConvdat)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)


## change error bars from 95CI to SE)
mean_se_1 <- mean_se(HR1wConvdat, mult =1)
mean_se_2 <- mean_se(HR2wConvdat, mult =1)
mean_se_3 <- mean_se(HR1woutConvdat, mult =1)
mean_se_4 <- mean_se(HR2woutConvdat, mult =1)

score_se_1 <- score_mean_1-mean_se_1$ymin
score_se_2 <- score_mean_2-mean_se_2$ymin
score_se_3 <- score_mean_3-mean_se_3$ymin
score_se_4 <- score_mean_4-mean_se_4$ymin
se <- c(score_se_1, score_se_2, score_se_3, score_se_4)


summary_df <- data.frame(group, score_mean, se)


#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above



y_lim_min <- min(d$y)
y_lim_max <- max(d$y)


#Start plotting

set.seed(321)

x1fk <- numeric((nhr)*2)

x1fk[1:nwConvscans] <- rep(c(1,2), each=nwConv)
x1fk[(nwConvscans+1):totalscans] <- rep(c(1,2), each=nwoutConv)

xj <- jitter(x1fk, amount = .09)

d$xj <- xj

#shifted means to edges to not obscure things
xx1 = 0.9
xx2 = 2.1
#########################

f4 <- ggplot(data = d, aes(y = y)) +
  #Add geom_() objects
  # individual means at baseline and followup for 2 groups
  geom_point(data = d %>% filter(x =="3"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj), color = 'darkgray', size = 1.5,alpha = .5) +
  
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'red', size = 1.5, alpha = 1) +
  
  ## join means with line
  geom_line(data = d %>% filter(x =="3" | x =="4"), aes(x = xj, group = ID_Order), color = 'darkgray', alpha = 0.5) +
  
  geom_line(data = d %>% filter(x =="1" | x =="2"), aes(x = xj, group = ID_Order), color = 'red', alpha = 0.5) +
  
  #clouds for rainclouds!
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = -2.5), 
    side = "l", fill = 'darkgray', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "r", fill = 'darkgray', alpha = .6) +
  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'red', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = .5), 
    side = "r", fill = 'red', alpha = .6) + 
  
  
  ##Group means #Add lines connecting the two means
  geom_line(data = summary_df[3:4,], aes(x = c((xx1-0.05),(xx2+0.05)), y = score_mean),  # nudged grey  means a bit wider so they don't overlap red
            color = 'darkgray', size = 1.5, alpha = 1) + 
  geom_line(data = summary_df[1:2,], aes(x = c(xx1,xx2), y = score_mean),
            color = 'red', size = 1.5, alpha = 1) +
  
  
  # group means with shape = 16, filled circle & SEM error bars
  # baseline red
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1], ymin = score_mean[1]-se[1], ymax = score_mean[1]+se[1]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  
  # #baseline grey
  geom_errorbar(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3], ymin = score_mean[3]-se[3], ymax = score_mean[3]+se[3]),
                position = position_nudge(-0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="3"), aes(x = xx1, y = score_mean[3]),
             position = position_nudge(-0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  #position = position_nudge(x = 0), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="1"), aes(x = xx1, y = score_mean[1]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  #followup red
  
  geom_errorbar(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2], ymin = score_mean[2]-se[2], ymax = score_mean[2]+se[2]),
                position = position_nudge(0),
                color = "red", width = 0.05, size = 0.4, alpha = 0.8) +
  
  # follow-up grey
  geom_errorbar(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4], ymin = score_mean[4]-se[4], ymax = score_mean[4]+se[4]),
                position = position_nudge(0.05),
                color = "darkgray", width = 0.05, size = 0.4, alpha = 1) +
  
  geom_point(data = d %>% filter(x=="4"), aes(x = xx2, y = score_mean[4]),
             position = position_nudge(x = 0.05), shape = 21, color = "black", fill = "darkgray",  size = 3, alpha = 0.8) +
  
  geom_point(data = d %>% filter(x=="2"), aes(x = xx2, y = score_mean[2]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "red",  size = 3, alpha = 0.8) +
  
  # formatting 
  
  scale_x_continuous(breaks=c(1,2), labels=c("Baseline", "Follow Up"))+
  
  ylab("Mean Connectivity") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max))

f4 #4. HR converted to BD diagnosis (manic+dep episode)

figfname<-paste(FigOutDir, '/GrpTimeInt_Converter.png', sep="")
ggsave(figfname, width = w, height = h)

