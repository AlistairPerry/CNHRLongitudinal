### Rain cloud plots across 2 timepoints for 2 groups - Main effect of time
# suplots for 1. positive and 2. negative connectivity (increases with time, decreases with time)
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

# width and height variables for saved plots 
w = 6
h = 4

# Set dirs
setwd(">>> enter in base directory <<<")
TenPercCons <- read_excel(">>> select data source file and adjust range as needed<<< .xlsx", sheet = 1, range = cell_rows(2:368))
FigOutDir <- (">>> enter in figure output directory<<<")

##############################
## plot 1:  Positive Connectivity 

#split into group and time
CN1 <- subset(Tweights, Group == 1 & Time == 1)
CN2 <- subset(Tweights, Group == 1 & Time == -1)

HR1 <- subset(Tweights, Group == -1 & Time == 1)
HR2 <- subset(Tweights, Group == -1 & Time == -1)

CN1pos <- CN1$AveragePositive
CN2pos <- CN2$AveragePositive
HR1pos <- HR1$AveragePositive
HR2pos <- HR2$AveragePositive


ncons <- length(CN1pos)
nhr <- length(HR1pos)

nconsscans <- ncons*2
nhrscans <- nhr*2
totalscans <- nconsscans+nhrscans

x1 <- rep(c(1,2), each=ncons)
x2 <- rep(c(3,4), each=nhr)

cnids <- rep(1:ncons,2)
hrids<-rep(87:183,2)

d <- data.frame(y = c(CN1pos, CN2pos, HR1pos, HR2pos), x = c(x1, x2), id = as.factor(c(cnids, hrids)))

y_lim_min <- min(d$y)
y_lim_max <- max(d$y)

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,3,4)

score_mean_1 <- mean(CN1pos)
score_mean_2 <- mean(CN2pos)
score_mean_3 <- mean(HR1pos)
score_mean_4 <- mean(HR2pos)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)

summary_df <- data.frame(group, score_mean)

#### get confidence intervals
# score_ci_1 <- CI(d$y[1:86], ci = 0.95)
# score_ci_2 <- CI(d$y[87:100], ci = 0.95)
score_ci_1 <- CI(CN1pos, ci = 0.95) #CN1
score_ci_2 <- CI(CN2pos, ci = 0.95) #CN1
score_ci_3 <- CI(HR1pos, ci = 0.95) #CN1
score_ci_4 <- CI(HR2pos, ci = 0.95) #CN1

# CI's around mean
ci <- c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]), (score_ci_3[1] - score_ci_3[3]), (score_ci_4[1] - score_ci_4[3]))

#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above
x_tick_means_z <- c(2.87, 4.13) #just add 2 for each tick

#Start plotting

set.seed(321)

xj <- numeric((ncons+nhr)*2)

xj[1:nconsscans] <- jitter(d$x[1:nconsscans], amount = .09)

xj[(1+nconsscans):totalscans] <- jitter(d$x[(1+nconsscans):totalscans], amount = .09)


d$xj <- xj

f1 <- ggplot(data = d, aes(y = y)) +
  #Add geom_() objects
  geom_point(data = d %>% filter(x =="1"), aes(x = xj[1:ncons]), color = 'dodgerblue', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj[1:ncons]), color = 'dodgerblue', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="3"), aes(x = xj[1:nhr]), color = 'darkorange', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj[1:nhr]), color = 'darkorange', size = 1.5,
             alpha = .6) +
  
  geom_line(aes(x = xj, group = id), color = 'darkgrey', alpha = 0.5) + 
  
  
  # geom_half_boxplot(
  #   data = d %>% filter(x=="1"), aes(x=x, y = y), 
  #   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
  #   fill = 'dodgerblue', alpha = .2) +
  # geom_half_boxplot(
  #   data = d %>% filter(x=="2"), aes(x=x, y = y),
  #   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
  #   fill = 'dodgerblue', alpha = .2) +
  # geom_half_boxplot(
#   data = d %>% filter(x=="3"), aes(x=x, y = y), 
#   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
#   fill = 'darkorange', alpha = .2) +
# geom_half_boxplot(
#   data = d %>% filter(x=="4"), aes(x=x, y = y), 
#   fill = 'darkorange', alpha = .2) + 

## swap boxplot for mean+error bars 95%CI


#Add lines connecting the two means
geom_line(data = summary_df[1:2,], aes(x = c(1,2), y = score_mean[1:2]),
          color = 'dodgerblue', size = 1.5, alpha = 1) +    #changed from 'darkgrey' ##### MC edit
  
  geom_line(data = summary_df[3:4,], aes(x = c(3,4), y = score_mean),
            color = 'darkorange', size = 1.5, alpha = 1)  +  #changed from 'darkgrey' ##### MC edit
  
  
  # means with shape = 16, filled circle
  #CN1
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[1], ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(0),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[1]),
             position = position_nudge(x = 0), shape = 21, color = "black", fill = "dodgerblue",  size = 3, alpha = 0.8) +
  #CN2  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[2], ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]),
                position = position_nudge(1),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[2]),
             position = position_nudge(x = 1), shape = 21, color = "black", fill = "dodgerblue",  size = 3, alpha = 0.8) +
  #HR1  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[3], ymin = score_mean[3]-ci[3], ymax = score_mean[3]+ci[3]),
                position = position_nudge(2),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[3]),
             position = position_nudge(x = 2), shape = 21, color = "black", fill = "darkorange", size = 3, alpha = 0.8) +
  #HR2  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[4], ymin = score_mean[4]-ci[4], ymax = score_mean[4]+ci[4]),
                position = position_nudge(3),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[4]),
             position = position_nudge(x = 3),  shape = 21, color = "black", fill = "darkorange",  size = 3, alpha = 0.8) +
  
  
  ##  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = 1.85), 
    side = "l", fill = 'darkorange', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = .85), 
    side = "l", fill = 'darkorange', alpha = .6) +
  
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Baseline", "Follow Up","Baseline", "Follow Up"),
                     limits=c(0, 5))+
  ylab("Mean Positive Connectivity") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max)) 

f1

figfname<-paste(FigOutDir, '/Time_Main_t3.0_POSITIVE.tiff', sep="")
ggsave(figfname, width = w, height = h) #random sizes I ended up exported manually so sticking with it for consistency


##############################
## plot 2:  Negative Connectivity 
#CN1 <- subset(Tweights, Group == 1 & Time == 1, select=c(ID, Controllability))

#split into group and time
CN1 <- subset(Tweights, Group == 1 & Time == 1)
CN2 <- subset(Tweights, Group == 1 & Time == -1)

HR1 <- subset(Tweights, Group == -1 & Time == 1)
HR2 <- subset(Tweights, Group == -1 & Time == -1)

CN1neg<- CN1$AverageNegative
CN2neg<- CN2$AverageNegative
HR1neg<- HR1$AverageNegative
HR2neg<- HR2$AverageNegative


ncons <- length(CN1neg)
nhr <- length(HR1neg)

nconsscans <- ncons*2
nhrscans <- nhr*2
totalscans <- nconsscans+nhrscans

x1 <- rep(c(1,2), each=ncons)
x2 <- rep(c(3,4), each=nhr)

cnids <- rep(1:ncons,2)
hrids<-rep(87:183,2)

d <- data.frame(y = c(CN1neg, CN2neg, HR1neg, HR2neg), x = c(x1, x2), id = as.factor(c(cnids, hrids)))

y_lim_min <- min(d$y)
y_lim_max <- max(d$y)

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,3,4)

score_mean_1 <- mean(CN1neg)
score_mean_2 <- mean(CN2neg)
score_mean_3 <- mean(HR1neg)
score_mean_4 <- mean(HR2neg)

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4)

summary_df <- data.frame(group, score_mean)

#### get confidence intervals
# score_ci_1 <- CI(d$y[1:86], ci = 0.95)
# score_ci_2 <- CI(d$y[87:100], ci = 0.95)
score_ci_1 <- CI(CN1neg, ci = 0.95) #CN1
score_ci_2 <- CI(CN2neg, ci = 0.95) #CN1
score_ci_3 <- CI(HR1neg, ci = 0.95) #CN1
score_ci_4 <- CI(HR2neg, ci = 0.95) #CN1

# CI's around mean
ci <- c((score_ci_1[1] - score_ci_1[3]), (score_ci_2[1] - score_ci_2[3]), (score_ci_3[1] - score_ci_3[3]), (score_ci_4[1] - score_ci_4[3]))

#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above
x_tick_means_z <- c(2.87, 4.13) #just add 2 for each tick

#Start plotting

set.seed(321)

xj <- numeric((ncons+nhr)*2)

xj[1:nconsscans] <- jitter(d$x[1:nconsscans], amount = .09)

xj[(1+nconsscans):totalscans] <- jitter(d$x[(1+nconsscans):totalscans], amount = .09)


d$xj <- xj

f2 <- ggplot(data = d, aes(y = y)) +
  #Add geom_() objects
  geom_point(data = d %>% filter(x =="1"), aes(x = xj[1:ncons]), color = 'dodgerblue', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj[1:ncons]), color = 'dodgerblue', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="3"), aes(x = xj[1:nhr]), color = 'darkorange', size = 1.5,
             alpha = .6) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj[1:nhr]), color = 'darkorange', size = 1.5,
             alpha = .6) +
  
  geom_line(aes(x = xj, group = id), color = 'darkgrey', alpha = 0.5) + 
  
  
  # geom_half_boxplot(
  #   data = d %>% filter(x=="1"), aes(x=x, y = y), 
  #   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
  #   fill = 'dodgerblue', alpha = .2) +
  # geom_half_boxplot(
  #   data = d %>% filter(x=="2"), aes(x=x, y = y),
  #   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
  #   fill = 'dodgerblue', alpha = .2) +
  # geom_half_boxplot(
  #   data = d %>% filter(x=="3"), aes(x=x, y = y), 
  #   side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
  #   fill = 'darkorange', alpha = .2) +
  # geom_half_boxplot(
  #   data = d %>% filter(x=="4"), aes(x=x, y = y), 
  #   fill = 'darkorange', alpha = .2) + 

## swap boxplot for mean+error bars 95%CI


#Add lines connecting the two means
geom_line(data = summary_df[1:2,], aes(x = c(1,2), y = score_mean[1:2]),
          color = 'dodgerblue', size = 1.5, alpha = 1) +    #changed from 'darkgrey' ##### MC edit
  
  geom_line(data = summary_df[3:4,], aes(x = c(3,4), y = score_mean),
            color = 'darkorange', size = 1.5, alpha = 1)  +  #changed from 'darkgrey' ##### MC edit


# means with shape = 16, filled circle
#CN1
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[1], ymin = score_mean[1]-ci[1], ymax = score_mean[1]+ci[1]),
                position = position_nudge(0),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[1]),
            position = position_nudge(x = 0), shape = 21, color = "black", fill = "dodgerblue",  size = 3, alpha = 0.8) +
#CN2  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[2], ymin = score_mean[2]-ci[2], ymax = score_mean[2]+ci[2]),
                position = position_nudge(1),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[2]),
             position = position_nudge(x = 1), shape = 21, color = "black", fill = "dodgerblue",  size = 3, alpha = 0.8) +
#HR1  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[3], ymin = score_mean[3]-ci[3], ymax = score_mean[3]+ci[3]),
                position = position_nudge(2),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[3]),
             position = position_nudge(x = 2), shape = 21, color = "black", fill = "darkorange", size = 3, alpha = 0.8) +
#HR2  
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[4], ymin = score_mean[4]-ci[4], ymax = score_mean[4]+ci[4]),
                position = position_nudge(3),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[4]),
             position = position_nudge(x = 3),  shape = 21, color = "black", fill = "darkorange",  size = 3, alpha = 0.8) +

  
##  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.5), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = -1.5), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = 1.85), 
    side = "l", fill = 'darkorange', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = .85), 
    side = "l", fill = 'darkorange', alpha = .6) +
  
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Baseline", "Follow Up","Baseline", "Follow Up"),
                     limits=c(0, 5))+
  ylab("Average Negative Weight") +
  
  theme_classic()+
  coord_cartesian(ylim=c(y_lim_min, y_lim_max)) 
  


f2

figfname<-paste(FigOutDir, '/Time_Main_t3.0_NEGATIVE.tiff', sep="")
ggsave(figfname, width = w, height = h) 

# all done! :-) 