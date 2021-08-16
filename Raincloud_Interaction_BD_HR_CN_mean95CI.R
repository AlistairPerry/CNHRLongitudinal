### Raincloud plot of longitudinal paired data, for two groups x 2 timepoints, with a comparision to a third group with only 1 timepoint.
## COMPARE OUR HR GROUP WITH PREVIOUS SAMPLE OF PEOPLE WITH BD DIAGNOSIS
### adding a single timepoint of BD participants to compare on the existing TimexGroup interaction plot smaple of n=52 BD patients

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


###
# Set fig output dir
setwd(">>> enter in base directory <<<")
TenPercCons <- read_excel(">>> select data source file and adjust range as needed<<< .xlsx", sheet = 1, range = cell_rows(2:368))
FigOutDir <- (">>> enter in figure output directory<<<")

# control gorup time 1 and 2
CN1 <- subset(TenPercCons, Group == 1 & Time == 1, select=c(ID_Order, Average))
CN2 <- subset(TenPercCons, Group == 1 & Time == -1, select=c(ID_Order, Average))
# high-risk group time 1 and 2
HR1 <- subset(TenPercCons, Group == -1 & Time == 1)
HR2 <- subset(TenPercCons, Group == -1 & Time == -1)

# BD baseline only (previous dataset)
BD1 <- subset(TenPercCons, Group == 4 & Time == 1)

CN1dat <- CN1$Average
CN2dat <- CN2$Average
HR1dat <- HR1$Average
HR2dat <- HR2$Average

BD1dat <- BD1$Average

ncons <- length(CN1dat)
nhr <- length(HR1dat)
nbd <- length(BD1dat)

# for the 2 timepoints
nconsscans <- ncons*2
nhrscans <- nhr*2
totalscans <- nconsscans+nhrscans

# x marks to centre the clusters of data around 
x1 <- rep(c(1,2), each=ncons)
x2 <- rep(c(3,4), each=nhr)
x3 <- rep(c(5.4), each=(nbd))  # have BD's plots at the far right side after HR plots

# within this dataframe index the 3 groups (actually 5 lots with HR and Cn having 2 times, and BD only 1)
cnids <- rep(1:ncons,2)
hrids<-rep((ncons+1):(ncons+nhr),2)
bdids <-(c(ncons+nhr+1):(ncons+nhr+nbd))

d <- data.frame(y = c(CN1dat, CN2dat, HR1dat, HR2dat, BD1dat), x = c(x1, x2, x3), ID_Order = as.factor(c(cnids, hrids, bdids)))

## d2 <- data.frame(y = c(BD1dat), x = c(x3), ID_Order = as.factor(c(cnids, hrids))) # separate data structure?
y_lim_min <- min(d$y)
y_lim_max <- max(d$y)

#Descriptive stats for calculating and connecting group means

#First do the descriptives

group <- c(1,2,1,2,3) # group by the time?

#### get confidence intervals
# CI gives: upper, mean, lower - to get interval take upper-mean
score_ci_1 <- CI(CN1dat, ci = 0.95) #CN1
score_ci_2 <- CI(CN2dat, ci = 0.95) #CN2
score_ci_3 <- CI(HR1dat, ci = 0.95) #HR1
score_ci_4 <- CI(HR2dat, ci = 0.95) #HR2
score_ci_5 <- CI(BD1dat, ci = 0.95) #Bipolar group baseline only (previous data set)


score_mean_1 <- score_ci_1[2]
score_mean_2 <- score_ci_2[2]
score_mean_3 <- score_ci_3[2]
score_mean_4 <- score_ci_4[2]
score_mean_5 <- score_ci_5[2]

score_mean <- c(score_mean_1, score_mean_2, score_mean_3, score_mean_4, score_mean_5)


# CI's around mean [1] = upper limit; [2] = mean
ci <- c((score_ci_1[1] - score_ci_1[2]), (score_ci_2[1] - score_ci_2[2]), (score_ci_3[1] - score_ci_3[2]), (score_ci_4[1] - score_ci_4[2]), (score_ci_5[1] - score_ci_5[2]))

summary_df <- data.frame(group, score_mean, ci)


#First we must again define the x-coordinates of the means.
# x_tick_means_x <- c(.87, 2.13) #same as above

#scale limits
y_lim_min <- min(d$y)
y_lim_max <- max(d$y)


#First we must again define the x-coordinates of the means.
x_tick_means_x <- c(.87, 2.13) #same as above
x_tick_means_z <- c(2.87, 4.13) #just add 2 for each tick
### ^^^^ not sure if these are used?

#Start plotting

set.seed(321)

# jittering for individual participant means (rain-drops in the rain-cloud plot)
xj <- numeric((ncons+nhr)*2+nbd) # preallocate right number of values, and here I added points for BD group 
xj[1:nconsscans] <- jitter(d$x[1:nconsscans], amount = .09)
xj[(1+nconsscans):totalscans] <- jitter(d$x[(1+nconsscans):totalscans], amount = .09)
xj[(1+totalscans):(totalscans+nbd)] <- jitter(d$x[(1+totalscans):(totalscans+nbd)], amount = .09)


d$xj <- xj

d$ID_Order[1]


  ############################# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##################################

  #Add geom_() objects
  f1 <- ggplot(data = d, aes(y = y)) +
  #Add geom_() objects
  # 1, 2 = CN 1 and 2
  geom_point(data = d %>% filter(x =="1"), aes(x = xj), color = 'dodgerblue', size = 1.5, alpha = .6) +
  geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'dodgerblue', size = 1.5, alpha = .6) +
  # 3, 4 = HR 1 and 2
  geom_point(data = d %>% filter(x =="3"), aes(x = xj), color = 'darkorange', size = 1.5, alpha = .6) +
  geom_point(data = d %>% filter(x =="4"), aes(x = xj), color = 'darkorange', size = 1.5, alpha = .6) +
  # 5 = BD baseline only
  geom_point(data = d %>% filter(x =="5.4"), aes(x = xj), color = 'purple4', size = 1.5, alpha = 0.4) +
  
  #add highlight colour of data points that are in BaselineMood group
 # geom_point(data = d %>% filter(orange =="1"), aes(x = xj), color = 'orange', size = 5, alpha = .2) +
  #geom_point(data = d %>% filter(x =="2"), aes(x = xj), color = 'orange', size = 1.5, alpha = .7) +
  
  geom_line(aes(x = xj, group = ID_Order), color = 'darkgrey', alpha = 0.5) +  
  

  ########## 
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
  
  #BD group baciline only
  geom_errorbar(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[5], ymin = score_mean[5]-ci[5], ymax = score_mean[5]+ci[5]),
                position = position_nudge(4.4),
                color = "black", width = 0.05, size = 0.4, alpha = 0.8) +
  geom_point(data = d %>% filter(x=="1"), aes(x = x, y = score_mean[5]),
             position = position_nudge(x = 4.4),  shape = 21, color = "black", fill = "purple4",  size = 3, alpha = 0.7) +
  
  
  
  ##  
  geom_half_violin(
    data = d %>% filter(x=="1"),aes(x = x, y = y), position = position_nudge(x = -.3), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="2"),aes(x = x, y = y), position = position_nudge(x = -1.3), 
    side = "l", fill = 'dodgerblue', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="3"),aes(x = x, y = y), position = position_nudge(x = 1.3), 
    side = "r", fill = 'darkorange', alpha = .6) + 
  
  geom_half_violin(
    data = d %>% filter(x=="4"),aes(x = x, y = y), position = position_nudge(x = .3), 
    side = "r", fill = 'darkorange', alpha = .6) +
  
  geom_half_violin(
    data = d %>% filter(x=="5.4"),aes(x = x, y = y), position = position_nudge(x = .2), 
    side = "r", fill = 'purple4', alpha = .6) +
  
  # add below 5ith break, and another 'baseline' lable for BD
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Baseline", "Follow Up","Baseline", "Follow Up"),
                     limits=c(0, 6))+
  ylab("Average Connectivity") + theme_classic() + theme(axis.title.y = element_text(color = "black", size = 11, angle = 90, family = "serif", face = "bold"), axis.text.y = element_text(color = "black", size = 10, face = "bold"), axis.text.x = element_text(color = "black", size = 10, face = "bold")) +
  
  coord_cartesian(ylim=c(y_lim_min, y_lim_max)) 

f1


figfname<-paste(FigOutDir, '/GrpTimeInt_wBD.png', sep="")
ggsave(figfname, width = w, height = h)

# done :) 