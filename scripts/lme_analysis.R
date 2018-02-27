lme_analysis <- function(tablefname, OUTDIR) {

library("nlme")
library("dplyr")  
  
VARS=cbind("Pavg","withinmodsumall")
baseCOVARS=cbind("ID","Group","Time","Age","AgeSQ")
addCOVARS=cbind("addcovs")

allCOVARS=cbind(baseCOVARS,addCOVARS)
#VARS=cbind("connectivityweights")
  
data<-read.table(tablefname,header = T, sep = ",")
data$AgeSQ<-data$Age*data$Age

dataCOVARS=select(data,allCOVARS)
  
for(i in 1:length(VARS))
{
  
VAR=VARS[i]
VARmatch<-which(names(data)==VAR)

newdat<-cbind(data[,VARmatch],dataCOVARS)
names(newdat)[1]<-"y"

#obvs<-data[,VARmatch]

lmerInt<-lme(y ~ Group*Time + Age + addcovs, data=newdat, random = ~ Time|ID)

#conf<-confint(lmerInt, method="boot", nsim=5000)

out<-summary(lmerInt)

fixedcoeffs<-as.data.frame(out$coefficients$fixed)

npreds<-length(row.names(fixedcoeffs))
outstats<-matrix(, nrow = as.numeric(npreds), ncol = 5)

for(j in 1:npreds)
{
outstats[j,1]<-row.names(fixedcoeffs)[j]
}

anovaout<-anova(lmerInt)

outstats[,2:5]<-c(out$coefficients$fixed, anovaout[,2],anovaout[,3],anovaout[,4])
#outstats[,6:7]<-conf[3:length(conf[,1]),1:2]

outstats<-as.data.frame(outstats)
names(outstats)<-c("Predictor","Estimate","df","F","p")

fnameremext<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tablefname))
fname<-paste(fnameremext,"_","LMEresults","_",VAR,".csv",sep="")
outtablefname<-paste(OUTDIR, fname, sep="/")
write.table(outstats, outtablefname, sep = ",", row.names = FALSE)

lmerInt<-lme(y ~ Group*Time*Age, data=newdat, random = ~ Time|ID)

#conf<-confint(lmerInt, method="boot", nsim=5000)

out<-summary(lmerInt)

fixedcoeffs<-as.data.frame(out$coefficients$fixed)

npreds<-length(row.names(fixedcoeffs))
outstats<-matrix(, nrow = as.numeric(npreds), ncol = 5)

for(j in 1:npreds)
{
  outstats[j,1]<-row.names(fixedcoeffs)[j]
}

anovaout<-anova(lmerInt)

outstats[,2:5]<-c(out$coefficients$fixed, anovaout[,2],anovaout[,3],anovaout[,4])
#outstats[,6:7]<-conf[3:length(conf[,1]),1:2]

outstats<-as.data.frame(outstats)
names(outstats)<-c("Predictor","Estimate","df","F","p")

fnameremext<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tablefname))
fname<-paste(fnameremext,"_","LMEresults","_wAgeInt_",VAR,".csv",sep="")
outtablefname<-paste(OUTDIR, fname, sep="/")
write.table(outstats, outtablefname, sep = ",", row.names = FALSE)

}

}