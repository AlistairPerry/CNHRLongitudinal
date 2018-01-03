lme_analysis <- function(tablefname, OUTDIR) {

#VARS=cbind("EFF","CC","CPL","InterHemC")
VARS=cbind("connectivityweights")
  
data<-read.table(tablefname,header = T, sep = ",")  
  
library("lmerTest")

data$AgeSQ<-data$Age*data$Age

for(i in 1:length(VARS))
{
  
CVAR=VARS[i]

CVARmatch<-which(names(data)==CVAR)

obvs<-data[,CVARmatch]

lmerInt<-lmerTest::lmer(obvs ~ Group*Time + Age + (1 | ID), data=data)

conf<-confint(lmerInt, method="boot", nsim=5000)

out<-summary(lmerInt)

npreds<-length(row.names(out$coefficients))

outstats<-matrix(, nrow = as.numeric(npreds), ncol = 7)

for(j in 1:npreds)
{
outstats[j,1]<-row.names(out$coefficients)[j]
}  

outstats[,2:5]<-c(out$coefficients[,1], out$coefficients[,3],out$coefficients[,4],out$coefficients[,5])
outstats[,6:7]<-conf[3:length(conf[,1]),1:2]

outstats<-as.data.frame(outstats)
names(outstats)<-c("Predictor","Estimate","df","t","p","LCI","UCI")

fnameremext<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tablefname))
fname<-paste(fnameremext,"_","LMEresults","_",CVAR,".csv",sep="")
outtablefname<-paste(OUTDIR, fname, sep="/")
write.table(outstats, outtablefname, sep = ",", row.names = FALSE)

#Now 3-way Interaction with age

lmerAgeInt<-lmerTest::lmer(obvs ~ Group*Time + Age*Group*Time + (1 | ID), data=data)

conf<-confint(lmerAgeInt, method="boot", nsim=5000)

out<-summary(lmerAgeInt)

npreds<-length(row.names(out$coefficients))

outstats<-matrix(, nrow = as.numeric(npreds), ncol = 7)

for(j in 1:npreds)
{
  outstats[j,1]<-row.names(out$coefficients)[j]
}  

outstats[,2:5]<-c(out$coefficients[,1], out$coefficients[,3],out$coefficients[,4],out$coefficients[,5])
outstats[,6:7]<-conf[3:length(conf[,1]),1:2]

outstats<-as.data.frame(outstats)
names(outstats)<-c("Predictor","Estimate","df","t","p","LCI","UCI")

fnameremext<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tablefname))
fname<-paste(fnameremext,"_","LMEresultsINTAGE","_",CVAR,".csv",sep="")
outtablefname<-paste(OUTDIR, fname, sep="/")
write.table(outstats, outtablefname, sep = ",", row.names = FALSE)

}

}