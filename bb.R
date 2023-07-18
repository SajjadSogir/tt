library(survival)
library(KMsurv)
           #=====1(b)======#
group<-c(0,0,0,0,0,0,1,1,1,1,1,1)
time<-c(4.1,6.8,9,9,11.3,16.2,8.6,9,10.1,12.1,18.7,23.1)
censoring<-c(1,0,1,1,0,1,1,1,0,0,1,0)
d<-data.frame(group,time,censoring)
survdiff(Surv(time,censoring)~group,data=d)

  #=========1(c)=============#
time1<-c(3,1,5,7,4,9,8)
censoring1<-c(0,1,0,1,1,0,1)
my.s<-Surv(time1,censoring1,type='right')
my.fit<-summary(survfit(my.s~1,type="kaplan-meier",conf.type="log-log"))
summary(survfit(my.s~1,type="kaplan-meier",conf.type="log-log"))
summary(survfit(my.s~1,type="kaplan-meier"))
H.hat<-log(my.fit$surv)
plot(survfit(Surv(time1,censoring1,type='right')~1),xlab="Days")
#=========================2====================
d2<-read.table("G:/MASTERS/STAT-508/data/hw1And2.txt",header=FALSE)
head(d2)
colnames(d2)<-c("obs_n","Treatment","Z2","Z3","Z4","Z5","Z6","Z7","Z8","Z9","Z10","Z11",
                 "T1","1","T2","2","Ts","censoring")
str(d2)
head(d2)
    #=====2(a)==========#
survdiff(Surv(d2$Ts,d2$censoring)~d2$Treatment,data=d2)
 #===============2(b)================#
d2$Z4<-ifelse(d2$Z4<=29,1,ifelse(d2$Z4<=50,2,3))
survdiff(Surv(d2$Ts,d2$censoring)~d2$Z4,data=d2)
#===================2(c)=====================#
cm<-coxph(Surv(d2$Ts,d2$censoring)~d2$Treatment,data=d2)
summary(cm)

#===================2(d)=====================#
cmd<-coxph(Surv(d2$Ts,d2$censoring)~d2$Treatment+strata(d2$Z4),data=d2)
summary(cmd)

#========================3Q===========================#
Dogid<-1:10
T<-c(30,30,40,45,20,45,60,45,67,30)
S<-c(0,1,1,0,1,0,1,0,1,0)
M<-c("RX","US","RX","RX","US","RX","RX","US","RX","US")
F<-c(0,1,0,1,0,0,1,0,1,0)
dd<-data.frame(Dogid,T,S,M,F)





























#colnames(d2)<-c("obs.n","Treatment","Gender","Race","P_tsb",
                 "Head_Burn","Buttok_Burn","Trunk_Burn",
                  "Uleg_Burn","lleg_Burn","Respirayory_Burn",
                  "Type_Burn","Study_time","Excision")