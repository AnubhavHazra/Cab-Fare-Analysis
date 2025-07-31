rm(list=ls())
library(quantreg)
getwd()
setwd("C:/Users/Anubhav/OneDrive/Desktop/Cab FARE")
data=read.csv("DATASET_PROJECT.csv")
attach(data)
head(data)
y=fare



#boxplots to show outliers and homoscedasticity
par(mfrow=c(2,3))
distance_groups=cut(distance,10)
duration_groups=cut(duration,10)
tolltax_groups=cut(tolltax,10)
boxplot(y~distance_groups)
boxplot(y~duration_groups)
boxplot(y~extra,ylim=c(0,100))
boxplot(y~tolltax_groups)
boxplot(y~day,ylim=c(0,100))
boxplot(y~time,ylim=c(0,100))


#coefficient comparison over the deciles
t=seq(0.1,0.9,0.1)
intercept=dist=dur=ext=tolls=wkd=hour=c()

for(i in 1:9)
{
intercept[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[1]
dist[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[2]
dur[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[3]
ext[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[4]
tolls[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[5]
wkd[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[6]
hour[i]=rq(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time),tau=t[i])$coefficients[7]
}
model=lm(y~distance+duration+extra+tolltax+as.factor(weekday)+as.factor(time))
b=c()
for(i in 1:8)
{
b[i]=model$coefficients[i]
}
summary(model)
par(mfrow=c(2,4))
plot(t,intercept,type="l",xlab="decile")
lines(t,rep(b[1],9),type="l",col=2,lwd=3)
plot(t,dist,type="l",xlab="decile")
lines(t,rep(b[2],9),type="l",col=2,lwd=3)
plot(t,dur,type="l",xlab="decile",ylim=c(-0.001,0.0025))
lines(t,rep(b[3],9),type="l",col=2,lwd=3)
plot(t,ext,type="l",xlab="decile",ylim=c(0.7,1.6))
lines(t,rep(b[4],9),type="l",col=2,lwd=3)
plot(t,tolls,type="l",xlab="decile")
lines(t,rep(b[5],9),type="l",col=2,lwd=3)
plot(t,wkd,type="l",xlab="decile")
lines(t,rep(b[6],9),type="l",col=2,lwd=3)
plot(t,hour,type="l",xlab="decile")
lines(t,rep(b[7],9),type="l",col=2,lwd=3)

