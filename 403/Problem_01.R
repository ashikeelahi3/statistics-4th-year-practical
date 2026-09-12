###1
time=c(75,166,110,144,26,118,131,14,49,153,66,36,32,11,11,15,75,85,100)
length(time)
#sort(time)
status=c(rep(1,17),rep(0,2))
dataset=data.frame(time,status)
dataset

#(a)
library(survival)
km.fit=survfit(Surv(time,status==1)~1,data=dataset)
summary(km.fit)

plot(km.fit,col='red',lwd=2,main='Plot of estimated survival curve with 95%
     Confidence belts',col.main='blue',xlab='Survival Time',col.lab='blue',
     xlim=c(0,180),ylab='Survival Probability',col.lab='blue',conf.int=0.95)

#abline(v=45,col=4,lwd=2)
abline(h=0.75,v=26,col=4,lwd=2)

time=km.fit$time[-17]
s=km.fit$surv[-17]
km.fit$time
time
s
x=log(time)
y=log(-log(s))
plot(x,y,xlim=c(1.5,5),ylim=c(-3,2),main="Weibull plot of remission data",
     xlab="log(t)",ylab="log(-log(S(t)))")
abline(lsfit(x,y),col=4,lwd=2)
abline(h=0,col=3,lwd=2)

abline(h=c(0,-3.06),col=c(3,2),lwd=2)
abline(v=4.48,col=4,lwd=2)

