##3
#parallel
x=c(rep(1,24),rep(0,24),rep(0,24),rep(1,24),rep(log(0.25),8),rep(log(0.5),8),
    rep(log(1),8),rep(log(0.25),8),rep(log(0.5),8),rep(log(1),8) )
x=matrix(x,48,3)
x
ys1=c(6,6.8,6.2,6.6,6.4,6,6.9,6.3)
ys2=c(9.4,8.8,9.4,9.6,9.8,9.2,10.8,10.6)
ys3=c(12.8,13.6,13.4,13.8,12.8,14,13.2,12.8)
yt1=c(4.9,4.8,4.9,4.8,5.3,5.1,4.9,4.7)
yt2=c(8.2,8.1,8.1,8.2,7.6,8.3,8.2,8.1)
yt3=c(11,11.5,11.4,11.8,11.8,11.4,11.7,11.4)
y=c(ys1,ys2,ys3,yt1,yt2,yt3)
y
b=solve(t(x)%*%x)%*%t(x)%*%y
b
b[1]
M=(b[2]-b[1])/b[3] ##Estimate of log-potency
M
yhat=x%*%b
yhat
s=sum((y-yhat)^2)/45
s
r1=exp(M) ##Estimate of potency
r1
v=s*solve(t(x)%*%x)
v
h=(1/b[3])*c(-1,1,-M)
h
var.M=t(h)%*%v%*%h
var.M
s.e.M=sqrt(var.M)
s.e.M
s.e.r1=sqrt(r1^2*var.M)
s.e.r1
LCL=exp(M-1.96*s.e.M)
LCL
UCL=exp(M+1.96*s.e.M)
UCL



#Slope-ratio
x=c(rep(1,48),rep(0.25,8),rep(0.5,8),rep(1,8),rep(0,24),rep(0,24),
    rep(0.25,8),rep(0.5,8),rep(1,8) )
x=matrix(x,48,3)
x
y
b=solve(t(x)%*%x)%*%t(x)%*%y
b
r2=b[3]/b[2]
r2

yhat=x%*%b
yhat
s=sum((y-yhat)^2)/45
s
v=s*solve(t(x)%*%x)
v
h=(1/b[2])*c(0,-r2,1)
h
var.r2=t(h)%*%v%*%h
var.r2
s.e.r2=sqrt(var.r2)
s.e.r2

LCL=r2-1.96*s.e.r2
LCL
UCL=r2+1.96*s.e.r2
UCL
