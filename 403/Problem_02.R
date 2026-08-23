###2
#MLEs of Weibull parameters under Type-I censoring

# Twenty patients were given a treatment and it was planned to terminate the treatment on 180 weeks. The survival times in weeks of 16 patients by 180 weeks are as follows. 
# 70, 160, 105, 140, 20, 113, 121, 10, 44, 150, 60, 30, 30, 11, 11, 15
# It is assumed that the above data follows a two parameter Weibull distribution.

# (i) Estimate the parameter using ML method
# (ii) Calculate the standard error of the estimates. Construct the 95% confidence intervals of the parameters.
# (iii) Estimate the 75th percentile and also estimate the survival probability after 200 weeks.
# (iv) Comment on your findings. 

x=c(70,160,105,140,20,113,121,10,44,150,60,30,30,11,11,15)
length(x)
quantile(x,.632)
n=20;t0=180;r=16

library(stats4)   #loading package stats4 for mle()

logL=function(a,b){
  term1=-r*log(b)
  term2=b*r*log(a)
  term3=-(b-1)*sum(log(x))
  term4=sum((x/a)^b)
  term5=(n-r)*((t0/a)^b)
  term1+term2+term3+term4+term5  #-log-likelihood function
}

coefs=coef(mle(minuslogl = logL,start = list(a=quantile(x,.632),b=1.5)))
coefs

var=vcov(mle(minuslogl = logL,start = list(a=quantile(x,.632),b=1.5)))
var

diag(var)
sqrt(diag(var))

#95% confidence interval for scale parameter
LCL=coefs[1]-1.96*sqrt(diag(var)[1])
LCL
UCL=coefs[1]+1.96*sqrt(diag(var)[1])
UCL

#95% confidence interval for shape parameter
LCL=coefs[2]-1.96*sqrt(diag(var)[2])
LCL
UCL=coefs[2]+1.96*sqrt(diag(var)[2])
UCL

## Estimated survival probability after time 200 weeks
1-pweibull(200,shape=coefs[2],scale=coefs[1])
## Estimated 60 percentile
qweibull(0.75,shape=coefs[2],scale=coefs[1])