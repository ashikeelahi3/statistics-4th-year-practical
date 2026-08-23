##4
##Practical for odds ratio
n=35840;p11=0.0140;p12=0.1147;p21=0.0088;p22=0.8625
n11=n*0.0140
n12=n*0.1147
n21=n*0.0088
n22=n*0.8625
data <- matrix(c(n11,n12,n21,n22), 
               nrow = 2,byrow = TRUE,
               dimnames = list("Birthweight" = c("<= 2.5 kg", "> 2.5 kg"),
                               "Outcome" = c("Dead", "Alive")))
data
#install.packages("epitools")
library(epitools)

#(i)
odds.ratio<- oddsratio(data)
odds.ratio

#(ii)
chi_test <- chisq.test(data)
chi_test


o=(n11*n22)/(n12*n21)
o

ar=(p11*p22-p12*p21)/((p11+p21)*(p21+p22))
ar
se=sqrt((p12+ar*(p11+p22))/(n*p21))
se

LCL=1-exp(log(1-ar)+1.96*se)
LCL
UCL=1-exp(log(1-ar)-1.96*se)
UCL
