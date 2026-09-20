library(lifecontingencies)
showClass("lifetable")
showClass("actuarialtable")
x_sample<-seq(from=0, to=18, by=1)
lx_sample<-c(900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50,0)

# a) Construct life table with its different columns
lifetable<-new("lifetable",x=x_sample,lx=lx_sample, name="lifetable")
print(lifetable)

# b) Create actuarial table with interest rate 3% in terms of monetary function.
exampleAct<-new("actuarialtable", x=x_sample, lx=lx_sample, interest = 0.03, name ="example actuarialtable")
print(exampleAct)

# c) Calculate the probability that a person age 5 will survive 1 year.
DemoEX1<-pxt(lifetable, 5,1)
DemoEX1

# d) Calculate the probability that a person age 5 die within 1 year
DemoEX2<-qxt(lifetable, 5, 1)
c(DemoEX2)

# e) Calculate the probability that a person age 5 will not die between age 6 and 7.
l5=650; l6=600; l7=550
ans=1-(l6-l7)/l5
ans


# f) Calculate the complete expectation of life at age 5.
DemoEX3<-exn(lifetable, 5, type="complete")
c(DemoEX3)

# g) Calculate the central death rate at age 5.
DemoEX4<-mxt(lifetable, 5, 1)
c(DemoEX4)


# h) Calculate the number of death that is expected to occur between ages 5 to 6.
DemoEX5<-dxt(lifetable, 5, 1)
c(DemoEX5)


# i) Calculate the total number of death that expected to occur between the ages 10 to 15.
DemoEX10<-dxt(lifetable, 10, 5)
c(DemoEX10)


# j) From actuarial table, evaluate the value of a 15 years term life insurance for an aged 25
Axn(actuarialtable=exampleAct, x=25, n=15)
Axn(actuarialtable=exampleAct, x=10, n=15)

# k) Evaluate the value of a 10 year term life insurance for an aged 22 with interest rate 6%.
Axn(actuarialtable=exampleAct, x=22, n=10, i=0.06)
Axn(actuarialtable=exampleAct, x=10, n=12, i=0.06)

# l) Calculate the value of the following assurances using actuarial table from (b) on a life aged 10, the sum assured being payable at the end of the year of death
# i) Whole-life assurance of 1000
WL10 <- 1000 * Axn(actuarialtable = exampleAct, x = 10)
WL10

# ii) Pure endowment assurance of 500, term 6 years
PE10_6 <- 500 * Exn(actuarialtable = exampleAct, x = 10, n = 6)
PE10_6

# iii) Temporary (term) assurance of 1000, term 6 years
Term10_6 <- 1000 * Axn(actuarialtable = exampleAct, x = 10, n = 6)
Term10_6

# iv) Endowment assurance of 1000, term 5 years
End10_5  <- 1000 * AExn(actuarialtable = exampleAct, x = 10, n = 5)
End10_5
