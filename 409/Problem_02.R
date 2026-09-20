library(lifecontingencies)

# (a) Loan
P <- 150000
i <- 0.07
n <- 10
m <- 12

im <- (1+i)^(1/m) - 1

R = (1/m) * P / annuity(i = i, n = n, k = m, type = "immediate")
R 

balance <- P
loan <- data.frame()

for (mo in 1:(n*m)) {
  interest <- balance * im
  capital <- R - interest
  balance <- balance - capital
  loan <- rbind(loan, c(mo, R, interest, capital, balance))
}

names(loan) <- c("Month","Payment","Interest","Capital","Balance")
round(loan, 2)
loan[30, ]


# (b) Saving scheme
person <- LETTERS[1:5] # c("A", "B", "C", "D", "E")
P <- c(150000,120000,200000,100000,160000)
i <- c(.07,.06,.08,.075,.08)
n <- c(10,12,15,10,20)

data.frame(
  Person = person,
  Saving = P / mapply(accumulatedValue, i, n),
  Immediate = mapply(annuity, i, n,
                     MoreArgs = list(k = m, type = "immediate")),
  Due = mapply(annuity, i, n,
               MoreArgs = list(k = m, type = "due"))
)


# (c) Present value
500 * annuity(.04, 5, type = "immediate")
500 * annuity(.04, 5, type = "due")


# (d) Effective annual rate
(1 + 0.21/m)^m - 1


# (e) Future value
1000 * accumulatedValue(0.03, 5) * 1.03
