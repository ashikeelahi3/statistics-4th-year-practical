library(lifecontingencies)

#  Assign the probabilities for different causes (e.g., q1:death, q2:withdrawal, q3: disability)
# Assumes ages 0 to 100
n <- 100
age <- 0:n
q1 <- rep(0.01, n+1) # Cause 1 rate
q2 <- rep(0.02, n+1) # Cause 2 rate
q3 <- rep(0.03, n+1) # Cause 3 rate

# ii). Create a Data Frame for the Multiple Decrement Table
multi_table <- data.frame(age,q1,q2,q3)
multi_table

# iii). The total value for all causes
multi_table$total_q <- q1+q2+q3

# iv). survival total probabilities (p_tau)
multi_table$p_tau <- 1 - multi_table$total_q
multi_table

# v). Compute number of lives (l_tau)
# Starting with 100,000 people
l0 <- 100000
multi_table$l_tau <- 0
multi_table$l_tau[1] <- l0
for(i in 2:nrow(multi_table)){
  multi_table$l_tau[i] <- multi_table$l_tau[i-1] * multi_table$p_tau[i-1]
}

# vi-viii). Calculate deaths/decrements by cause
multi_table$d1 <- multi_table$l_tau * multi_table$q1
multi_table$d2 <- multi_table$l_tau * multi_table$q2
multi_table$d3 <- multi_table$l_tau * multi_table$q3

#ix)  View result
multi_table

