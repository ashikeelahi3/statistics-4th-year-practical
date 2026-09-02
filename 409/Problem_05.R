# ============================================================
# Problem 5 - Multiple Decrement Table
# Causes: q1 = death, q2 = withdrawal, q3 = disability
# ============================================================

# ------------------------------------------------------------
# i) Concept
# ------------------------------------------------------------
# A multiple-decrement table models a group of lives that can leave
# observation for more than one reason (decrement) - here: death,
# withdrawal, and disability. At each age, a life is exposed to all
# three causes simultaneously; the "total" decrement rate is the
# combined chance of leaving for ANY of the causes.


# ------------------------------------------------------------
# 1) Assign decrement probabilities: q1 = 0.01, q2 = 0.02, q3 = 0.03
#    (ages 0 to 100)
# ------------------------------------------------------------

n <- 100
age <- 0:n                # ages 0 through 100 (101 rows)
q1 <- rep(0.01, n + 1)     # Cause 1 rate: death
q2 <- rep(0.02, n + 1)     # Cause 2 rate: withdrawal
q3 <- rep(0.03, n + 1)     # Cause 3 rate: disability


# ------------------------------------------------------------
# 2) Create the data frame for the multiple decrement table
# ------------------------------------------------------------

multi_table <- data.frame(age = age, q1 = q1, q2 = q2, q3 = q3)
multi_table


# ------------------------------------------------------------
# 3) Total decrement rate for all causes combined (q_tau)
# ------------------------------------------------------------

multi_table$total_q <- multi_table$q1 + multi_table$q2 + multi_table$q3
multi_table$total_q
# total_q = 0.01 + 0.02 + 0.03 = 0.06 at every age


# ------------------------------------------------------------
# 4) Survival (total) probability at each age: p_tau = 1 - q_tau
# ------------------------------------------------------------

multi_table$p_tau <- 1 - multi_table$total_q
multi_table$p_tau


# ------------------------------------------------------------
# 5) Number of lives (l_tau) - starting with 100,000 people
# ------------------------------------------------------------



l0 <- 100000                       # radix: starting number of lives at age 0

multi_table$l_tau <- 0             # initialize the column
multi_table$l_tau[1] <- l0         # l_tau at age 0 = the radix

# Each subsequent l_tau is the previous l_tau reduced by the
# previous period's total survival probability
for (i in 2:nrow(multi_table)) {
  multi_table$l_tau[i] <- multi_table$l_tau[i - 1] * multi_table$p_tau[i - 1]
}
multi_table$l_tau


# ------------------------------------------------------------
# 6-8) Number of decrements by each cause
#      d_i(x) = l_tau(x) * q_i(x)   for i = death, withdrawal, disability
# ------------------------------------------------------------

multi_table$d1 <- multi_table$l_tau * multi_table$q1   # decrements by death
multi_table$d1

multi_table$d2 <- multi_table$l_tau * multi_table$q2   # decrements by withdrawal
multi_table$d2

multi_table$d3 <- multi_table$l_tau * multi_table$q3   # decrements by disability
multi_table$d3


# ------------------------------------------------------------
# 9) Full table view, and decrements at age 15 specifically
# ------------------------------------------------------------

multi_table   # full table: age, q1, q2, q3, total_q, p_tau, l_tau, d1, d2, d3

# Row for age 15 (age 0 is row 1, so age 15 is row 16)
multi_table[multi_table$age == 15, ]
