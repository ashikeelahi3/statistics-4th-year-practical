library(lifecontingencies)

# ============================================================
# 3(a) Gross and net rate of interest earned
#      -- pure arithmetic, no actuarial function needed here
# ============================================================
A  <- 12500000     # fund at start of year
PI <- 1000000      # premium income
GI <- 1250000      # gross interest
IT <- 125000       # income tax
C  <- 250000       # claims
E  <- 125000       # management expenses

I <- GI - IT       # Net interest
B <- A + PI + I - C - E  # fund at the end of the year
B

i_net <- (2*I) / (A + B - I)
i_net * 100

i_gross <- (2*GI) / (A + B - I)
i_gross * 100


# ============================================================
# 3(b) Capital redemption policy -- using annuity() / accumulatedValue()
# ============================================================
i <- 0.10
n_term <- 10        # full policy term (years)
K <- 10000          # sum to be redeemed at maturity

# (i) Annual premium: PV(premiums) = PV(benefit)
#     ä_n| via annuity(..., type = "due")
a_due <- annuity(i = i, n = n_term, type = "due")
P <- K * (1+i)^-n_term / a_due
P

# Policy value at duration t = accumulated value of premiums paid so far
#     s̈_t| via accumulatedValue(..., type = "due")
t <- 5
s_due_t <- accumulatedValue(i = i, n = t, type = "due")
PolicyValue_t <- P * s_due_t
PolicyValue_t

# (ii) Paid-up policy value: roll the policy value at t forward to maturity
PaidUpValue <- PolicyValue_t * (1+i)^(n_term - t)
PaidUpValue