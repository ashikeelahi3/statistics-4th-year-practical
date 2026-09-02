# ============================================================
# Problem 2 - Loans, Savings, Annuities, Effective Interest
# Package: lifecontingencies (uses its annuity() and 
#          accumulatedValue() functions)
# ============================================================

library(lifecontingencies)

# ------------------------------------------------------------
# a) Loan of Tk. 150,000 at 7% annual interest, 10-year term
#    -> monthly installment + full amortization schedule
#    -> capital & balance for month 30
# ------------------------------------------------------------

i <- 0.07                     # nominal annual interest rate
Capital <- 150000              # loan principal
n_years <- 10                  # loan term in years
n_months <- n_years * 12       # total number of monthly payments

# Convert the annual rate to an equivalent effective monthly rate
monthlyInt <- (1 + i)^(1/12) - 1

# Monthly installment (R):
# annuity(i, n, k, type="immediate") gives the PV of an annuity-immediate
# paid k times per year for n years at annual rate i.
# Dividing the loan by (n years * k payments/year) worth of annuity value,
# scaled by 1/12, gives the level monthly payment.
R <- (1/12) * Capital / annuity(i = i, n = n_years, k = 12, type = "immediate")
R
# Monthly installment ≈ 1725.05

# --- Loan amortization schedule (balance, interest, capital repaid) ---
balance   <- numeric(n_months + 1)   # outstanding balance after each payment
interests <- numeric(n_months + 1)   # interest portion of each payment
capitals  <- numeric(n_months + 1)   # principal (capital) portion of each payment

balance[1]   <- Capital   # starting balance (before any payment, "month 0")
interests[1] <- 0
capitals[1]  <- 0

for (m in 2:(n_months + 1)) {
  interests[m] <- balance[m - 1] * monthlyInt        # interest on previous balance
  capitals[m]  <- R - interests[m]                    # remainder pays down principal
  balance[m]   <- balance[m - 1] * (1 + monthlyInt) - R  # new outstanding balance
}

# Combine into a single data frame: row 1 = month 0 (no payment yet)
loanSummary <- data.frame(
  month     = 0:n_months,
  payment   = c(0, rep(R, n_months)),
  interest  = interests,
  capital   = capitals,
  balance   = balance
)
loanSummary                     # full amortization table

# Capital repaid and balance remaining at month 30 specifically
loanSummary[loanSummary$month == 30, ]


# ------------------------------------------------------------
# b) Five savers, each wants to reach a target amount with
#    different terms/rates -> required level annual deposit,
#    plus annuity-immediate and annuity-due values for each
# ------------------------------------------------------------

# accumulatedValue(i, n) = future value factor of 1 per year for n years
# Required annual deposit R = Target / accumulatedValue(i, n)

RA <- 150000 / accumulatedValue(i = 0.07,  n = 10)   # Person A
RB <- 120000 / accumulatedValue(i = 0.06,  n = 12)   # Person B
RC <- 200000 / accumulatedValue(i = 0.08,  n = 15)   # Person C
RD <- 100000 / accumulatedValue(i = 0.075, n = 10)   # Person D
RE <- 160000 / accumulatedValue(i = 0.08,  n = 20)   # Person E

# Required annual savings for each person
c(A = RA, B = RB, C = RC, D = RD, E = RE)

# --- Annuity values (per unit payment) for each person's term/rate ---
# k = 1  -> annual payments (annuity-immediate, paid at year-end)
# k = 12 -> monthly payments (annuity-due, paid at start of period)
# (kept as in the original setup: immediate at k=1, due at k=12)

iannuity_A <- annuity(i = 0.07,  n = 10, k = 1,  type = "immediate")
dannuity_A <- annuity(i = 0.07,  n = 10, k = 12, type = "due")
c(A_immediate = iannuity_A, A_due = dannuity_A)

iannuity_B <- annuity(i = 0.06,  n = 12, k = 1,  type = "immediate")
dannuity_B <- annuity(i = 0.06,  n = 12, k = 12, type = "due")
c(B_immediate = iannuity_B, B_due = dannuity_B)

iannuity_C <- annuity(i = 0.08,  n = 15, k = 1,  type = "immediate")
dannuity_C <- annuity(i = 0.08,  n = 15, k = 12, type = "due")
c(C_immediate = iannuity_C, C_due = dannuity_C)

iannuity_D <- annuity(i = 0.075, n = 10, k = 1,  type = "immediate")
dannuity_D <- annuity(i = 0.075, n = 10, k = 12, type = "due")
c(D_immediate = iannuity_D, D_due = dannuity_D)

iannuity_E <- annuity(i = 0.08,  n = 20, k = 1,  type = "immediate")
dannuity_E <- annuity(i = 0.08,  n = 20, k = 12, type = "due")
c(E_immediate = iannuity_E, E_due = dannuity_E)


# ------------------------------------------------------------
# c) PV of 5 annual payments of Tk. 500 at i = 4%
#    (i) first payment at end of year 1  -> annuity-immediate
#    (ii) first payment at time 0        -> annuity-due
# ------------------------------------------------------------

i <- 0.04
V <- 1 / (1 + i)     # one-year discount factor

# (i) Annuity-immediate: payments at t = 1,2,3,4,5
#     PV = 500 * (1 - V^5) / i
PV_immediate <- 500 * ((1 - V^5) / i)
PV_immediate
# PV ≈ 2225.91

# (ii) Annuity-due: payments at t = 0,1,2,3,4
#     PV = 500 * [1 + (1 - V^4)/i]
PV_due <- 500 * (1 + ((1 - V^4) / i))
PV_due
# PV ≈ 2317.50


# ------------------------------------------------------------
# d) Nominal 21% p.a. compounded monthly -> effective annual rate
#    ia = (1 + r/m)^m - 1
# ------------------------------------------------------------

r <- 0.21    # nominal annual rate
m <- 12      # compounding periods per year

i_a <- (1 + r/m)^m - 1
i_a                 # effective annual rate (decimal)
i_a * 100           # effective annual rate (%)


# ------------------------------------------------------------
# e) Future value of an annuity-due:
#    $1,000/year, i = 3%, n = 5, first deposit made today
#    FV = (1+i) * P * [((1+i)^n - 1) / i]
# ------------------------------------------------------------

P <- 1000    # annual deposit
i <- 0.03    # effective annual rate
n <- 5       # number of deposits

FV <- (1 + i) * P * (((1 + i)^n - 1) / i)
FV
# FV ≈ 5468.41