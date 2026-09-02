# ============================================================
# Problem 3
# ============================================================

# ------------------------------------------------------------
# a) Gross and net rate of interest earned by a life insurance
#    fund during the year
#
#    Formula (standard actuarial "fund method"):
#      B = A + Premiums + Interest - Claims - Expenses
#      i = 2*I / (A + B - I)
#    where A = fund at start of year, B = fund at end of year,
#    I = interest earned (net for net rate, gross for gross rate)
# ------------------------------------------------------------

A  <- 12500000    # Fund at 1 January (start of year)
PI <- 1000000     # Premium income during the year
GI <- 1250000     # Gross interest earned
IT <- 125000      # Income tax paid on the interest
C  <- 250000      # Claims paid during the year
E  <- 125000      # Management expenses

I  <- GI - IT      # Net interest = gross interest - tax

# Fund at end of year (31 December):
# start + premiums + net interest - claims - expenses
B <- A + PI + I - C - E
B
# B = 14,250,000

# --- Net rate of interest ---
# i = 2I / (A + B - I)
i_net <- (2 * I) / (A + B - I)
i_net
i_net * 100    # as a percentage
# Net rate of interest ≈ 8.78%

# --- Gross rate of interest ---
# Same formula, but numerator uses gross interest (I1), while the
# denominator still uses net interest I (standard convention)
I1 <- GI
i_gross <- (2 * I1) / (A + B - I)
i_gross
i_gross * 100   # as a percentage
# Gross rate of interest ≈ 9.76%


# ------------------------------------------------------------
# b) Capital redemption policy: secure Tk. 10,000 at the end of
#    10 years via level annual premiums, i = 10% p.a.
#    (i)  Find the annual premium and the policy value
#    (ii) Find the paid-up policy value at the end of year 5
# ------------------------------------------------------------

i <- 0.10          # annual interest rate
n <- 10            # full policy term (years)
V <- 1 / (1 + i)   # one-year discount factor
K <- 10000         # sum to be secured at end of term

# --- (i) Annual premium P ---
# The premium P, paid as an annuity-due for n years, must accumulate
# (with interest) to K at time n. Equivalently, its present value
# (as an annuity-due) must equal the present value of K:
#     P * ä_n| = K * V^n
#     P = (K * V^n) / ä_n|

AC <- (V^n) * K          # present value of the K=10000 due at time n
AC

a_n  <- (1 - V^n) / i    # a_n|   : PV of annuity-immediate, n years
a_n_due <- (1 + i) * a_n # ä_n|   : PV of annuity-due, n years (premiums paid at start of each year)

P <- AC / a_n_due        # solve for annual premium
P
# Annual premium P ≈ 570.41

# --- Policy value at end of year 5 ---
# This is the accumulated value of premiums actually paid in the
# first 5 years, i.e. P times the accumulated annuity-due factor
# for 5 years: S-double-dot_5|

n2 <- 5                          # time at which we value the policy
S_n    <- ((1 + i)^n2 - 1) / i   # S_5|  : accumulated annuity-immediate factor
S_n_due <- (1 + i) * S_n         # S-double-dot_5| : accumulated annuity-due factor

PValue <- P * S_n_due
PValue
# Policy value at end of year 5 ≈ 3830.67

# --- (ii) Paid-up policy value at end of year 5 ---
# If premiums stop being paid after year 5, the policy value already
# built up is "rolled forward" (discounted back to today, then
# re-projected to maturity) by dividing by V^(n - n2), i.e. the
# discount factor for the REMAINING term (years 5 to 10):
#     Paid-up value = PValue / V^(n - n2)

Paidvalue <- PValue / (V^(n - n2))
Paidvalue
# Paid-up policy value at end of year 5 ≈ 6169.33