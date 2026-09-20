library(lifecontingencies)

# ---- 1. Age-specific mortality & one-year survival probabilities ----
n <- 41                    # projection horizon: t = 0,...,40 years
i <- 0.05                  # interest rate
v <- 1 / (1+i)

t  <- 0:(n-1)
qx <- 0.005 * exp(0.05*t)   # mortality rate for x (age 65), rising with duration
qy <- 0.004 * exp(0.04*t)   # mortality rate for y (age 62), rising with duration
px <- pmin(1 - qx, 1)       # one-year survival probability, x
py <- pmin(1 - qy, 1)       # one-year survival probability, y

# ---- 2-3. Cumulative survival probabilities (vectorized - no loops needed) ----
tpx     <- c(1, cumprod(px))     # t-year survival probability of x
tpy     <- c(1, cumprod(py))     # t-year survival probability of y
tpxy    <- tpx * tpy             # joint survival: both alive (independence)
# for (k in 1:n) tpxy[k + 1] <- tpxy[k] * px1[k] * px2[k]
tp_last <- tpx + tpy - tpxy      # last survivor: at least one alive (inclusion-exclusion)



# ---- 4. Table for selected durations ----
years <- c(0, 1, 5, 10, 15, 20, 25, 30, 40)
data.frame(t = years,
           tpx = tpx[years+1], tpy = tpy[years+1],
           tpxy = tpxy[years+1], tp_last = tp_last[years+1])

# ---- 5-6. Annuity-due present values ----
disc   <- v^(0:n)
axy    <- sum(tpxy * disc)       # joint life annuity-due
a_last <- sum(tp_last * disc)    # last survivor annuity-due
axy
a_last

