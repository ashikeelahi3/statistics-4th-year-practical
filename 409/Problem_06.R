# ============================================================
# Problem 6 - Joint-Life and Last-Survivor Status
# Lives: x = age 65, y = age 62; projected out to t = 40 years
# ============================================================

# ------------------------------------------------------------
# 1) Mortality rates, survival probabilities, and interest rate
# ------------------------------------------------------------

age_x <- 65        # age of life x
age_y <- 62        # age of life y

qx <- 0.005        # constant age-specific mortality rate for x
qy <- 0.004        # constant age-specific mortality rate for y

i <- 0.05          # annual effective interest rate
v <- 1 / (1 + i)   # one-year discount factor

n <- 40            # project from t = 0 to t = 40 years
t <- 0:n           # vector of time points

# One-year survival probabilities (constant, since qx/qy are constant)
px <- 1 - qx
py <- 1 - qy

# t-year survival probabilities: tpx = px^t, tpy = py^t
tpx <- px^t
tpy <- py^t

tpx
tpy


# ------------------------------------------------------------
# 2) Joint survival probability (both alive at time t)
#    Under independence: tpxy = tpx * tpy
# ------------------------------------------------------------

tpxy <- tpx * tpy
tpxy


# ------------------------------------------------------------
# 3) Last-survivor probability (at least one alive at time t)
#    Inclusion-exclusion: tp(last) = tpx + tpy - tpxy
# ------------------------------------------------------------

tp_last <- tpx + tpy - tpxy
tp_last


# ------------------------------------------------------------
# 4) Combined table at selected durations
# ------------------------------------------------------------

select_t <- c(0, 1, 5, 10, 15, 20, 25, 30, 40)

cat(sprintf("%-6s  %-12s  %-12s  %-12s  %-14s\n",
            "Year", "tpx", "tpy", "tpxy", "tp(last surv)"))
for (k in select_t) {
  cat(sprintf("%-6d  %-12.6f  %-12.6f  %-12.6f  %-14.6f\n",
              k, tpx[k + 1], tpy[k + 1], tpxy[k + 1], tp_last[k + 1]))
}


# ------------------------------------------------------------
# 5) Joint-life annuity-due:  axy = sum_{t=0}^{n} v^t * tpxy
# ------------------------------------------------------------

axy <- sum(tpxy * v^t)
cat(sprintf("\nJoint life annuity-due (axy): %.4f\n", axy))


# ------------------------------------------------------------
# 6) Last-survivor annuity-due: a_last = sum_{t=0}^{n} v^t * tp_last
#    (equivalently: a_last = ax + ay - axy)
# ------------------------------------------------------------

a_last <- sum(tp_last * v^t)
cat(sprintf("Last survivor annuity-due: %.4f\n", a_last))

# Cross-check via the individual annuities-due
ax <- sum(tpx * v^t)
ay <- sum(tpy * v^t)
cat(sprintf("Individual annuity-due (ax): %.4f\n", ax))
cat(sprintf("Individual annuity-due (ay): %.4f\n", ay))
cat(sprintf("Check ax + ay - axy = %.4f (should equal a_last)\n", ax + ay - axy))