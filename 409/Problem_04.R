# ============================================================
# Problem 4 - Churn Data: Survival Analysis & Cox PH Model
# Packages: survival (for Surv, survfit, coxph)
# ============================================================

library(survival)

# ------------------------------------------------------------
# a) Load data and describe its characteristics
# ------------------------------------------------------------

data <- read.csv(file.choose())   # interactively select the churn CSV file

dim(data)      # number of rows (customers) and columns (variables)
head(data)     # preview first few rows
names(data)    # list all variable names
str(data)      # shows data type of each variable (numeric/int/character/logical)


# ------------------------------------------------------------
# b) Recode Churn as 0 = False, 1 = True
# ------------------------------------------------------------

dat <- data[, c("Account.length", "Churn")]

# Recode the logical/character Churn column into numeric 0/1
dat$Churn <- ifelse(dat$Churn == "TRUE", 1, 0)
dat$Churn

# Comment: The original Churn variable is categorical (True/False).
# Recoding converts it to a numeric event indicator: 0 = did not churn
# (censored), 1 = churned (event occurred) - required by Surv().


# ------------------------------------------------------------
# c) Fit the survival model (Kaplan-Meier) and plot it
# ------------------------------------------------------------

# Surv(time, event): time = Account.length (time until churn or censoring)
#                    event = 1 if churned, 0 if censored (still active)
survdat <- Surv(time = dat$Account.length, event = dat$Churn)

# ~1 means no covariates - overall Kaplan-Meier survival curve
fit <- survfit(survdat ~ 1, se = TRUE)

plot(fit,
     main = "Survival Function",
     xlab = "Time (Account length, days)",
     ylab = "Survival Probability")

fit   # prints summary including median survival time



# ------------------------------------------------------------
# d) Fit a Cox Proportional Hazards model
# ------------------------------------------------------------


fitt <- coxph(
  Surv(Account.length, Churn) ~ State + Int.l.Plan + VMail.Plan + Day.Mins,
  data = within(data, Churn <- ifelse(Churn == "TRUE", 1, 0))
)

summary(fitt)
