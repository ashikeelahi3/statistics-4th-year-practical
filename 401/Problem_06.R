# Part a) Read and Summarize Data

pmaam.data <- read.csv("pmaam.csv", as.is=TRUE)
head(pmaam.data)
summary(pmaam.data)


# Part b) Prepare Data for CCA (Split into X and Y)
X <- pmaam.data[, 1:3]  # psychological variables
Y <- pmaam.data[, 4:7]  # academic variables


# Part c) Check Correlations Within and Between Sets

cat("Correlations within psychological variables:\n")
print(round(cor(X), 3))

cat("\nCorrelations within academic variables:\n")
print(round(cor(Y), 3))

cat("\nCorrelations between sets:\n")
print(round(cor(X, Y), 3))


# Part d) Perform Canonical Correlation Analysis (CCA)
# (Note: The `cc()` function requires loading the `CCA` library in R)

library(CCA)
CCA.result <- cc(X, Y)
print(CCA.result)


# Part e) Significance Testing of Canonical Correlations

library(CCP)
rho <- CCA.result$cor
n <- dim(pmaam.data)[1]
p <- ncol(X)    # number of variables in first set
q <- ncol(Y)    # number of variables in second set

# Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = "Wilks")
p.asym(rho, n, p, q, tstat = "Hotelling")
p.asym(rho, n, p, q, tstat = "Pillai")
p.asym(rho, n, p, q, tstat = "Roy")


# Part f) Compute Canonical Coefficients, Loadings, & Cross-Loadings

# The raw canonical coefficients:
CCA.result$xcoef
CCA.result$ycoef

# Canonical loadings:
loadings.X <- cor(X, CCA.result$scores$xscores)
loadings.Y <- cor(Y, CCA.result$scores$yscores)

cat("Canonical loadings for psychological variables(X):\n")
print(round(loadings.X, 3)) 
cat("\nCanonical loadings for academic performance (Y):\n")
print(round(loadings.Y, 3)) 

# Cross-loadings:
Cross.loadings.X <- cor(X, CCA.result$scores$yscores)
Cross.loadings.Y <- cor(Y, CCA.result$scores$xscores)

cat("\nCross Loadings - X variables with Y canonical variables:\n")
print(round(Cross.loadings.X, 3))
cat("\nCross Loadings - Y variables with X canonical variables:\n")
print(round(Cross.loadings.Y, 3))


# Part g) Visualization

# (i) Plot of first pair of canonical variables:
  
plot(CCA.result$scores$xscores[, 1],
     CCA.result$scores$yscores[, 1],
     xlab = "First psychological canonical variable",
     ylab = "First academic canonical variable",
     main = "First canonical pair",
     pch = 19, col = "blue", alpha = 0.6)
abline(lm(CCA.result$scores$yscores[, 1] ~ CCA.result$scores$xscores[, 1]),
       col = "red", lwd = 2)


# (ii) Plot of second pair of canonical variables:

plot(CCA.result$scores$xscores[, 2],
     CCA.result$scores$yscores[, 2],
     xlab = "2nd psychological canonical variable",
     ylab = "2nd academic canonical variable",
     main = "Second canonical pair",
     pch = 19, col = "blue", alpha = 0.6)
abline(lm(CCA.result$scores$yscores[, 2] ~ CCA.result$scores$xscores[, 2]),
       col = "red", lwd = 2)

