# Part a) Generate and Plot Original Tones

library(MASS) # To use mvrnorm()
set.seed(123)

# Tone 1 corrupted by noise
s1 <- 0.7*sin((1:1000)/19+0.57*pi) + mvrnorm(n =1000, mu=0, Sigma= 0.004)
s1 <- as.numeric(s1)

# s1 <- 0.7*sin((1:1000)/19+0.57*pi) + rnorm(n =1000, 0, 0.004)


# Tone 2 corrupted by noise
s2 <- sin((1:1000)/33) + mvrnorm(n = 1000, mu = 0.03, Sigma = 0.005)
s2 <- as.numeric(s2)

# Plot to see Original (source) Tones
par(mfcol = c(2, 1))
plot(s1, main="Additive Noise Corrupted Tone 1", xlab="Time", ylab = "Amplitude")
plot(s2, main = "Additive Noise Corrupted Tone 2", xlab = "Time", ylab = "Amplitude")

# Part b) Create Source Matrix (S)

S <- matrix(c(s1, s2), 1000, 2)
head(S)

# Part c) Create Mixing Matrix (A) and Obtain Observed Mixtures (X)

A <- matrix(c(1, 1.73, -2, 3.41), 2, 2, byrow = TRUE)
X <- S %*% A  # Mixing Process: X = SA
head(X)

# Part d) Joint Distributions Scatter Plots
# par(mfcol = c(1,1))
dev.off()    # reset the mfrow/mfcol parameter

# Scatter plot of S
plot(S, main = "Joint distribution of the source data with 2\nindependent components", 
     xlab = "1st Dimension in S", ylab = "2nd Dimension in S")

# Scatter plot of X
plot(X, main = "Joint distribution of the observed linearly\nmixtures data, x1 and x2", 
     xlab = "1st Dimension in X", ylab = "2nd Dimension in X")

# Part e) Estimate Source Tones Using fastICA

# install.packages("fastICA")
library(fastICA)
?fastICA

est <- fastICA(X, 2, alg.typ ="parallel", fun="logcosh",
               method = "C", verbose=TRUE)
str(est)

# Estimates of source tones
est$S

# Part f) Plot and Compare Original vs. Estimated Tones

# Original and Estimated tones, S and \hat{S}:
dev.off()    # reset the mfrow/mfcol parameter
ymin <- min(s1, s2, est$S[,1], est$S[,2])
ymax <- max(s1, s2, est$S[,1], est$S[,2])

plot(s1, col="black", main = "Original and Estimated Tones",
     xlab = "Time", ylab ="Amplitude", ylim=c(ymin,ymax))
lines(est$S[,1], col="red")    # first column of S_hat
lines(s2, col="green")
lines(est$S[,2], col="blue")  # second column of S_hat
mtext("Original Tones in Black and Green, Estimated Tones in\nRed and Blue")

# The combination of 6 graphs - Original, Mixtures, and Estimated Tones:
par(mfcol = c(2, 3))
plot(1:1000, S[,1], type = "l", xlab = "Original, S1", ylab = "")
plot(1:1000, S[,2], type = "l", xlab = "Original, S2", ylab = "")
plot(1:1000, X[,1], type = "l", xlab = "Mixture, X1", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "Mixture, X2", ylab = "")
plot(1:1000, est$S[,1], type="l", xlab="Estimated, S1", ylab="")
plot(1:1000, est$S[,2], type="l", xlab="Estimated, S2", ylab="")
mtext("Comparison of Original, Mixture, and Estimated Tones",
      side=3, line= -3, outer=TRUE)
