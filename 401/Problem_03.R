# install.packages("psych")

library(psych) # For EFA and factor analysis tools
car.data <- read.csv("car_sales.csv", as.is=TRUE)
head(car.data)

# Pearsons Correlation matrix
corr_matrix <- cor(car.data)

# Eigenvalues
ev <- eigen(corr_matrix, symmetric = TRUE)
e.value <- ev$values

# cumulative percentage of total variance
cum.e.value <- cumsum(e.value)
cum.e.value/sum(e.value)*100

# Kaiser-Meyer-Olkin (KMO) Test of Sampling Adequacy. 
# Measures sampling adequacy (values > 0.6 are acceptable).
KMO(corr_matrix)

# The overall KMO for our data is greater than 0.80,
# which is meritorious - this suggests that we can go
# ahead with our planned factor analysis.


# Bartlett’s Test of Sphericity. Test if the correlation matrix is an 
# identity matrix (we want p < 0.05).

cortest.bartlett(cor(car.data), n = nrow(car.data))

# Scree plot
plot(x = seq(1:length(e.value)), y = e.value, type = "o",
     main="Scree Plot", xlab = "Number of factors", ylab
     = "Eigenvalue")

# Number of factors based on total variation in eigenvalues.
Nfacs <- 3

# Exploratory Factor Analysis (EFA) with 3 factors using the observed data
efa.3 <- factanal(x = car.data, factors = Nfacs,
                  rotation="varimax")
efa.3

loadings <- efa.3$loadings # Factor loadings
loadings

print(efa.3, digits=3, sort=TRUE)


