# Problem – 1: PCA
  
SBP <- c(126, 128, 128, 130, 130, 132)
DBP <- c(78, 80, 82, 82, 84, 86)

# Rename

x <- SBP; y <- DBP

plot(x, y, pch = 19)

S <- matrix(c(cov(x, x), cov(x, y), cov(y, x), cov(y, y)),
            nrow=2, ncol=2, byrow=TRUE,
            dimnames= list(c("x","y"), c("x","y")))

S

a <- eigen(S)

a

# Centered the data

x1 <- x - mean(x)

x1

summary(x1)

y1 <- y - mean(y)

y1

summary(y1)

plot(x1, y1, pch = 19)

# The next step is to calculate the covariance matrix

cov(x1, x1)

cov(y1, y1)

cov(x1, y1)

m <- matrix(c(cov(x1, x1), cov(x1, y1), cov(y1, x1),
              cov(y1, y1)),
            nrow=2, ncol=2, byrow=TRUE,
            
            dimnames= list(c("x","y"),c("x","y")))

m


# Next we need to find the eigenvector and eigenvalues of the covariance matrix

e <- eigen(m)

e

e$values/sum(e$values)

# eigen() decomposition

# $values

# $vectors

#The largest eigenvalue is the first principal component; we multiply the standardized values to the first eigenvector, which is stored in e$vectors[,1].

pc1 <- x1 * e$vectors[1,1] + y1 * e$vectors[2,1]

pc1

pc2 <- x1 * e$vectors[1,2] + y1 * e$vectors[2,2]

pc2


Z <- data.frame(PC1 = pc1, PC2 = pc2)

plot(pc1, pc2, pch = 19)

cov.pc <- matrix(c(cov(pc1, pc1), cov(pc1, pc2),
                   
                   cov(pc2, pc1), cov(pc2, pc2)),
                 
                 nrow=2, ncol=2, byrow=TRUE,
                 
                 dimnames= list(c("pc1","pc2"), c("pc1","pc2")))

cov.pc

# Now to perform PCA using the prcomp() function.

data <- data.frame(x,y)

data.pca <- prcomp(data)

data.pca

names(data.pca)

data.pca$x

plot(data.pca$x[,1], data.pca$x[,2], pch = 19)

# The sign is meaningless here. It’s just chosen at random at the beginning.

# It does not change the relationship.