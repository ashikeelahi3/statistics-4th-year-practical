# B.Stat.-401: Multivariate Analysis
# Practical_Assignment
# Dr. Md. Rezaul Karim
# Professor, Department of Statistics, University of Rajshahi
# Email: mrkarim@ru.ac.bd 
# May 29, 2026
# ==================================================

# Load required libraries
library(tidyverse)
library(factoextra)
library(psych)
library(corrplot)
library(CCA)
library(CCP)
library(cluster)
library(NbClust)

# Set seed for reproducibility and uniqueness
# Use the last four digits of your student ID 
# to set the seed value for the object U0 below

U0 <- 
  set.seed(U0)
print(paste("The last 4 digits of your ID:", U0))
cat("If it is correct, proceed; Otherwise change it!")

# Generate multivariate dataset with 5 latent factors 
# and 15 observed variables
n <- 300  # number of observations

# Create latent factors
factor1 <- rnorm(n, mean = 50, sd = 10)  # Economic factor
factor2 <- rnorm(n, mean = 60, sd = 12)  # Social factor
factor3 <- rnorm(n, mean = 40, sd = 8)   # Health factor
factor4 <- rnorm(n, mean = 70, sd = 15)  # Education factor
factor5 <- rnorm(n, mean = 55, sd = 11)  # Environmental factor

# Generate observed variables with different loadings on factors
# Economic indicators (load heavily on factor1)
GDP_growth <- 0.8 * factor1 + 0.1 * factor2 + rnorm(n, sd = 5)
unemployment <- -0.7 * factor1 + 0.2 * factor3 + rnorm(n, sd = 4)
inflation <- -0.6 * factor1 + 0.3 * factor4 + rnorm(n, sd = 3)
income_per_capita <- 0.9 * factor1 + 0.1 * factor5 + rnorm(n, sd = 6)

# Social indicators (load heavily on factor2)
crime_rate <- -0.7 * factor2 + 0.2 * factor3 + rnorm(n, sd = 5)
population_density <- 0.6 * factor2 + 0.3 * factor4 + rnorm(n, sd = 8)
social_support <- 0.8 * factor2 + 0.1 * factor5 + rnorm(n, sd = 4)

# Health indicators (load heavily on factor3)
life_expectancy <- 0.8 * factor3 + 0.2 * factor1 + rnorm(n, sd = 3)
infant_mortality <- -0.9 * factor3 + 0.1 * factor2 + rnorm(n, sd = 2)
health_expenditure <- 0.7 * factor3 + 0.2 * factor4 + rnorm(n, sd = 5)

# Education indicators (load heavily on factor4)
literacy_rate <- 0.8 * factor4 + 0.2 * factor1 + rnorm(n, sd = 4)
school_enrollment <- 0.9 * factor4 + 0.1 * factor3 + rnorm(n, sd = 5)
teacher_student_ratio <- 0.7 * factor4 + 0.2 * factor2 + rnorm(n, sd = 3)

# Environmental indicators (load heavily on factor5)
air_quality_index <- -0.8 * factor5 + 0.2 * factor3 + rnorm(n, sd = 10)
green_space <- 0.7 * factor5 + 0.2 * factor1 + rnorm(n, sd = 6)
water_quality <- 0.8 * factor5 + 0.1 * factor4 + rnorm(n, sd = 4)

# Create dataframe
data <- data.frame(
  GDP_growth = GDP_growth + 2,
  unemployment = abs(unemployment + 5),
  inflation = abs(inflation + 3),
  income_per_capita = income_per_capita + 10000,
  crime_rate = abs(crime_rate + 10),
  population_density = population_density + 100,
  social_support = social_support + 4,
  life_expectancy = life_expectancy + 70,
  infant_mortality = abs(infant_mortality + 10),
  health_expenditure = health_expenditure + 1000,
  literacy_rate = literacy_rate + 70,
  school_enrollment = school_enrollment + 60,
  teacher_student_ratio = teacher_student_ratio + 15,
  air_quality_index = abs(air_quality_index + 50),
  green_space = green_space + 20,
  water_quality = water_quality + 30
)

# See every column in a data frame
glimpse(data)

# Standardize the data
data_scaled <- scale(data)

# Display first few rows
head(data_scaled)

summary(data)
summary(data_scaled)


# 1. PRINCIPAL COMPONENT ANALYSIS (PCA)

#Correlation matrix and its plot
cor_matrix <- cor(data_scaled)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Eigenvectors and eigenvalues of the covariance matrix
e <- eigen(cor_matrix)
e.value <- e$values
e.vector <- e$vectors

# Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 30)) +
  ggtitle("Scree Plot - PCA Eigenvalues") +
  theme_minimal()

# Visualize variable contributions
fviz_pca_var(pca_result, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "PCA - Variable Contributions")

# Get loadings and interpret
loadings <- pca_result$rotation[, 1:4]
print("First 4 Principal Component Loadings:")
print(round(loadings, 3))

# Explained variance by components
explained_variance <- summary(pca_result)$importance[2, ]
explained_variance_df <- data.frame(
  Component = paste0("PC", 1:length(explained_variance)),
  Variance_Explained = explained_variance * 100
)
print(explained_variance_df)
# ======================================

# 2. FACTOR ANALYSIS (FA)
# ---------------------------------------

# Cumulative percentage of total variance
cum.e.value <- cumsum(e.value)
cum.e.value/sum(e.value)*100

KMO(cor_matrix) 

cortest.bartlett(cor(data_scaled), n = nrow(data_scaled))

# Scree plot
plot(x = seq(1:length(e.value)), y = e.value, type = "o",
     main="Scree Plot", xlab = "Number of factors", ylab = "Eigenvalue")

# Select the suitable number of factors based on the above scree plot
# and then insert that values to Nfacs

Nfacs <- 
  
  # Perform Factor Analysis with Nfacs factors
  fa_result <- fa(data_scaled, nfactors = Nfacs, rotate = "varimax", fm = "ml")
print(fa_result, digits = 3, sort = TRUE)

loadings <- fa_result$loadings # Factor loadings
loadings

# Perform Factor Analysis with varimax rotation
fa_result_vmax <- factanal(x = data_scaled, factors = Nfacs, rotation="varimax")

print(fa_result_vmax, digits=3, sort=TRUE)
fa_result_vmax$loadings
# =================================

# 3. CANONICAL CORRELATION ANALYSIS (CCA)
# ----------------------------------------

# Split variables into two sets for CCA
# Set 1: Economic + Social variables
X_vars <- data_scaled[, c("GDP_growth", "unemployment", "inflation", 
                          "income_per_capita", "crime_rate", "population_density")]

# Set 2: Health + Education + Environmental variables
Y_vars <- data_scaled[, c("life_expectancy", "infant_mortality", "health_expenditure",
                          "literacy_rate", "school_enrollment", "air_quality_index", 
                          "green_space", "water_quality")]

# Perform Canonical Correlation Analysis
cca_result <- cc(X_vars, Y_vars)

# Display canonical correlations
print("Canonical Correlations:")
print(round(cca_result$cor, 4))

# Test significance of canonical correlations
rho <- cca_result$cor
n <- nrow(X_vars)
p <- ncol(X_vars)
q <- ncol(Y_vars)

# Wilks Lambda test for canonical correlations
significance_test <- p.asym(rho, n, p, q, tstat = "Wilks")
print("Significance Test for Canonical Correlations:")
print(significance_test)

# Canonical loadings for X variables
print("Canonical Loadings for Set X (Economic-Social):")
print(round(cca_result$xcoef[, 1:3], 3))

# Canonical loadings for Y variables
print("Canonical Loadings for Set Y (Health-Education-Environment):")
print(round(cca_result$ycoef[, 1:3], 3))
# =============================================

# Interpretation of CCA Results:

# 4. K-MEANS CLUSTER ANALYSIS
# ------------------------------

# Determine optimal number of clusters using multiple methods
# Method 1: Elbow method
fviz_nbclust(data_scaled, kmeans, method = "wss") +  ggtitle("Elbow Method for Optimal K")

# Method 2: Silhouette method
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data_scaled))
  mean(ss[, 3])
})

optimal_k_sil <- which.max(sil_width) + 1
print(paste("Optimal k by Silhouette method:", optimal_k_sil))

# Select the suitable value for K and insert in below for K.value
K.value <- 
  
# Perform k-means with K.value
set.seed(U0)
kmeans_result <- kmeans(data_scaled, centers = K.value, nstart = 25)

# Add cluster assignments to original data
data_with_clusters <- as.data.frame(data)
data_with_clusters$cluster <- as.factor(kmeans_result$cluster)

# Cluster summary
print("Cluster Sizes:")
print(table(kmeans_result$cluster))

print("Cluster Centers (standardized):")
print(round(kmeans_result$centers, 3))

cluster_summary <- data_with_clusters %>%
  group_by(cluster) %>%  
  summarise(    
    Count = n(),    
    across(everything(), mean)    
  )
print(cluster_summary)
view(cluster_summary)

# Visualize clusters using first two principal components
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores$cluster <- as.factor(kmeans_result$cluster)

cluster_pca_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(level = 0.68) +
  labs(title = "K-Means Clusters Visualized in PCA Space",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
print(cluster_pca_plot)

# Interpretation of K-Means Results:

# Final Integrated Summary

