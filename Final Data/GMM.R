# Load necessary libraries
library(mvtnorm) # for multivariate normal distribution
library(MASS)    # for Mahalanobis distance

# Function to calculate Mahalanobis distance
mahalanobis_distance <- function(x, mean, cov) {
  sqrt(mahalanobis(x, mean, cov))
}

# Function for Gaussian Mixture Model with EM Algorithm
gmm_em_mahalanobis <- function(data, K, tol = 1e-6, max_iter = 100) {
  data <- scale(data)
  N <- nrow(data)
  D <- ncol(data)
  
  # Initialize parameters
  set.seed(123)
  means <- data[sample(1:N, K), ]           # Randomly initialize means
  covariances <- replicate(K, diag(D), simplify = FALSE) # Initialize covariances
  weights <- rep(1/K, K)                    # Equal initial weights
  
  # Responsibilities (probability of each data point belonging to each cluster)
  responsibilities <- matrix(0, N, K)
  
  log_likelihood <- 0
  iter <- 0
  
  repeat {
    # E-step: Compute responsibilities
    for (k in 1:K) {
      responsibilities[, k] <- weights[k] * dmvnorm(data, mean = means[k,], sigma = covariances[[k]])
    }
    responsibilities <- responsibilities / rowSums(responsibilities)
    
    # M-step: Update weights, means, and covariances
    for (k in 1:K) {
      Nk <- sum(responsibilities[, k])
      weights[k] <- Nk / N
      means[k, ] <- colSums(responsibilities[, k] * data) / Nk
      centered_data <- sweep(data, 2, means[k, ])
      covariances[[k]] <- t(centered_data) %*% (responsibilities[, k] * centered_data) / Nk
    }
    
    # Compute log-likelihood
    new_log_likelihood <- sum(log(rowSums(sapply(1:K, function(k) {
      weights[k] * dmvnorm(data, mean = means[k,], sigma = covariances[[k]])
    }))))
    
    # Check for convergence
    if (abs(new_log_likelihood - log_likelihood) < tol || iter >= max_iter) {
      break
    }
    log_likelihood <- new_log_likelihood
    iter <- iter + 1
  }
  
  # Assign each data point to the cluster with the highest responsibility
  cluster_assignment <- apply(responsibilities, 1, which.max)
  
  # Calculate Mahalanobis distance for each point to its assigned cluster
  mahalanobis_distances <- sapply(1:N, function(i) {
    mahalanobis_distance(data[i,], means[cluster_assignment[i],], covariances[[cluster_assignment[i]]])
  })
  
  list(means = means, covariances = covariances, weights = weights, 
       responsibilities = responsibilities, clusters = cluster_assignment, 
       mahalanobis_distances = mahalanobis_distances)
}

# # Generate some example data for testing
# set.seed(42)
# data <- cbind(
#   as.numeric(SENSEX.dat),
#   as.numeric(CPI.dat)
# )

# K <- 2
# # Run GMM with Mahalanobis distance in 2D
# gmm_result <- gmm_em_mahalanobis(data, K)
# 
# # Plot results
# plot(data, col = gmm_result$clusters, pch = 16, xlab = "SENSEX", ylab = "CPI",
#      main = "Gaussian Mixture Model Clustering (Mahalanobis Distance)")
# points(gmm_result$means, col = 1:K, pch = 19, cex = 2)
