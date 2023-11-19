# Multivariate Normal Generation using Spectral and Cholesky decompositions 

mu <- c(0,0)
sigma <- matrix(c(1, .9, .9, 1), nrow=2, ncol=2)
rmvn.eigen <- function (n, mu, sigma) 
  {
  d <- length(mu)
  ev <- eigen(sigma, symmetric = TRUE)
  lambda <- ev$values
  V <- ev$vectors
  R <- V %*% diag(sqrt(lambda)) %*% t(V)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
  return(X)
}

X <- rmvn.eigen(1000, mu, sigma)
plot(X, xlab = "x", ylab="y", pch = 20)

print(colMeans(X))
print(cor(X))


rmvn.Cholesky <- function(n, mu, sigma)
{
  d <- length(mu)
  Q <- chol(sigma)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z %*% Q + matrix(mu, n, d, byrow = TRUE)
  return(X)
}

X <- rmvn.Cholesky(1000, mu, sigma)
plot(X, xlab = "x", ylab="y", pch = 20)

print(colMeans(X))
print(cor(X))

size <- 50
mu <- rnorm(size)
A <- matrix(rnorm(size^2), nrow=size, ncol=size)
qr(A)$rank # check rank is full 
sigma <- t(A) %*% A
all(eigen(sigma)$values > 0)  # check that sigma is positive definite (it should be if A has full rank)
 
# WHich method is faster? 

start.time <- Sys.time()
output.eigen <- replicate(2000, rmvn.eigen(1000, mu, sigma))
Sys.time() - start.time 

start.time <- Sys.time()
output.Cholesky <- replicate(2000, rmvn.Cholesky(1000, mu, sigma))
Sys.time() - start.time 


library(MASS)
library(mvtnorm)

?mvrnorm
?rmvnorm
