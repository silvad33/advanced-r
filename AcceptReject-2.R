n <- 10000 # sample size
j <- 0 # counts considered draws
k <- 0 # counts accepted draws
x <- rep(NA, length=n) 

while(k < n){
u <- runif(1)	
j <- j + 1
y <- runif(1)
if( u < 4 * y * (1 - y)){
k <- k + 1	
x[k] <- y	
}	
}

c = 3 / 2 # Recall the formulas that define the algorithm 

cat(paste("The total number of candidates is", j)) 
cat(paste("The total number of accepted draws is", k)) 
cat(paste("The observed acceptance rate is", k / j)) 
cat(paste("The theoretical acceptance rate is", 1 / c)) 


hist(x, prob=TRUE)
# Plot density function
t <- seq(from=0, to=1, by=0.01)
dens <- 6 * t * (1 - t)
lines(t, dens, lty=1)

p <- seq(from=0.10, to=0.90, by=0.1)
p
# Compare quantiles
Qhat <- quantile(x, p)
Qhat
Q <- qbeta(p, 2, 2)
rbind(Qhat, Q)
# Calculate standard error
se <- sqrt(p * (1 - p) / (n * dbeta(Q, 2, 2)^2) )  # book has a typo
round(rbind(Qhat, Q, se), digits=3)
