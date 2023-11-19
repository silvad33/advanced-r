# Simulate mixture: height for males and females

n <- 20000
gender <- sample(c("M", "F"), size=n, replace=TRUE)
draws <- rep(NA, length=n)
for(i in 1:n){
if(gender[i]=="F"){
draws[i] <- rnorm(1, mean=64, sd=2)	
}
if(gender[i]=="M"){
draws[i] <- rnorm(1, mean=70, sd=2)	
}		
}
hist(draws, breaks=200, prob=TRUE, main="Normal mixture for height", xlab="height (in)")

# Plot density estimate:  plot(density(draws), "Normal mixture for height", xlab="height (in)")

#######################################

# Mixture of 4 normals
n <- 20000
outcome <- sample(1:4, size=n, replace=TRUE)
draws <- rep(NA, length=n)
mu <- c(40, 50, 60, 70)
sigma <- c(2, 2.5, 3, 3.5)

for(i in 1:n){
draws[i] <- rnorm(1, mean=mu[outcome[i]], sd=sigma[outcome[i]] )	
}
hist(draws, breaks=200, prob=TRUE, main="Normal mixture for height", xlab="height (in)")


##########################################

# Poisson-Gamma mixture (see textbook Sec 3.5)
n = 10000
r <- 4 
beta <- 3
draws <- rep(NA, length=n)
lambda <- rgamma(n, r, beta)
for(i in 1:n){
draws[i] <- rpois(1, lambda[i])
}
hist(draws, breaks=0:max(draws), prob=TRUE, right=FALSE)
table(draws)

# Compare sample and theoretical probabilities 

#sample proportions
p.sample <- table(draws)/n  
# theorical probabilities from Negative Binomal 
p.nbinom <- round(dnbinom(0:max(draws), r, beta / (1 + beta)), digits=3) 
# calculate SE
se <- sqrt(p.nbinom * (1-p.nbinom) / n) 
# combine these results
results <- rbind(p.sample, p.nbinom, se)
# Round the results
round(results, digits=4)
