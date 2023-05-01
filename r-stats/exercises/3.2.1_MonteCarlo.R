library(rafalib)

# Q1: Imagine you are William Sealy Gosset and have just mathematically derived the distribution of the t-statistic when the sample comes from a normal distribution. Unlike Gosset, you have access to computers and can use them to check the results. Let's start by creating an outcome.
# Set the seed at 1, then use rnorm() to generate a random sample of size 5, X1... X5, from a standard normal distribution, 
# then compute the t-statistic root(5)Xbar/s  with s the sample standard deviation. 
# What value do you observe?
set.seed(1)
rnd_5sample <- rnorm(5)
s <- sd(rnd_5sample)
tstat <- sqrt(5)*mean(rnd_5sample)/s
tstat  
## 0.3007746


# Q2: You have just performed a Monte Carlo simulation using rnorm(), a random number generator for normally distributed data. Gosset's mathematical calculation tells us that the t-statistic defined in the previous exercises, a random variable, follows a t-distribution with N-1 degrees of freedom. Monte Carlo simulations can be used to check the theory: we generate many outcomes and compare them to the theoretical result. 
# Set the seed to 1, then generate B=1000 t-statistics as done in exercise 1. 
# What proportion is larger than 2?
set.seed(1)

tstat <- function(n) {
  rnd_sample <- rnorm(n)
  s <- sd(rnd_sample)
  tstat <- sqrt(n)*mean(rnd_sample)/s
  tstat  
}
B <- 1000

tstats <- replicate(B, tstat(5))
mean(tstats>2)
## 0.068



# Q3: The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). We can check several such quantiles using the qqplot function.
# To obtain quantiles for the t-distribution we can generate percentiles from just above 0 to just below 1: B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B), and 
# compute the quantiles with qt(ps,df=4). Now we can use qqplot() to compare these theoretical quantiles to those obtained in the Monte Carlo simulation. 
# Use Monte Carlo simulation developed for exercise 2 to corroborate that the t-statistic t= rt(N)Xbar/s follows a t-distribution for 
# several values of N (try Ns <- seq(5,30,5)).
1-pt(2,df=4) # 0.05805826 compared to 0.068 from above

set.seed(1)

B <- 100
Ns <- seq(5,30,5)
ps <- seq(1/(B+1), 1-1/(B+1),len=B)

tstat <- function(n) {
  rnd_sample <- rnorm(n)
  s <- sd(rnd_sample)
  tstat <- sqrt(n)*mean(rnd_sample)/s
  tstat  
}

par(mfrow=c(3,2))
LIM <- c(-4.5,4.5)

for (i in Ns) {
  quantiles <- qt(ps,df=i-1)
  tstats <- replicate(B, tstat(i))
  
  qqplot(quantiles, tstats, main=i,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}
## The approximations are spot on for all sample sizes.
# Their code:
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 



# Q4: Use Monte Carlo simulation to corroborate that the t-statistic comparing two means and obtained with normally distributed (mean 0 and sd) data follows a t-distribution. 
# In this case we will use the t.test() function with var.equal=TRUE. 
# With this argument the degrees of freedom will be df=2*N-2 with N the sample size. 
# For which sample sizes does the approximation best work?
set.seed(1)

B <- 100
Ns <- seq(5,30,5)
ps <- seq(1/(B+1), 1-1/(B+1),len=B)

tstat <- function(n) {
  x <- rnorm(n)
  y <- rnorm(n)
  ttest <- t.test(x, y, df=2*n-2, var.equal = TRUE)
  ttest$statistic
}

par(mfrow=c(3,2))
LIM <- c(-4.5,4.5)

for (i in Ns) {
  quantiles <- qt(ps,df=2*i-2)
  tstats <- replicate(B, tstat(i))
  
  qqplot(quantiles, tstats, main=i,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}
## The approximations are spot on for all sample sizes.


# Q5: Is the following statement true or false? If instead of generating the sample with X=rnorm(15) 
# we generate it with binary data (either positive or negative 1 with probability 0.5) X =sample(c(-1,1), 15, replace=TRUE) 
# then the t-statistic tstat <- sqrt(15)*mean(X) / sd(X) is approximated by a t-distribution with 14 degrees of freedom.
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 

par(mfrow=c(1,1))
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
## Answer says False. This could be because data is not linearly following abline, but rather duplicates in horizontal step pattern

# Q6: Is the following statement true or false ? 
# If instead of generating the sample with X=rnorm(N) with N=1000, 
# we generate the data with binary data X= sample(c(-1,1), N, replace=TRUE), 
# then the t-statistic sqrt(N)*mean(X)/sd(X) is approximated by a t-distribution with 999 degrees of freedom.
set.seed(1)
N <-  1000
B <- 10000
tstat <- function(n) {
  X <- sample(c(-1,1), n, replace=TRUE)
  t <- sqrt(n)*mean(X)/sd(X)
  t
}

tstats <- replicate(B, tstat(N))
ps <- seq(1/(B+1), 1-1/(B+1), len=B)
quantiles <- qt(ps,N-1)

par(mfrow=c(1,1))
qqplot(quantiles, tstats, xlim=range(tstats))
abline(0,1)
## follows line very smoothly, a little bit off at ends but not much => True
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.


# Q7: We can derive approximation of the distribution of the sample average or the t-statistic theoretically. However, suppose we are interested in the distribution of a statistic for which a theoretical approximation is not immediately obvious.
# Consider the sample median as an example. Use a Monte Carlo to determine which of the following best approximates the median of a sample taken from normally distributed population with mean 0 and standard deviation 1.
# A) Just like for the average, the sample median is approximately normal with mean 0 and SD 1/rt(N).
# B) The sample median is not approximately normal.
# C) The sample median is t-distributed for small samples and normally distributed for large ones.
# D) The sample median is approximately normal with mean 0 and SD larger than 1/rt(N).

B <- 10000
Ns <- seq(5, 30, 5)
ps <- seq(1/(B+1), 1-1/(B+1), len=B)

set.seed(1)
par(mfrow=c(2,3))

for (n in Ns) {
  quantiles <- qt(ps, n-1)
  medians <- replicate(B, {
    X <- rnorm(n)
    median(X)
  })
  title = paste("N=", n, ", avg=", round( mean(medians), 2 ), ", sd*sqrt(n)=", round( sd(medians)*sqrt(N),2))
  qqnorm(medians, main = title)
  qqline(medians)
}

## graph shows that it does follow the normal but when you multiply sd*sqrt(n) you get bigger than 1: The sample median is approximately normal with mean 0 and SD larger than 1/sqrt(n).

# Their code:
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}





