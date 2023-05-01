# http://genomicsclass.github.io/book/pages/permutation_tests.html

library(downloader)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)

babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist


# Q1: We will generate the following random variable based on a sample size of 10 and observe the following difference:
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers) # -13.3

# The question is whether this observed difference is statistically significant. We do not want to rely on the assumptions needed for the normal or t-distribution approximations to hold, so instead we will use permutations. We will reshuffle the data and recompute the mean. We can create one permuted sample with the following code:
dat <- c(smokers, nonsmokers) # puts together all bw for both groups into one vector
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar) # -9.5

# The last value is one observation from the null distribution we will construct. 
# Set the seed at 1, and then repeat the permutation 1,000 times to create a null distribution. 
# What is the permutation derived p-value for our observation?
set.seed(1)
B <- 1000
n <- 10

null_mean <- function(n) {
  dat <- c(smokers, nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:n]
  nonsmokersstar <- shuffle[(n+1):(2*n)]
  mean(smokersstar)-mean(nonsmokersstar)
}

null_distribution <- replicate(B, null_mean(n))
mean( abs(null_distribution) >= abs(obs) ) 
## 0.116

# Their answer:
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )


# Q2: Repeat the above exercise, but instead of the differences in mean, consider the differences in median obs <- median(smokers) - median(nonsmokers). 
# What is the permutation based p-value?
set.seed(1)
B <- 1000
n <- 10

# Observed:
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs_median <- median(smokers) - median(nonsmokers)

# Null:
null_median <- function(n) {
  dat <- c(smokers, nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:n]
  nonsmokersstar <- shuffle[(n+1):(2*n)]
  median(smokersstar)-median(nonsmokersstar)
}
null_distribution <- replicate(B, null_median(n))

# pvalue:
mean( abs(null_distribution) >= abs(obs_median) ) 
## 0.286






