library(downloader)
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population in which we are interested. 
# We will study the differences in birth weight between babies born to smoking and non-smoking mothers.
babies <- read.table("babies.txt", header=TRUE)

# First, let's split this into two birth weight datasets: one of birth weights to non-smoking mothers and the other of birth weights to smoking mothers.
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# Now, we can look for the true population difference in means between smoking and non-smoking birth weights.
mean(bwt.nonsmoke)-mean(bwt.smoke) # 8.937666
popsd(bwt.nonsmoke) # 17.38696
popsd(bwt.smoke) # 18.08024


# Q1: Set the seed at 1 and obtain a samples from the non-smoking mothers (dat.ns) of size N=25. 
# Then, without resetting the seed, take a sample of the same size from and smoking mothers (dat.s). 
# Compute the t-statistic (call it tval).
# What is the absolute value of the t-statistic?
set.seed(1)
n <- 25
dat.ns <- sample(bwt.nonsmoke, n)
dat.s <- sample(bwt.smoke, n)
obs_effect <- mean(dat.s) - mean(dat.ns)
se <- sqrt( var(dat.s)/n + var(dat.ns)/n )
tval <- obs_effect/se
abs(tval)
## 1.659325


# Q2: Recall that we summarize our data using a t-statistics because we know that in situations where the null hypothesis is true (what we mean when we say "under the null") and the sample size is relatively large, this t-value will have an approximate standard normal distribution. Because we know the distribution of the t-value under the null, we can quantitatively determine how unusual the observed t-value would be if the null hypothesis were true.
# The standard procedure is to examine the probability a t-statistic that actually does follow the null hypothesis would have larger absolute value than the absolute value of the t-value we just observed -- this is called a two-sided test.
# We have computed these by taking one minus the area under the standard normal curve between -abs(tval) and abs(tval). In R, we can do this by using the pnorm() function, which computes the area under a normal curve from negative infinity up to the value given as its first argument:
# pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval
## 0.09705034

# By reporting only p-values, many scientific publications provide an incomplete story of their findings. As we have mentioned, with very large sample sizes, scientifically insignificant differences between two groups can lead to small p-values. Confidence intervals are more informative as they include the estimate itself.



