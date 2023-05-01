library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)


# Q1:
# The CLT is a result from probability theory. Much of probability theory was originally inspired by gambling. This theory is still used in practice by casinos. For example, they can estimate how many people need to play slots for there to be a 99.9999% probability of earning enough money to cover expenses. Let's try a simple example related to gambling.
# Suppose we are interested in the proportion of times we see a 6 when rolling n=100 dice. This is a random variable which we can simulate with x=sample(1:6, n, replace=TRUE) and the proportion we are interested in can be expressed as an average: mean(x==6). Because the die rolls are independent, the CLT applies.
# We want to roll n dice 10,000 times and keep these proportions. This random variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. So according to the CLT, z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1.
# Set the seed to 1, then use replicate() to perform the simulation, and report what proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05).
set.seed(1)
p <- 1/6
n <- 100
var <- p*(1-p)/n

die_100 <- replicate(10000, mean(sample(1:6, n, replace=TRUE)==6))

zs <- (die_100 - p) / sqrt(var)
qqnorm(zs)
abline(0,1) # confirm it's well approximated with normal distribution

mean(abs(zs)>2)
## 0.0431
# Their answer with code:
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)


# Q2:For the last simulation you can make a qqplot to confirm the normal approximation. Now, the CLT is an asymptotic result, meaning it is closer and closer to being a perfect approximation as the sample size increases. In practice, however, we need to decide if it is appropriate for actual sample sizes. Is 10 enough? 15? 30?
# In the example used in exercise 1, the original data is binary (either 6 or not). In this case, the success probability also affects the appropriateness of the CLT. With very low probabilities, we need larger sample sizes for the CLT to "kick in".
# Run the simulation from exercise 1, but for different values of p and n. For which of the following is the normal approximation best?
# p=0.5 and n=5 ## Not a good appoximation according to qq plot; 0.4001
# p=0.5 and n=30 ## Closer to having a trend but not close to normal; 0.98
# p=0.01 and n=30 ## has trend but far away from norm; 0.9691
# p=0.01 and n=100 ## 1
set.seed(1)
p <- 0.01
n <- 100
var <- p*(1-p)/n

die_100 <- replicate(10000, mean(sample(1:6, n, replace=TRUE)==6))

zs <- (die_100 - p) / sqrt(var)
qqnorm(zs)
abline(0,1) # confirm it's well approximated with normal distribution

mean(abs(zs)>2)
## p=0.5 and n=30 
# They changed the number of sides on their die and did a histplot, their code:
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}



# Q3:As we have already seen, the CLT also applies to averages of quantitative data. A major difference with binary data, for which we know the variance is p(1-p), is that with quantitative data we need to estimate the population standard deviation.
# In several previous exercises we have illustrated statistical concepts with the unrealistic situation of having access to the entire population. In practice, we do *not* have access to entire populations. Instead, we obtain one random sample and need to reach conclusions analyzing that data. dat is an example of a typical simple dataset representing just one sample. We have 12 measurements for each of two populations:
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
# We think of X as a random sample from the population of all mice in the control diet and Y as a random sample from the population of all mice in the high fat diet.
# Define the parameter mux as the average of the control population. We estimate this parameter with the sample average Xbar. What is the sample average?
mean(X)
## 23.81333

# Q4: Which of the following uses CLT to understand how well Xbar approximates mux?
##  Xbar follows a normal distribution with mean mux and standard deviation sigmax/root12 where sigmax is the population standard deviation.


# Q5:The result above tells us the distribution of the following random variable: Z = root(12)(xbar-mux)/sigmax. 
# What does the CLT tell us is the mean of Z (you don't need code)?
## 0

# Q6:The result of 4 and 5 tell us that we know the distribution of the difference between our estimate and what we want to estimate, but don't know. However, the equation involves the population standard deviation sigmax, which we don't know.
# Given what we discussed, what is your estimate of sigmax?
# Hint: While the popsd() function from rafalib calculates population standard deviations, the sd() function in base R calculates sample standard deviations.
sd(X)


# Q7: Use the CLT to approximate the probability that our estimate Xbar is off by more than 2 grams from mux.
# P(|Xbar - mux| > 2) ... P(|(Xbar-mux)/sigma/root(n)| > 2/sigma/root(n))
n <- length(X)
sigma <- sd(X)
boundary <- 2/(sigma/sqrt(n))
2*(1-pnorm(boundary))

# Q8:Now we introduce the concept of a null hypothesis. We don't know mux nor muy. We want to quantify what the data say about the possibility that the diet has no effect: mux=muy. If we use CLT, then we approximate the distribution of Xbar as normal with mean mux and standard deviation sigmax/root(M) and the distribution of Ybar as normal with mean muy and standard deviation sigmay/root(N), with M and N the sample sizes for X and Y respectively, in this case 12. This implies that the difference Ybar - Xbar  has mean 0. We described that the standard deviation of this statistic (the standard error) is SE(Xbar-Ybar) = sqrt( sd(X)^2/12 + sd(Y)^2/12 ) and that we estimate the population standard deviations sigmax and sigmay with the sample estimates.
# What is the estimate of SE(Xbar - Ybar) ?
SE <- sqrt( sd(X)^2/12 + sd(Y)^2/12 )
SE

# Q9:So now we can compute Ybar-Xbar as well as an estimate of this standard error and construct a t-statistic. What number is this t-statistic?
t.test(Y, X)
# OR:
( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
## 2.055174
# You will need to remember that the t-distribution is centered at 0 and has one parameter: the degrees of freedom, that control the size of the tails. You will notice that if X follows a t-distribution the probability that X is smaller than an extreme value such as 3 SDs away from the mean grows with the degrees of freedom. 
# For example, notice the difference between:
1 - pt(3,df=3) # 0.02883444
1 - pt(3,df=15) # 0.004486369
1 - pt(3,df=30) # 0.002694982
1 - pnorm(3) # 0.001349898
# As we explained, under certain assumptions, the t-statistic follows a t-distribution. Determining the degrees of freedom can sometimes be cumbersome, but the t.test function calculates it for you. One important fact to keep in mind is that the degrees of freedom are directly related to the sample size. There are various resources for learning more about degrees of freedom on the internet as well as statistics books.
# http://genomicsclass.github.io/book/pages/clt_and_t-distribution.html

# Q10: If we apply the CLT, what is the distribution of this t-statistic?
# http://genomicsclass.github.io/book/pages/t-tests_in_practice.html
## CLT tells us that tstat is approximately normal with mean 0 (the null hypothesis) and SD 1 (we divided by its SE).



# Q11: Now we are ready to compute a p-value using the CLT. What is the probability of observing a quantity as large as what we computed in 9, when the null distribution is true?
control <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

obs_effect <- mean(treatment) - mean(control) # observed effect size.
SE <- sqrt(var(treatment)/12 + var(control)/12)
tstat <- obs_effect/SE
right_tail <- 1 - pnorm(abs(tstat))
left_tail <- pnorm(-abs(tstat))
pval<- right_tail + left_tail
pval 
## 0.0398622
# OR: Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
#     2*( 1-pnorm(Z)) 


# Q12: CLT provides an approximation for cases in which the sample size is large. In practice, we can't check the assumption because we only get to see 1 outcome (which you computed above). As a result, if this approximation is off, so is our p-value. As described earlier, there is another approach that does not require a large sample size, but rather that the distribution of the population is approximately normal. We don't get to see this distribution so it is again an assumption, although we can look at the distribution of the sample with qqnorm(X) and qqnorm(Y). If we are willing to assume this, then it follows that the t-statistic follows the t-distribution.
# What is the p-value under the t-distribution approximation?
# Hint: use the t.test() function.
mypar(1,2)
qqnorm(X)
qqline(X) # close to normal
qqnorm(Y)
qqline(Y) # close to normal
t.test(Y, X)
# OR: t.test(X,Y)$p.value


# Q13: With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, one that is larger. They can't both be right. What best describes the difference?
# These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, under the null, large values are more probable under the null distribution.







