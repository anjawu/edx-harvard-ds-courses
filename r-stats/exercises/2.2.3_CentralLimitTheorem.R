library(downloader) 
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)

dat <- na.omit( read.csv(filename) )


# Q1: If a list of numbers has a distribution that is well approximated by the normal distribution, 
# what proportion of these numbers are within one standard deviation away from the list's average?
pnorm(1)-pnorm(-1)
## 0.6826895


# Q2: What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(2)-pnorm(-2)
## 0.9544997


# Q3: What proportion of these numbers are within three standard deviations away from the list's average?
pnorm(3) - pnorm(-3)
## 0.9973002


# Q4: Define y to be the weights of males on the control diet.
# What proportion of the mice are within one standard deviation away from the average weight?
head(dat)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
hist(y)
mu <- mean(y) # 30.96381
sd(y) # 4.430445
pop_sigma <- popsd(y) # 4.420501

upper <- mean(y) + pop_sigma # 38.11788
lower <- mean(y) - pop_sigma # 27.41375
upper2 <- mean(y) + 2*pop_sigma 
lower2 <- mean(y) - 2*pop_sigma 
upper3 <- mean(y) + 3*pop_sigma 
lower3 <- mean(y) - 3*pop_sigma 

pnorm(upper, mu, pop_sigma) - pnorm(lower, mu, pop_sigma)  # 0.6826895

z <- (y-mu)/pop_sigma
mean(abs(z)<=1) 
## 0.6950673


# Q5: What proportion of these numbers are within two standard deviations away from the list's average?
pnorm(upper2, mu, pop_sigma) - pnorm(lower2, mu, pop_sigma) # 0.9544997

mean( abs(z) <=2) # 0.9461883

## 0.9544997 I don't know why. The answer said mean( abs(z) <=2) was the method to get 0.9544997


# Q6: What proportion of these numbers are within three standard deviations away from the list's average?
mean( abs(z) <=3) # 0.9910314
pnorm(upper3, mu, pop_sigma) - pnorm(lower3, mu, pop_sigma) # 0.9973002
## 0.9927885


# Q7: Note that the numbers for the normal distribution and our weights are relatively close. Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles of the mouse weight distribution. We can actually compare all quantiles using a qqplot.
qqnorm(z)
abline(0,1)
# Which of the following best describes the qq-plot comparing mouse weights to the normal distribution?
# The mouse weights are well approximated by the normal distribution, although the larger values (right tail) are larger than predicted by the normal. This is consistent with the differences seen between question 3 and 6.




# Q8: Here we are going to use the function replicate() to learn about the distribution of random variables. All the above exercises relate to the normal distribution as an approximation of the distribution of a fixed list of numbers or a population. We have not yet discussed probability in these exercises. If the distribution of a list of numbers is approximately normal, then if we pick a number at random from this distribution, it will follow a normal distribution. However, it is important to remember that stating that some quantity has a distribution does not necessarily imply this quantity is random. Also, keep in mind that this is not related to the central limit theorem. The central limit applies to averages of random variables. Let's explore this concept.
# We will now take a sample of size 25 from the population of males on the chow diet. The average of this sample is our random variable. 
# We will use the replicate() function to observe 10,000 realizations of this random variable. 
# Set the seed at 1, then generate these 10,000 averages. 
# Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.
y <- filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avg_10000 <- replicate(10000, mean( sample(y, 25) ))
mypar(1,2)
hist(avg_10000)
qqnorm(avg_10000)
qqline(avg_10000)
# We can see that, as predicted by the CLT, the distribution of the random variable is very well approximated by the normal distribution.

# What is the average of the distribution of the sample average?
mean(avg_10000)
## 30.96856


# Q9: What is the standard deviation of the distribution of sample averages (use popsd())?
popsd(avg_10000)
## 0.827082



