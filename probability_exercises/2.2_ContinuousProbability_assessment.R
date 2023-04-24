# The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all involve simulating some ACT test scores and answering probability questions about them.
# For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values instead.)

# Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. 
# Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.

act_mean <- 20.9
act_sd <- 5.7
set.seed(16)
act_scores <- rnorm(10000, act_mean, act_sd)

# 1) What is the mean of act_scores?
actscores_mean <- mean(act_scores)
## 20.84012

# 2) What is the standard deviation of act_scores?
actscores_sd <- sd(act_scores)
## 5.675237

# 3) A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# 4) In act_scores, what is the probability of an ACT score greater than 30?
1- pnorm(30, actscores_mean, actscores_sd)
## 0.05326283
# Their answer:
mean(act_scores > 30)
## 0.0527

# 5) In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <=10)
## 0.0282

# 6) Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
# Which of the following plots is correct?
x <- seq(1,36)
f_x <- dnorm(x, act_mean, act_sd)
plot(x, f_x)
## returns normal curve (slight left skew)
# Their answer:
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()







# In this 3-part question, you will convert raw ACT scores to Z-scores and answer some questions about them.
# Convert act_scores to Z-scores. Recall from Data Visualization (the second course in this series) that to standardize values (convert values into Z-scores, that is, values distributed with a mean of 0 and standard deviation of 1), 
# you must subtract the mean and then divide by the standard deviation. Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.
z_act_scores <- sapply(act_scores, function(a) (a-actscores_mean)/actscores_sd)

# 1) What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
1- pnorm(2, mean(z_act_scores), sd(z_act_scores)) 
# OR:
mean(z_act_scores > 2)
## 0.02275013

# 2) What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*actscores_sd + actscores_mean
## 32.1906

# 3) A Z-score of 2 corresponds roughly to the 97.5th percentile.
# Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(0.975, actscores_mean, actscores_sd)
## 31.96338


# Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). 
# Apply this function to the range 1 to 36.
x <- seq(1:36)
F_a <- function(a) mean(x>=a)

# 4) What is the minimum integer score such that the probability of that score or lower is at least .95?
# Your answer should be an integer 1-36.
qnorm(0.95, actscores_mean, actscores_sd)
## 31
# Their answer:
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
plot(cdf)
min(which(cdf >= .95))

# 5) Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
# What is the expected 95th percentile of ACT scores?
qnorm(0.95, 20.9, 5.7)
## 30.27567


# 6) As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
# In what percentile is a score of 26?
# Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th and 99th percentile should be considered the 98th percentile, for example, and that quantile numbers are used as names for the vector sample_quantiles.
sample_quantiles 
## 82
# Their answer:
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# 7) Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles.
# Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
# Which of the following graphs is correct?
library(ggplot2)
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, actscores_mean, actscores_sd)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()





