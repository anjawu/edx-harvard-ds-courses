################################################# Stratification and Variance Explained, Part 2 ################################################# 

# In the second part of this assessment, you'll analyze a set of mother and daughter heights, also from GaltonFamilies.
# Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

head(female_heights)

# 8) Calculate the mean and standard deviation of mothers' heights, the mean and standard deviation of daughters' heights, and the correlaton coefficient between mother and daughter heights.
# Mean of mothers' heights
mu_m <- mean(female_heights$mother)
## 64.125

# Standard deviation of mothers' heights
sigma_m <- sd(female_heights$mother)
## 2.289292

# Mean of daughters' heights
mu_d <- mean(female_heights$daughter)
## 64.28011

# Standard deviation of daughters' heights
sigma_d <- sd(female_heights$daughter)
## 2.39416

# Correlation coefficient
rho <- cor(female_heights$mother, female_heights$daughter)
## 0.3245199

# 9) Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. 
# Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?
# Slope of regression line predicting daughters' height from mothers' heights rho*sigma_d/sigma_m
m <- rho*sigma_d/sigma_m
m
## 0.3393856

# Intercept of regression line predicting daughters' height from mothers' heights mu_d - m*mu_m
b <- mu_d - m*mu_m
b
## 42.51701

# Change in daughter's height in inches given a 1 inch increase in the mother's height
m
## 0.3393856

# 10) What percent of the variability in daughter heights is explained by the mother's height? rho^2 *100
# Report your answer as a value between 0 and 100. Do NOT include the percent symbol (%) in your submission.
rho^2 *100
## 10.53132


# 11) A mother has a height of 60 inches.
# Using the regression formula, what is the conditional expected value of her daughter's height given the mother's height?
x <- 60
m*x +b
## 62.88015




