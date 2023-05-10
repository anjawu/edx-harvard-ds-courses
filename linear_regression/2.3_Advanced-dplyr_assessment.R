# 5) You want to take the tibble dat, which we used in the video on the advanced dplyr, and run the linear model R ~ BB for each strata of HR. 
# Then you want to add three new columns to your grouped tibble: the coefficient, standard error, and p-value for the BB term in the model.
# You’ve already written the function get_slope(), shown below.
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# As a reminder, the tibble dat is defined as follows:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# What additional code could you write to accomplish your goal?
dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))


# 7) You want to know whether the relationship between home runs and runs per game varies by baseball league. You create the following dataset:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

# What code would help you quickly answer this question?
dat %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR")

# This code is a good application of the command tidy(), from the broom package.
# The glance() function provides data on model fit rather than on effect estimates and confidence intervals. If you forget the line group_by(lgID), your code will give you a single estimate for the entire dataset because you have not grouped the data by league ID.


########################################### PART 2 ###########################################
# The galton dataset is a sample of one male and one female child from each family in the GaltonFamilies dataset. The pair column denotes whether the pair is father and daughter, father and son, mother and daughter, or mother and son.
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton


# 8) Group by pair and summarize the number of observations in each group.
pairs_count <- galton %>%
  group_by(pair) %>%
  count(pair)
pairs_count
# How many father-daughter pairs are in the dataset?
## 176

# How many mother-son pairs are in the dataset?
## 179

# Their code:
galton %>%
  group_by(pair) %>%
  summarize(n = n())


# 9) Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers and daughters and mothers and sons.
cor <- galton %>%
  group_by(pair) %>%
  summarize(cor = cor(childHeight, parentHeight))
# Which pair has the strongest correlation in heights?
cor %>% filter(cor == max(cor))
## father_son 0.430

# Which pair has the weakest correlation in heights?
cor %>% filter(cor == min(cor))
## mother_son 0.343



# Use lm() and the broom package to fit regression lines for each parent-child pair type. 
# Compute the least squares estimates, standard errors, confidence intervals and p-values for the parentHeight coefficient for each pair.


# 10) 
# a) What is the estimate of the father-daughter coefficient?
galton %>%
  group_by(pair) %>%
  filter(pair=="father_daughter") %>%
  summarize(tidy(lm(childHeight ~ parentHeight)))
## 0.345


# For every 1-inch increase in mother's height, how many inches does the typical son's height increase?
galton %>%
  group_by(pair) %>%
  filter(pair=="mother_son") %>%
  summarize(tidy(lm(childHeight ~ parentHeight)))
## 0.381 

# Their code:
galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)

# b) Which sets of parent-child heights are significantly correlated at a p-value cut off of 0.05?
galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>%
  filter(p.value < 0.05) 

## All

# When considering the estimates, which of the following statements are true?
## All of the confidence intervals overlap each other.
## The confidence intervals involving mothers' heights are larger than the confidence intervals involving fathers' heights.
## The data are consistent with inheritance of height being independent of the child's gender.
## The data are consistent with inheritance of height being independent of the parent's gender.


# The std.error values are lower for daughters than sons, resulting in smaller confidence intervals
# To instead compare the effect of parent's gender, for example, we would want to compare the ESTIMATES for father-daughter vs mother-daughter (as well as father-son vs mother-son). If they are very different, this will suggest there is a difference by gender. 
















