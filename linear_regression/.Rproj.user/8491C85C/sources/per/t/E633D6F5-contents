
# 3) Imagine you have two teams. 
# Team A is comprised of batters who, on average, get two bases on balls, four singles, one double, no triples, and one home run. 
# Team B is comprised of batters who, on average, get one base on balls, six singles, two doubles, one triple, and no home runs.
# (For convenience, the coefficients for the model are as follows: BB 0.371, singles 0.519, doubles 0.771, triples 1.24, and home runs 1.44.)
# Which team scores more runs, as predicted by our model?
# 0.371b + 0.519s + 0.771d + 1.24t + 1.44hr
A <- 0.371*2 + 0.519*4 + 0.771*1 + 1.24*0 + 1.44*1
B <- 0.371*1 + 0.519*6 + 0.771*2 + 1.24*1 + 1.44*0
A # 5.03
B # 6.27
## Team B


# Use the Teams data frame from the Lahman package. 
# Fit a multivariate linear regression model to obtain the effects of BB and HR on Runs (R) in 1971. 
# Use the tidy() function in the broom package to obtain the results in a data frame.
library(Lahman)
library(broom)

fit <- Teams %>% 
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit)

# 9a) What is the estimate for the effect of BB on runs?
tidy(fit) %>%
  filter(term == "BB") %>%
  pull(estimate)
## 0.414
# What is the estimate for the effect of HR on runs?
tidy(fit) %>%
  filter(term == "HR") %>%
  pull(estimate)
## 1.30 

# 9b) Interpret the p-values for the estimates using a cutoff of 0.05 and considering the year 1971 as a sample to make inference on the population of all baseball games across years.
# Which of the following is the correct interpretation?
BB: 0.0625 
HR: 0.00673
## HR has a significant effect on runs, but the evidence is not strong enough to suggest BB also does.

# 10) Repeat the above exercise to find the effects of BB and HR on runs (R) for every year from 1961 to 2018 using summarize() and the broom package.
fit2 <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit2)
# Make a scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals.
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = across()))) %>%
  ungroup() 
res

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")
## The effect of BB on runs has increased

res

# 11) Fit a linear model on the results from Question 10 to determine the effect of year on the impact of BB. 
# That is, determine how the estimated coefficients of BB from the models in Question 10 can be predicted by the year (recall that we grouped the data by year before fitting the models, so we have different estimated coefficients for each year).
# For each additional year, by what value does the impact of BB on runs change?
res %>% 
  filter(term == "BB") %>%
  summarize(tidy(lm(estimate ~ yearID)))

##0.00355 

# What is the p-value for this effect?
## 0.00806617336696384

# Their code:
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate, p.value)






