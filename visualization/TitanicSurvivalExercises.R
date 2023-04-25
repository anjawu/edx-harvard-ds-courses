install.packages("titanic")
install.packages("tidyverse")

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(dplyr)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# 1) Inspect the data and also use ?titanic_train to learn more about the variables in the dataset. 
# Match these variables from the dataset to their variable type. There is at least one variable of each type (ordinal categorical, non-ordinal categorical, continuous, discrete).
?titanic_train
head(titanic,15)
## Survived non-ordinal categorical
## Pclass ordinal categorical
## Sex non-ordinal categorical
## SibSp discrete
## Parch discrete
## Fare continuous

# 2) Make density plots of age grouped by sex. Try experimenting with combinations of faceting, alpha blending, stacking and using variable counts on the y-axis to answer the following questions. Some questions may be easier to answer with different versions of the density plot.
titanic %>% ggplot(aes(Age, color = Sex)) + geom_density()
## Females and males had the same general shape of age distribution.

titanic %>% ggplot(aes(Age, color = Sex, fill = Sex, y=..count..)) + geom_density(alpha=0.2) + facet_grid(Sex ~.)
## The age distribution was bimodal, with one mode around 25 years of age and a second smaller mode around 5 years of age.

titanic %>% ggplot(aes(Age, color = Sex, fill = Sex, y=..count..)) + geom_density(alpha=0.2)
## The count of males of age 40 was higher than the count of females of age 40.

titanic %>% ggplot(aes(Age, color = Sex)) + geom_density()
## The proportion of males age 18-35 was higher than the proportion of females age 18-35.
## The proportion of females under age 17 was higher than the proportion of males under age 17.

# 3) Use geom_qq() to make a QQ-plot of passenger age and add an identity line with geom_abline(). 
# Filter out any individuals with an age of NA first. Use the following object as the dparams argument in geom_qq():
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
## same graph

# 4) To answer the following questions, make barplots of the Survived and Sex variables using geom_bar(). Try plotting one variable and filling by the other variable. You may want to try the default plot, then try adding position = position_dodge() to geom_bar() to make separate bars for each group.
titanic %>% ggplot(aes(Survived, colour=Sex, fill = Sex)) + geom_bar(position = position_dodge())
## Most of the survivors were female.

titanic %>% ggplot(aes(Survived)) + geom_bar()
## Less than half of passengers survived.

titanic %>% ggplot(aes(Sex, colour=Survived, fill = Survived)) + geom_bar(position = position_dodge())
## Most of the females survived.

# 5) Make a density plot of age filled by survival status. Change the y-axis to count and set alpha = 0.2.
titanic %>% ggplot(aes(Age, color = Survived, fill = Survived, y = ..count..)) + geom_density(alpha = 0.2)
# Which age group is the only group more likely to survive than die?
## 0-8
# Which age group had the most deaths?
## 18-30
titanic %>% ggplot(aes(Age, color = Survived, fill = Survived)) + geom_density(alpha = 0.2)
# Which age group had the highest proportion of deaths?
## 70-80

# 6) Filter the data to remove individuals who paid a fare of 0. 
# Make a boxplot of fare grouped by survival status. Try a log2 transformation of fares. 
# Add the data points with jitter and alpha blending.
# Which of the following are true?
titanic %>% filter(Fare>0) %>% ggplot(aes(Fare, color = Survived)) +
  geom_density() + scale_x_continuous(trans = "log2") 
## Passengers who survived generally payed higher fares than those who did not survive.
titanic %>% filter(Fare>0) %>% ggplot(aes(Fare, color = Survived)) + 
  geom_boxplot() + scale_x_continuous(trans = "log2")
## The median fare was lower for passengers who did not survive.
## Most individuals who paid a fare around $8 did not survive.
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)

# 7) The Pclass variable corresponds to the passenger class. Make three barplots. 
# For the first, make a basic barplot of passenger class filled by survival. 
titanic %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar()
## There were more third class passengers than passengers in the first two classes combined.

# For the second, make the same barplot but use the argument position = position_fill() to show relative proportions in each group instead of counts. 
titanic %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = position_fill())
## Survival proportion was highest for first class passengers, followed by second class. Third-class had the lowest survival proportion.
## Most passengers in first class survived. Most passengers in other classes did not survive.

# For the third, make a barplot of survival filled by passenger class using position = position_fill().
titanic %>% ggplot(aes(Survived, fill =Pclass )) + geom_bar(position = position_fill())
## The majority of those who did not survive were from third class.


# 8) Create a grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class.
titanic %>% ggplot(aes(Age, fill = Survived, y = ..count..)) + 
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ Pclass)
## The largest group of passengers was third-class males.
## Most first-class and second-class females survived.
## Almost all second-class males did not survive, with the exception of children.





