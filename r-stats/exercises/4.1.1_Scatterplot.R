library(dplyr)

data(nym.2002, package="UsingR")
head(nym.2002)

# Q1: Use dplyr to create two new data frames: males and females, with the data for each gender. 
# For males, what is the Pearson correlation between age and time to finish?
males <- filter(nym.2002, gender=="Male")
females <- filter(nym.2002, gender=="Female")

par(mfrow=c(1,1))
plot(males$age, males$time)

cor(males$age, males$time, method="pearson") 
## 0.2432273


# Q2: For females, what is the Pearson correlation between age and time to finish?
par(mfrow=c(1,1))
plot(females$age, females$time)

cor(females$age, females$time, method="pearson") 
## 0.2443156


# Q3: If we interpret these correlations without visualizing the data, we would conclude that the older we get, the slower we run marathons, regardless of gender. 
# Look at scatterplots and boxplots of times stratified by age groups (20-25, 25-30, etc..).
par(mfrow=c(2,2))
boxplot( split( females$time, seq( min( females$age), max( females$age), 5) ), main="Female" )
boxplot( split( males$time, seq( min( males$age), max( males$age), 5) ), main="Male" )

plot(females$age, females$time, main="Female")
plot(males$age, males$time, main="Male")
# After examining the data, what is a more reasonable conclusion?
## Finish times are constant up through around 50-60, then they get slower.

# Their code:
library(rafalib)
mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)
