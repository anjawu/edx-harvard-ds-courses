# R has the data set InsectSprays
head(InsectSprays)
View(InsectSprays)

boxplot(InsectSprays$count ~ InsectSprays$spray, xlab = "Spray Label", ylab = "Insect Count After Treatment", main = "Insecticide Sprays vs Number of Remaining Insects")
#boxplot(split(InsectSprays$count, InsectSprays$spray))

# Q1: Which spray seems the most effective (has the lowest median count)?
## C

# Q2: Which are the groups with visible outliers?
## C, D

# Q3: Let's consider a random sample of finishers from the New York City Marathon in 2002. This dataset can be found in the UsingR package. Load the library and then load the nym.2002 dataset.
library(dplyr)
install.packages("UsingR")
library(UsingR)
data(nym.2002, package="UsingR")
# Use boxplots and histograms to compare the finishing times of males and females. 
# Which of the following best describes the difference?
head(nym.2002)

# filter by gender, for some reason select() wont work:
male <- filter(nym.2002, gender == "Male")
female <- filter(nym.2002, gender == "Female")

# create boxplot for both
par(mfrow=c(1,1))
boxplot(male$time, female$time)

# create histograms
par(mfrow=c(1,2))
hist(male$time, main = "Male Times", xlim=c(range(nym.2002$time)))
hist(female$time, main = "Female Times", xlim=c(range(nym.2002$time)))

mean(unlist(male$time))
mean(unlist(female$time))
