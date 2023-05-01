# We will use the data set called "Gapminder" which is available as an R-package on Github. This data set contains the life expectancy, GDP per capita, and population by country, every five years, from 1952 to 2007. It is an excerpt of a larger and more comprehensive set of data available on Gapminder.org 
install.packages("gapminder")

library(dplyr)
library(gapminder)
data(gapminder)
head(gapminder)
class(gapminder)

# Create a vector x of the life expectancies of each country for the year 1952. 
x <- filter(gapminder, year==1952) %>% select(lifeExp) %>% unlist
x

# Plot a histogram of these life expectancies to see the spread of the different countries.
hist(x)


# Q1: In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.
# We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal to a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.
# The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.
# What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(x<=40)
plot(ecdf(x)) # for fun
## 0.2887324


# practice making a function like plot(ecdf())
prop = function(q) {
  mean(x <= q)
}

prop(40) # gives same answer as above (0.2887324)

qs = seq(from=min(x), to=max(x), length=20)
qs # creates vector with 20 values ranging from minium to maximum x.

props = sapply(qs, prop) # applying the function(q) on the qs vector
plot(qs, props) # this will give us a similar shape to empirical cumulative distribution but with less points

# All can be simplified to this:
props = sapply(qs, function(q) mean(x <= q))
