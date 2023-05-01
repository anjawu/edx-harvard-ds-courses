# contains weight of chicks in grams as they grow from day 0 to day 21. 
data(ChickWeight)

# This dataset also splits up the chicks by different protein diets, which are coded from 1 to 4. 
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# We use this dataset to also show an important operation in R (not related to robust summaries): reshape.
# the rows here represent time points rather than individuals. To facilitate the comparison of weights at different time points and across the different chicks, we will reshape the data so that each row is a chick.
#reshape the data from _long_ to _wide_, where the columns Chick and Diet are the ID's and the column Time indicates different observations for each ID. 
chick <- reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time", direction="wide")
head(chick)

# remove nulls values chicks:
chick <- na.omit(chick)

# Q1: Save the weights of the chicks on day 4 from diet 1 as a vector x. 
x <- filter(chick, Diet == "1") %>% select(weight.4) %>% unlist

# Save the weights of the chicks on day 4 from diet 4 as a vector y. 
y <- filter(chick, Diet == "4") %>% select(weight.4) %>% unlist

# Perform a t-test comparing x and y (in R the function t.test(x,y) will perform the test). 
t.test(x,y) # t = -5.8393, df = 21.827, p-value = 7.32e-06

# Then perform a Wilcoxon test of x and y (in R the function wilcox.test(x,y) will perform the test). A warning will appear that an exact p-value cannot be calculated with ties, so an approximation is used, which is fine for our purposes.
wilcox.test(x,y) # W = 6, p-value = 0.0002012

# Perform a t-test of x and y, after adding a single chick of weight 200 grams to x (the diet 1 chicks). 
x_new <- c(x, 200)

# What is the p-value from this test? The p-value of a test is available with the following code: t.test(x,y)$p.value
t.test(x_new,y)$p.value # 0.9380347
## 0.9380347
# their code:
x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
t.test(c(x, 200), y)$p.value


# Q2: Do the same for the Wilcoxon test. The Wilcoxon test is robust to the outlier. In addition, it has less assumptions that the t-test on the distribution of the underlying data.
wilcox.test(x_new,y, exact=FALSE)$p.value # exact = False because an error shows up and it cannot compute an exact pvalue
##  0.0009840921


# Q3: We will now investigate a possible downside to the Wilcoxon-Mann-Whitney test statistic. 
# Using the following code to make three boxplots, showing the true Diet 1 vs 4 weights, and then two altered versions: one with an additional difference of 10 grams and one with an additional difference of 100 grams. 
# Use the x and y as defined above, NOT the ones with the added outlier.
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

# What is the difference in t-test statistic (obtained by t.test(x,y)$statistic) between adding 10 and adding 100 to all the values in the group y? 
# Take the the t-test statistic with x and y+10 and subtract the t-test statistic with x and y+100. The value should be positive.
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic 
## 67.75097









