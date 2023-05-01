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

# Q1: Focus on the chick weights on day 4 (check the column names of chick and note the numbers). 
# How much does the average of chick weights at day 4 increase if we add an outlier measurement of 3000 grams? 
# Specifically, what is the average weight of the day 4 chicks, including the outlier chick, divided by the average of the weight of the day 4 chicks without the outlier. 
# Hint: use c() to add a number to a vector.
View(chick)
day4_chick <- select(chick, weight.4) %>% unlist
reg_avg <- mean(day4_chick) # 60.15556

outlier_chick <- c(day4_chick, 3000)
outlier_avg <- mean(outlier_chick)
outlier_avg # 124.0652

outlier_avg/reg_avg 
## 2.062407
# Their code:
mean(c(chick$weight.4, 3000))/mean(chick$weight.4)


# Q2: In exercise 1, we saw how sensitive the mean is to outliers. Now let's see what happens when we use the median instead of the mean. 
# Compute the same ratio, but now using median instead of mean. 
# Specifically, what is the median weight of the day 4 chicks, including the outlier chick, divided by the median of the weight of the day 4 chicks without the outlier.
reg_median <- median(day4_chick)
outlier_median <- median(outlier_chick)
outlier_median/reg_median 
## 1

# Q3: Now try the same thing with the sample standard deviation (the sd() function in R). 
# Add a chick with weight 3000 grams to the chick weights from day 4. 
# How much does the standard deviation change? 
# What's the standard deviation with the outlier chick divided by the standard deviation without the outlier chick?
reg_sd <- sd(day4_chick)
outlier_sd <- sd(outlier_chick)
outlier_sd/reg_sd 
## 101.2859
  

# Q4: Compare the result above to the median absolute deviation in R, which is calculated with the mad() function. Note that the MAD is unaffected by the addition of a single outlier. 
# The mad() function in R includes the scaling factor 1.4826, such that mad() and sd() are very similar for a sample from a normal distribution.
# What's the MAD with the outlier chick divided by the MAD without the outlier chick?
reg_mad <- mad(day4_chick)
outlier_mad <- mad(outlier_chick)
outlier_mad/reg_mad 
## 1
  
  
# Q5: Our last question relates to how the Pearson correlation is affected by an outlier as compared to the Spearman correlation. The Pearson correlation between x and y is given in R by cor(x,y). The Spearman correlation is given by cor(x,y,method="spearman").
# Plot the weights of chicks from day 4 and day 21. We can see that there is some general trend, with the lower weight chicks on day 4 having low weight again on day 21, and likewise for the high weight chicks.
plot(chick$weight.4, chick$weight.21)

# Calculate the Pearson correlation of the weights of chicks from day 4 and day 21. 
reg_pcor <- cor(chick$weight.4, chick$weight.21, method="pearson") #0.4159499
# Now calculate how much the Pearson correlation changes if we add a chick that weighs 3000 on day 4 and 3000 on day 21. Again, divide the Pearson correlation with the outlier chick over the Pearson correlation computed without the outliers.
outlier_pcor <- cor(c(chick$weight.4, 3000), c(chick$weight.21, 3000)) # 0.9861002
outlier_pcor/reg_pcor 
## 2.370719
  
# Spearman
reg_scor <- cor(chick$weight.4, chick$weight.21, method="spearman") # 0.4303941
# Now calculate how much the Pearson correlation changes if we add a chick that weighs 3000 on day 4 and 3000 on day 21. Again, divide the Pearson correlation with the outlier chick over the Pearson correlation computed without the outliers.
outlier_scor <- cor(c(chick$weight.4, 3000), c(chick$weight.21, 3000), method="spearman") # 0.4669028
outlier_scor/reg_scor 
## 1.084826
