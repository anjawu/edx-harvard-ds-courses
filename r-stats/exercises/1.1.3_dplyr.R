install.packages("dplyr")

library(downloader)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url, filename)

# Q1: Read in the msleep_ggplot2.csv file with the function read.csv() and 
# use the function class() to determine what type of object is returned.
msleep_df <- read.csv(filename)
# View(msleep_df) 
class(msleep_df)
## data.frame


# Q2: Now use the filter() function to select only the primates.
# How many animals in the table are primates?
head(msleep_df, 2)
primate <- filter(msleep_df, order == "Primates")
View(primate)
nrow(primate)
## 12


# Q3: What is the class of the object you obtain after subsetting the table to only include primates?
class(primate)
## data.frame

# Q4: Now use the select() function to extract the sleep (total) for the primates.
primate_sleep <- select(primate, sleep_total)
# OR using piping:
primate_sleep <- filter(msleep_df, order == "Primates") %>% select(sleep_total)
class(primate_sleep)
## data.frame


# Q5: Now we want to calculate the average amount of sleep for primates (the average of the numbers computed above). One challenge is that the mean() function requires a vector so, if we simply apply it to the output above, we get an error. Look at the help file for unlist() and use it to compute the desired average.
# What is the average amount of sleep for primates?
primate_sleep_list <- filter(msleep_df, order == "Primates") %>% select(sleep_total) %>% unlist
mean(primate_sleep_list)
## 10.5


# Q6: For the last exercise, we could also use the dplyr summarize() function. We have not introduced this function, but you can read the help file and repeat exercise 5, this time using just filter() and summarize() to get the answer.
# What is the average amount of sleep for primates calculated by summarize()
primate_sleep_mean <- filter(msleep_df, order == "Primates") %>% summarize(mean(sleep_total))
primate_sleep_mean 
##  mean(sleep_total)
# 1              10.5
class(primate_sleep_mean) #data.frame
