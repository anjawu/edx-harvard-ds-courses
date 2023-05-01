library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
X <- unlist(read.csv(filename))


# Q1: What is the average of these weights?
X
mean(X)
## 23.89338



# Q2: Set the seed to 1. Take a random sample of size 5. 
# What is the absolute value (use abs()) of the difference between the average of the sample and the average of all the values?
set.seed(1)
x1<-sample(X, 5)
abs(mean(x1)-mean(X))  
## 0.3293778


# Q3: After setting the seed at 5, set.seed(5), take a random sample of size 5. 
# What is the absolute value of the difference between the average of the sample and the average of all the values?
set.seed(5)
x2<-sample(X, 5)
abs(mean(x2)-mean(X))  


# Q4: Why are the answers from 2 and 3 different?
## Because the average of the samples is a random variable.

  