install.packages("rafalib")
library(downloader) 
library(dplyr)
library(rafalib)


url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)

dat <- read.csv(filename) 
# We will remove the lines that contain missing values:
dat <- na.omit( dat )
head(dat)


# Q1: Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
# What is this population's average?
x <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
x_bar <- mean(x)
x_bar
## 30.96381


# Q2: Now use the rafalib package and use the popsd() function to compute the population standard deviation.
popsd(x)
## 4.420501



# Q3: Set the seed at 1. Take a random sample X of size 25 from x.
# What is the sample average?
set.seed(1)
X <- sample(x, 25)
X_bar <- mean(X)
X_bar
## 30.5196


# Q4: Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
# What is this population's average?
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
y_bar <- mean(y)
y_bar
## 34.84793


# Q5: Now use the rafalib package and use the popsd() function to compute the population standard deviation.
popsd(y)
## 5.574609

# Q6: Set the seed at 1. Take a random sample Y of size 25 from y.
# What is the sample average?
set.seed(1)
Y <- sample(y, 25)
Y_bar <- mean(Y)
Y_bar
## 35.8036


# Q7: What is the difference in absolute value between y-x and Y-X?
abs((y_bar-x_bar) - (Y_bar-X_bar))
## 1.399884



# Q8: Repeat the above for females, this time setting the seed to 2.
xf <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
xf_bar <- mean(xf)

set.seed(2)
Xf <- sample(xf, 25)
Xf_bar <- mean(Xf)


yf <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
yf_bar <- mean(yf)

set.seed(2)
Yf <- sample(yf, 25)
Yf_bar <- mean(Yf)


# What is the difference in absolute value between y-x and Y-X?
abs((yf_bar-xf_bar) - (Yf_bar-Xf_bar))
## 0.3647172