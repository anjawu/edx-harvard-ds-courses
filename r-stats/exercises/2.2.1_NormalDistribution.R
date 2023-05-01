library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
# x represents the weights for the entire population.
x <- unlist( read.csv(filename) )

# set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages. 
set.seed(1)
n1 <- 1000
avg5_1000 <- vector("numeric", n1)
for (i in 1:n1) {
  avg5_1000[i]<-mean(sample(x, 5))
}

# After that, set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times. Save these averages:
set.seed(1)
n2 <- 1000
avg50_1000 <- vector("numeric", n1)
for (i in 1:n2) {
  avg50_1000[i]<-mean(sample(x, 50))
}

# Q1:Use a histogram to "look" at the distribution of averages we get with a sample size of 5 and a sample size of 50. How would you say they differ?
par(mfrow=c(1,2))
par(mfrow=c(1,1))
hist(avg5_1000)
hist(avg50_1000)
## They both look roughly normal, but with a sample size of 50 the spread is smaller.

# Q2: For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?
mean(23 < avg50_1000 & avg50_1000 < 25)
## 0.982


# Q3: Note that you can use the function pnorm() to find the proportion of observations below a cutoff x given a normal distribution with mean mu and standard deviation sigma with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma ).
# What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 and standard deviation 0.43?
pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43) # think area under curve
## 0.9765648




