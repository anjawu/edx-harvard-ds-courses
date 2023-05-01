library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
X <- unlist( read.csv(filename) )

X

# Q1: Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.
# What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n<-1000
rnd_1000 <- vector("numeric", n)
for (i in 1:n) {
  rnd_1000[i]<-mean(sample(X, 5))
}

avg_plus1 <- mean(X)+1
avg_minus1 <- mean(X)-1

mean(rnd_1000 > avg_plus1) + mean(rnd_1000 < avg_minus1)
## 0.503
# much better way: mean( abs( averages5 - mean(x) ) > 1)


# Q2: We are now going to increase the number of times we redo the sample from 1,000 to 10,000. Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. Save these averages.
# What proportion of these 10,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n<-10000
sample10000_avg <- vector("numeric", n)
for (i in 1:n) {
  sample10000_avg[i]<-mean(sample(X, 5))
}
mean(abs(sample10000_avg - mean(X)) > 1)
## 0.5084


