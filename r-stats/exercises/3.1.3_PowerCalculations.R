library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)

babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist


# Q1: We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.
# Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets. 
# Use the t-test function to find the p-value. (Note that you already performed this calculation in the last assessment.)
# The p-value is larger than 0.05 so using the typical cut-off, we would not reject. This is a type II error. 
# Which of the following is *not* a way to decrease this type of error?
## Find a population for which the null is not true.

# Q2: Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times. 
# What proportion of the time do we reject at the 0.05 level?
set.seed(1)
reject <- function(n, alpha=0.05) {
  dat.ns <- sample(bwt.nonsmoke, n)
  dat.s <- sample(bwt.smoke, n)
  pval <- t.test(dat.ns, dat.s)$p.value
  pval < alpha
}

rejections <- replicate(10000, reject(5))
mean(rejections)
## 0.099

# Q3: Note that, not surprisingly, the power is lower than 10%. 
# Repeat the exercise above for samples sizes of 30, 60, 90 and 120. 
# Which of those four gives you power of about 80%?
rejections30 <- replicate(10000, reject(30))
mean(rejections30) # 0.4798

rejections60 <- replicate(10000, reject(60))
mean(rejections60) # 0.7916

rejections90 <- replicate(10000, reject(90))
mean(rejections90) # 0.9349

rejections120 <- replicate(10000, reject(120))
mean(rejections120) # 0.9841

# Their code:
Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 


# Q4: Repeat the problem above, but now require an alpha level of 0.01. Which of those four gives you power of about 80%?
Ns = c(30, 60, 90, 120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 
## 90


# Q5: Consider this statement: In a world where the alternative hypothesis is true, if you fail to reject the null hypothesis because the p value exceeds 0.05 and the power of your test is 90%, the chance that your finding is a false negative is 10%.
## False
# If the null hypothesis is false and you accept it, the chance you are in error is 100%, not 10%. Conversely, if the null hypothesis is true and you accept it, the chance you are in error is 0%. The 10% refers only to how often you would be in error over very many uses of the test across different studies.




