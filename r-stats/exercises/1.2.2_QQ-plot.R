load("skew.RData")

dim(dat)
View(dat)

par(mfrow = c(3,3)) # changes view to be a 3 by 3 subplots plot

for (i in 1:9) {
  qqnorm(dat[,i])
}

par(mfrow=c(1,1)) # changes view to be one plot at a time

# Skewed data columns 4 and 9
hist(dat[,4])
qqnorm(dat[,4])
# has positive skew (histogram: long right tail, qqnorm: u shape; opens up)

hist(dat[,9])
qqnorm(dat[,9])
# has negative skew (histogram: long left tail, qqnorm: n shape; opens down)
