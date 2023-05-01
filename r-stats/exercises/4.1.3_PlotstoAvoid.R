install.packages("dslabs")
library(dslabs)

# The dslabs package has a dataset called divorce_margarine that has data from 2000 to 2009 about the per capita US consumption of margarine and the divorces per 1000 in Maine. Run this example.
data("divorce_margarine")

par(mfrow=c(1,1))
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine) #0.9925585
## As the saying goes, "correlation does not imply causation." Just because two variables seem to be related doesn't mean that one of them caused the other.