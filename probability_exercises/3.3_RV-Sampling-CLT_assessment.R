# This is a 6-part question asking you to determine some probabilities of what happens when a student guessed for all of their answers on the SAT. Use the information below to inform your answers for the following questions.
# An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. 
# The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.
points_incorrect <- -0.25
points_correct <- 1
q <- 4/5
p <- 1/5
n <- 44


# 1) What is the probability of guessing correctly for one question?
p
## 0.2

# 2) What is the expected value of points for guessing on one question?
E_X1 <- points_correct*p + points_incorrect*q
E_X1
## 0

# 3) What is the expected score of guessing on all 44 questions?
mu <- n*E_X1
mu
## 0

# 4) What is the standard error of guessing on all 44 questions?
sigma <- sqrt(n) * abs(-0.25-1)*sqrt(p*q)
sigma
## 3.316625

# 5) Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1- pnorm(8, mu, sigma)
## 0.007930666

# 6) Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
B <- 10000
sim <- replicate(B, {
  X <- sample(c(points_correct, points_incorrect), n, replace = TRUE, prob = c(p, q))
  sum(X)
})
mean(sim >= 8)



# The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.
# In this two-part question, you'll explore how that affected the expected values for the test.
points <- 1
q_new <- 3/4
p_new <- 1/4
n <- 44

# 7) Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
# What is the expected value of the score when guessing on this new test?
E_Y <- n*(p_new*points)
E_Y
## 11

# 8) Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
E <- n*p*points
S <- sqrt(n) * abs(0-1)*sqrt(p*(1-p))
prob <- 1 - pnorm(35, E, S)
data.frame(p = p, over35 = prob)
plot(prob, p)
## 0.85
#  Their code:
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])



################################################ Betting on Roulette ################################################
# A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. 
# The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. 
# A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
p <- 5/38
q <- 1-p
wins <- 6
loses <- -1
n <- 500

# 1) What is the expected value of the payout for one bet?
mu <- wins*p + loses*q
mu
## -0.07894737

# 2) What is the standard error of the payout for one bet?
SE_X1 <- abs(wins-loses)*sqrt(p*q)
SE_X1 
## 2.366227

# 3) What is the expected value of the average payout over 500 bets?
# Remember there is a difference between expected value of the average and expected value of the sum.
mu
## -0.07894737

# 4) What is the standard error of the average payout over 500 bets?
# Remember there is a difference between the standard error of the average and standard error of the sum.
SE_X1/sqrt(500)
## 0.1058209

# 5) What is the expected value of the sum of 500 bets?
n*mu
## -39.47368

# 6) What is the standard error of the sum of 500 bets?
sqrt(n)*SE_X1
## sqrt(n)

# 7) Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, P(X<=0).
pnorm(0, n*mu, sqrt(n)*SE_X1)
## 0.7721805


