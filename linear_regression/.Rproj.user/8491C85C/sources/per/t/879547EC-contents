install.packages('broom')
library(ggplot2)
library(HistData)

data("GaltonFamilies")

galton_heights <- GaltonFamilies%>%     
  filter(gender == "male") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(father, childHeight) %>%     
  rename(son = childHeight) 

# 1) The following code was used in the video to plot RSS with beta0 = 25.
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
# In a model for sons’ heights vs fathers’ heights, what is the least squares estimate (LSE) for  if we assume  is 36?
# Hint: modify the code above to do your analysis.
## 0.5



# 2) The least squares estimates for the parameters minimize the residual sum of squares.



# 3) Load the Lahman library and filter the Teams data frame to the years 1961-2001.
# Mutate the dataset to create variables for bases on balls per game, runs per game, and home runs per game, 
# then run a linear model in R predicting the number of runs per game based on both the number of bases on balls per game and the number of home runs per game.
# What is the coefficient for bases on balls per game?
library(Lahman)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G, HR_per_game = HR/G) %>%
  select(BB_per_game, R_per_game, HR_per_game) %>%
  lm(R_per_game ~ BB_per_game+HR_per_game, data = .)
## 0.3874
# Their code:
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))



# 4) We run a Monte Carlo simulation where we repeatedly take samples of N = 100 from the Galton heights data and compute the regression slope coefficients for each sample:
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
# What does the central limit theorem tell us about the variables beta_0 and beta_1?
## With a large enough N, the central limit theorem applies and tells us that the distributions of both beta_0 and beta_1 are approximately normal. The expected values of beta_0 and beta_1 are the true values of  and , assuming that the Galton heights data are a complete population.
## For hypothesis testing, we assume that the errors in the model are normally distributed.



# 5) Which R code(s) below would properly plot the predictions and confidence intervals for our linear model of sons’ heights?
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# OR
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")







set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


# 6) Fit a linear regression model predicting the mothers' heights using daughters' heights.
lm(female_heights$mother ~ female_heights$daughter)
# What is the slope of the model?
## 0.31  

# What the intercept of the model?
## 44.18
# Their code
fit <- lm(mother ~ daughter, data = female_heights)
m <- fit$coef[2]
b <- fit$coef[1]


# 7) Predict mothers' heights using the model from Question 6 and the predict() function.
female_heights[1,]
# 1st daughters height is 69 
# What is the predicted height of the first mother in the dataset?
x <- 69
m*x+b
## 65.6 
# Their code:
predict(fit)[1]

# What is the actual height of the first mother in the dataset?
## 67



########################################## BASEBALL ##########################################
# we want to generate two tables: one for 2002 and another for the average of 1999-2001 seasons. We want to define per plate appearance statistics, keeping only players with more than 100 plate appearances.
# Here is how we create the 2002 table:
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# 8) Now compute a similar table but with rates computed over 1999-2001. 
# Keep only rows from 1999-2001 where players have 100 or more plate appearances, 
# calculate each player's single rate and BB rate per stint (where each row is one stint - a player can have multiple stints within a season), 
# then calculate the average single rate (mean_singles) and average BB rate (mean_bb) per player over the three year period.
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

# How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 1999-2001?
sum(bat_99_01$mean_singles > 0.2)
## 46

# How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
sum(bat_99_01$mean_bb > 0.2)
## 3




# 9) Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you created in the previous question.
bat <- inner_join(bat_02, bat_99_01, by = "playerID")
head(bat)

# What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(bat$singles, bat$mean_singles)
## 0.551

# What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
cor(bat$bb, bat$mean_b)
## 0.717


# 10) Make scatterplots of mean_singles versus singles and mean_bb versus bb.
# Are either of these distributions bivariate normal?
ggplot(bat, aes(singles, mean_singles)) +
  geom_point()

ggplot(bat, aes(bb, mean_bb)) +
  geom_point()
## Both are (oval shape)


# 11) Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
# What is the coefficient of mean_singles, the slope of the fit?
lm(singles ~ mean_singles, bat)
## 0.5881
# Their code:
fit_singles <- lm(singles ~ mean_singles, data = dat)
fit_singles$coef[2]

# Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
# What is the coefficient of mean_bb, the slope of the fit?
lm(bb ~ mean_bb, bat)
## 0.8290 





