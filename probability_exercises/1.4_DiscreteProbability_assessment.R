################################################### PART 1 ###################################################
# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). 
# In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. 
# The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
install.packages("gtools")
install.packages("tidyverse")
library(gtools)


# 1) How many different ways can the 3 medals be distributed across 8 runners?
medals <- permutations(8,3)
nrow(medals)
##336

# 2) How many different ways can the three medals be distributed among the 3 runners from Jamaica?
jam_medal <- permutations(3,3)
nrow(jam_medal)
## 6

# 3) What is the probability that all 3 medals are won by Jamaica?
nrow(jam_medal)/nrow(medals)
## 0.01785714

# 4) Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
# For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and
# check whether they are all from Jamaica. 
# Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
B <- 10000
set.seed(1)
jamaica <- replicate(B, {all(sample(runners, 3)=="Jamaica")})

# Calculate the probability that all the runners are from Jamaica.
mean(jamaica)
## 0.0174

#Their code:
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)


################################################### PART 2 ###################################################
# A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. 
# He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. 
# He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

# 1) How many meal combinations are possible with the current menu?
nrow(permutations(6,1))*nrow(combinations(6,2))*nrow(permutations(2,1))
## 180

# 2) The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original special to 3 drink options?
nrow(permutations(6,1))*nrow(combinations(6,2))*nrow(permutations(3,1))
## 270

# 3) The manager decides to add the third drink but needs to expand the number of options. 
# The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.
# How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
nrow(permutations(6,1))*nrow(combinations(6,3))*nrow(permutations(3,1))

# 4) The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
# - Write a function that takes a number of entree choices and returns the number of meal combinations possible 
# given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
entree_poss <- function(n){
  combo <- nrow(permutations(n,1))*nrow(combinations(6,2))*nrow(permutations(3,1))
  combo
}
# - Use sapply() to apply the function to entree option counts ranging from 1 to 12.
N <- seq(1:12)
sapply(N, entree_poss)
# What is the minimum number of entree options required in order to generate more than 365 combinations?
## 9

# Their code:
library(tidyverse)

entree_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

# 5) The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
# - Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
# - Use sapply() to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate more than 365 combinations?
side_poss <- function(n){
  6*nrow(combinations(n,2))*3
}
N <- c(2:12)
combo <- sapply(N, side_poss)

data.frame(sides = c(2:12), combos = combo) %>% filter(combos>=365) %>% min(.$sides)
## 7

################################################### PART 3 ###################################################
# Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. 
# The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) 
# to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. 
# The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
head(esoph)
# Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.

# 1) How many groups are in the study?
nrow(esoph)
## 88

# 2) How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases
## 200

# 3) How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls

# 4) What is the probability that a subject in the highest alcohol consumption group is a cancer case?
unique(esoph$alcgp)
highest_alc_cases <- filter(esoph, alcgp == "120+") %>% select(ncases)
highest_alc_control <- filter(esoph, alcgp == "120+") %>% select(ncontrols)
sum(highest_alc_cases)/sum(highest_alc_cases, highest_alc_control)
## 0.672 (0.6716418)

# Their code:
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# 5) What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases) , ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases/(ncases + ncontrols)) %>% # creates new column that are functions of existing variables
  pull(p_case) # selects a column in a data frame and transforms it into a vector
## 0.06987952

unique(esoph$tobgp)
# 6) Given that a person is a case, what is the probability that they smoke 10g or more a day?
all_cases_count <- esoph %>%  filter(ncases>0) %>% select(tobgp, ncases)
all_cases_count <- sum(all_cases$ncases)

lowtob_cases <- esoph %>%  filter(ncases>0, tobgp == "0-9g/day") %>% select(tobgp, ncases)
lowtob_cases_count <- sum(lowtob_cases$ncases)
1- lowtob_cases_count/all_cases_count
## 0.61
# Their code:
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# 7) Given that a person is a control, what is the probability that they smoke 10g or more a day?
tob_use_control <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()
tob_use_control/all_controls
## 0.423

# 8) For cases, what is the probability of being in the highest alcohol group?
head(esoph)
unique(esoph$alcgp)
highest_alc <- esoph %>%
  filter(alcgp=="120+") %>%
  summarize(ncases=sum(ncases))

p_cases_high_alc <- highest_alc/all_cases
p_cases_high_alc
## 0.225
# Their code
high_alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc

# 9) For cases, what is the probability of being in the highest tobacco group?
unique(esoph$tobgp)
highest_tob <- esoph %>%
  filter(tobgp=="30+") %>%
  summarize(ncases=sum(ncases))

p_cases_high_tob <- highest_tob/all_cases
p_cases_high_tob
## 0.155

# 10) For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
highest_tob_alc <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alc_tob <- highest_tob_alc / all_cases
p_case_high_alc_tob
## 0.05

# 11) For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
highest_tob_or_alc <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncases) %>%
  sum()
p_case_high_alcORtob <- highest_tob_or_alc / all_cases
p_case_high_alcORtob
## 0.33
# Or you could use P(A or B) = P(A) +P(B) - P(A and B)

# 12) For controls, what is the probability of being in the highest alcohol group?
# Report your answer to 3 significant figures.
high_alc_control <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc <- high_alc_control/all_controls
p_control_high_alc
## 0.0283871
  
# 13) How many times more likely are cases than controls to be in the highest alcohol group?
# Report your answer to 3 significant figures.
p_cases_high_alc/p_control_high_alc
## 7.926136

# 14) For controls, what is the probability of being in the highest tobacco group?
# Report your answer to 3 significant figures.
high_tob_control <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_tob_alc <- high_tob_control/all_controls
p_control_tob_alc
## 0.06580645


# 15) For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
# Report your answer to 3 significant figures.
highest_tob_alc_control <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc_tob <- highest_tob_alc_control / all_controls
p_control_high_alc_tob
## 0.003870968

# 16) For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
# Report your answer to 3 significant figures.
highest_tob_or_alc_control <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

p_control_high_alc_or_tob <- highest_tob_or_alc_control / all_controls
p_control_high_alc_or_tob
## 0.09032258

# 17) How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
# Report your answer to 3 significant figures.
p_case_high_alcORtob /p_control_high_alc_or_tob
## 3.653571



