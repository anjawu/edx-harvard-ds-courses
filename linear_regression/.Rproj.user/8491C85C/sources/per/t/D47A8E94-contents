install.packages('Lahman')
library(Lahman)
library(dplyr)
library(ggplot2)


# 5) What does the variable “SOA” stand for in the Teams table?
?Teams
## Strikeouts by pitchers

# 6) Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001. 
# Make a scatterplot of runs per game versus at bats (AB) per game.
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Which of the following is true?
## As the number of at bats per game increases, the number of runs per game tends to increase.

# 7) Use the filtered Teams data frame from Question 6. 
# Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)
# Which of the following is true?
# As the number of errors per game increases, the win rate tends to decrease.


# Use the filtered Teams data frame from Question 6. 
# Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)
# Which of the following is true?
# There is no clear relationship between doubles per game and triples per game.


