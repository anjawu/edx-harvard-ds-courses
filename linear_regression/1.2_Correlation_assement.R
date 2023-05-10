library(Lahman)

# 7) Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001.
# What is the correlation coefficient between number of runs per game and number of at bats per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  select(AB_per_game, R_per_game) %>%
  cor(method = "pearson")
## 0.6580976
# Their answer:
library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)

# 8) Use the filtered Teams data frame from Question 7.
# What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  select(W_per_game, E_per_game) %>%
  cor(method = "pearson")
## -0.3396947

# 9) Use the filtered Teams data frame from Question 7.
# What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G)%>%
  select(X3B_per_game, X2B_per_game) %>%
  cor(method = "pearson")
## -0.01157404 

