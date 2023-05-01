# to make sure you are using the new RNG settings for R:
RNGkind()
# This should return 3 values: “Mersenne-Twister”, “Inversion”, “Rejection”.

install.packages("downloader")

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)


# Q1: Read in the file femaleMiceWeights.csv and 
# report the exact name of the column containing the weights.

femaleMiceWeights_df <- read.csv(filename)
names(femaleMiceWeights_df)
# can also just checkout head(femaleMiceWeights_df)
View(femaleMiceWeights_df) # lets you see the table
## "Bodyweight"



# Q2: The [ and ] symbols can be used to extract specific rows and specific columns of the table.
# What is the entry in the 12th row and second column?

femaleMiceWeights_df[12, 2] # [row, column]
## 26.25



# Q3: You should have learned how to use the $ character to extract a column from a table and return it as a vector. 
# Use $ to extract the weight column and report the weight of the mouse in the 11th row.

weight <- femaleMiceWeights_df$Bodyweight
weight[11]
## 26.91



# Q4: The length() function returns the number of elements in a vector.
# How many mice are included in our dataset?

length(weight)
# OR
nrow(femaleMiceWeights_df)
## 24



# Q5: To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. 
# View the data and determine what rows are associated with the high fat or hf diet. 
# Then use the mean() function to compute the average weight of these mice.
# What is the average weight of mice on the high fat diet?

unique(femaleMiceWeights_df$Diet) # check what outputs there are in first column
# https://stackoverflow.com/questions/5577727/is-there-an-r-function-for-finding-the-index-of-an-element-in-a-vector
index_v <- which(femaleMiceWeights_df$Diet %in% "hf") # gets index that has hf value
mean(femaleMiceWeights_df[index_v, 2]) # calculates mean of weight of hf mice
## 26.83417



# Q6: One of the functions we will be using often is sample(). Read the help file for sample() using ?sample. 
# Now take a random sample of size 1 from the numbers 13 to 24 and report back the weight of the mouse represented by that row. 
# Make sure to type set.seed(1) to ensure that everybody gets the same answer.
set.seed(1)
random_index <- sample(13:24, size=1)
femaleMiceWeights_df[random_index, 2]
## 34.02









