# Comment out lines: command + shift + C

#Q1: What version of R are you using?
R.version
## 4.2.2

# Q2: Create a numeric vector containing the numbers 2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23. 
# What is the average of these numbers?
x <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(x)
## 7.794286

# Q3: Use a for loop to determine the value of sum(i^2) i=1 to 25
sum <- 0
for(i in 1:25)
  sum <- sum + i^2
sum
## 5525

# Q4: The cars dataset is available in base R. You can type cars to see it.
# Use the class() function to determine what type of object is cars.
class(cars)
## "data.frame"

# Q5: How many rows does the cars object have?
nrow(cars)
## 50

# Q6: What is the name of the second column of cars?
names(cars)[2]
# OR
str(cars) # less efficient but gives you more information
## "dist"

# Q7: The simplest way to extract the columns of a matrix or data.frame is using [. For example you can access the second column with cars[,2].
# What is the average distance traveled in this dataset?
mean(cars[,2])
## 42.98

# Q8: Familiarize yourself with the which() function. Which row of cars has a a distance of 85?
# https://www.digitalocean.com/community/tutorials/which-function-in-r
which(cars[,2]==85)
# OR
which(cars==85, arr.ind=TRUE) # gives more information than what was asked
## 50