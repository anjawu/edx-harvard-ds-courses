# Create a vector time of the sorted times:
time <- sort(nym.2002$time)


# Q1: What is the fastest time divided by the median time?
min(time) # 147.3333
median(time) # 262.8417
min(time)/median(time)
## 0.5605402

# Q2: What is the slowest time divided by the median time?
max(time)/median(time)
## 2.156368