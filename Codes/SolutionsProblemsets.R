# The solution all problem sets. 

### Problem 1: Vectors
## 1. Create the vectors:

# (a) (1, 2, 3, ..., 29, 30)

a <- c(1:30)

# (b) (30, 29, 28, ..., 2, 1)
b <- c(30:1)

# (c) (1, 2, 3, ..., 19, 20, 19, 18, ..., 2, 1)
c <- c(1:20, 19:1)

# (d) (44, 66, 33) and assign it to the name "futureforum".

futureforum <- c(44, 66, 33)

# (e) (44, 66, 33, 44, 66, 33, ..., 44, 66, 33) where there are 20 
# occurrences of 44. 

# Use the help function by typing "?rep" to know the rep command mean in R.  
?rep 
e <- rep(futureforum, 20)

# (f) (44, 66, 33, 44, 66, 33, ..., 44, 66, 33, 44) where there are 
# 11 occurrences of 44, 10 occurrences of 66 and 10 occurrences of 33.
f <- rep(futureforum, l=31)

# (g) (44, 44, ..., 44, 66, 66, ..., 66, 33, 33, ..., 33) where there are 
# 10 occurrences of 44, 20 occurrences of 44, 20 occurrences of 66, 
# 30 occurrences of 33. 
g <- rep(futureforum, times=c(10, 20, 30))

## 2. Create a vector of the value of e^{x}cos(x) at x = 3, 3.1, 3.2, ..., 7.
x <- seq(3, 7, by = 0.1)
valuex <- exp(x)*cos(x)

## 3. Create the following vectors:
# (a) (0.1^{3}0.2^{1}, 0.1^{6}0.2^{3}, ..., 0.1^{37}0.2^{32})
valuea <- (0.1^seq(3, 37, by = 3))*(0.2^seq(1,32,by=3))

# (b) (2, \dfrac{2^{2}}{4}, \dfrac{2^{3}}{4}, \dfrac{2^{4}}{5}, ..., 
# \dfrac{2^{26}}{26})
valueb <- (2^(1:26))/c(1, 4, 4, 5:26)

## 4.Calculate the following: 
# (a) \sum_{n=1}^{200}(i^{3}+4i^{2})
valuen <- (1:200)
calculatei <- sum()