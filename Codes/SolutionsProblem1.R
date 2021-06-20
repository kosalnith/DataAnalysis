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

## 4. Calculate the following: 
# (a) \sum_{n=1}^{200}(i^{3}+4i^{2})
valuena <- (1:200)
calculatena <- sum(valuena^3 + 4 * valuena^2)

# (b) \sum_{n=1}^{25}(\dfrac{2^{n}}{n} + \dfrac{3^{n}}{n^{2}})
valuenb <- (1:25)
calculatenb <- sum((2^valuenb) / valuenb + 3^valuenb / (valuenb^2))

## 5. Use the function "paste" to create the following character vectors
# (a) ("Cambodia 1", "Cambodia 2", ..., "Cambodia 34")
Cambodia <- paste("Cambodia", 1:30)

# (b) ("ffteam1", "ffteam2", ..., "ffteam29")
ffteam <- paste("ffteam", 1:29)

## 6. Execute the following lines which create two vectors of random integers 
## which are chosen with replacement from the integers 0, 1, ..., 999. 
## Both vector have length 250. 

set.seed(50) 
  xVec <- sample(0:999, 250, replace=T)
  yVec <- sample(0:999, 250, replace=T)

# Suppose x = (x_{1}, x_{2}, ..., x_{n}) denotes the vector xVec 
# and y = (y_{1}, y_{2}, ..., y_{n}) denotes the vector yVec

# (a) Create the vector (y_{2}-x_{1}, ..., y_{n}-x_{n-1})
xy <- yVec[-1] - xVec[-length(xVec)]

# (b) Create the vector \left ( \dfrac{sin(y_{1})}{cos(x_{2})}, 
# \dfrac{sin(y_{2})}{cos(x_{3})}, ..., \dfrac{sin(y_{n-1})}{cos(x_{n})}\right )
sincos <- sin(yVec[-length(yVec)]) / cos(xVec[-1])

# (c) Create the vector 
# (x_{1} + 2x_{2} - x_{3}, x_{2} + 2x_{3} - x{4}, ..., x_{n-2} +2x_{n-1} - x_{n})
vecxy <- xVec[-c(249,250)] + 2*xVec[-c(1,250)]-xVec[-c(1,2)]

# (d) Calculate \sum_{i=1}^{n-1} \dfrac{e^{-x_{i+1}}}{x_{i} + 10}
sum(exp(-xVec[-1])/(xVec[-length(xVec)]+10))

## 7. Use the vectors xVec and yVec created in the previous question and 
## the functions sort, order, mean, sqrt, sum and abs. 

# (a) Pick out the values in yVec which are >400.
yVec[yVec>400]

# (b) What are the index positions in yVec of the values which are >400
(1:length(yVec))[yVec>400] # or use function "which"
which(yVec>400) 

# (c) What are the values in xVec which correspond to 
# the values in yVec which are >400?
xVec[yVec>400]

# (d) Create the vector (|x_{1} - \bar{X}|^{1/2}, |x_{2} - \bar{X}|^{1/2}, ..., 
# |x_{n} - \bar{X}|^{1/2}) where \bar{X} denotes the mean of 
# the vector X = (x_{1}, x_{2}, ..., x_{n})
sqrt(abs(xVec-mean(xVec)))

# (e) How many values in yVec are within 200 of the maximum 
# value of the terms in yVec?
sum(yVec>max(yVec)-200)
  
# (f) How many numbers in xVec are divisible by 2? 
sum(xVec%%2==0)

# (g) Sort the numbers in the vector xVec in the order of increasing 
# values in yVec.
xVec[order(yVec)]

# (h) Pick out the elements in yVec at index positions 1, 4, 7, 10, 13, ...
yVec[c(T,F,F)]

## 8. By using the function cumprod or otherwise, calculate
## 1 + \dfrac{2}{3} + \dfrac{24}{35} + \dfrac{246}{357} + ... + 
## (\dfrac{2}{3} \dfrac{4}{5} ... \dfrac{38}{39})
1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))