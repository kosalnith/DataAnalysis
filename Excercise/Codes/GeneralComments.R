# General Comments 

# Don't use prompt (>) in R scripts. 
a <- c(1:30)
b <- seq(30, 1, -1)

# Writing in the language programming should well organized because it is the 
# efforts that need to review and edit many times.  

# Data transformation is the first step of data analysis, it is very important 
# task that everyone should carefully and much pay attentions. 

# When you call library in your RStudio, please check it first to make sure you 
# have already installed it in your application.

installed.packages("tidyverse")

# If not yet, plsease install it 
install.packages("tidyverse")

# And than call it for the use 
library(tidyverse)

# When create a vector with many values, please don't forest add "c": 
# Combine Values into a Vector or List
C <- c(2,4,5,6)
c <- 2:7

# Before call data frame or vector in the console or R script, please check it first
houseprices$sale.price

area <- c(694, 905, 802, 1366, 716, 963, 821, 714, 1018, 887, 790, 696, 
          771, 1006, 1191)
sale.price <- c(192, 215, 215, 274, 112.7, 185, 212, 220, 276, 260, 221, 255,
                260, 293, 375)
houseprices <- data.frame(area, sale.price)

houseprices$sale.price

# Should carefully about it command and other write.
futureforum <- c(44,66,33)
e<- rep(futureforum, 20) b 
e<- rep(futureforum, 20)  
f <- rep(futureforum, each-11)
f <- rep(futureforum, each = 11)

# == is different from =
x = 20
y = 21
z = 20
x == y
x == z

# Should resign a new object name different from the object in the data set.  
(x <- c(2011:2020))
(x <- data_frame((x), 3))
(x <- c(200:100))
( y <- c(23:40))  
x
y
