### Problem 3

## 1. The following table gives the size of the floor area (ha) 
## and the price ($000), for 15 houses sold in the Canberra (Australia) 
## suburb of Aranda in 1999.
## \begin{tabular}{cols}
## & area & sale.price \\
## 1 & 694 & 192.0 \\
## 2 & 905 & 215.0 \\
## 3 & 802 & 215.0 \\
## 4 & 1366 & 274.0 \\
## 5 & 716 & 112.7 \\
## 6 & 963 & 185.0 \\
## 7 & 821 & 212.0 \\
## 8 & 714 & 220.0 \\
## 9 & 1018 & 276.0 \\
## 10 & 887 & 260.0 \\
## 11 & 790 & 221.5 \\ 
## 12 & 696 & 255.0 \\ 
## 13 & 771 & 260.0 \\ 
## 14 & 1006 & 293.0 \\
## 15 & 1191 & 375.0
## \end{tabular}$$

library(tidyverse)
library(DAAG)
## Type these data into a data frame with column names area and sale.price.
area <- c(694, 905, 802, 1366, 716, 963, 821, 714, 1018, 887, 790, 696, 
          771, 1006, 1191)
sale.price <- c(192, 215, 215, 274, 112.7, 185, 212, 220, 276, 260, 221, 255,
                260, 293, 375)
datasale <- data.frame(area, sale.price)

# (a) Plot sale.price versus area.
plot(sale.price ~ area)

# (b) Use the hist() command to plot a histogram of the sale prices.
hist(datasale$sale.price)

# (c) Repeat (a) and (b) after taking logarithms of sale prices. 
plot(sale.price ~ area, data=datasale, log="y",
       pch=16, xlab="Floor Area", ylab="Sale Price",
       main="(c) log(sale.price) vs area")

hist(log(datasale$sale.price))

hist(log(datasale$sale.price),
     xlab="Sale Price (logarithmic scale)",
     main="Histogram of log(sale.price)")


## 2. The "orings" data frame (DAAG package) gives data on the damage that had 
## occurred in US space shuttle launches prior to the disastrous Challenger 
## launch of 28 January 1986. The observations in rows 1, 2, 4, 11, 13, and 18 
## were included in the pre-launch charts used in deciding whether to proceed 
## with the launch, while remaining rows were omitted.	

## Create a new data frame by extracting these rows from "orings", and plot 
## "total" incidents against "temperature" for this new data frame. 
## Obtain a similar plot for the full data set.
library(DAAG)
odata <- data.frame(orings)
odata86 <- odata[c(1,2,4,11,13,18), ]
plot(Total ~ Temperature, data = odata86,
     main="Temperature vs Total")

## 3. For the data frame "possum" (DAAG package)
# (a) Use the function str() to get information on each of the columns.
dpossum <- data.frame(possum)
str(dpossum)

# (b) Using the function complete.cases(), determine the rows in which one or 
# more values is missing. Print those rows. 
complete.cases(dpossum)

# In which columns do the missing values appear?
# No missing values in each columns. 

## 4. For the data frame "ais" (DAAG package)
# (a) Use the function str() to get information on each of the columns. 
# Determine whether any of the columns hold missing values.
dais <- data.frame(ais)
?ais
str(dais)
complete.cases(dais)

# (b) Make a table that shows the numbers of males and females for each 
# different sport. In which sports is there a large imbalance 
view(dais$sex)  
dais$sex
str(dais$sex)
library(plyr)
count(dais$sex)

## 5. Create a table that gives, for each species represented in the data frame 
# "rainforest" (DAAG package), the number of values of branch that are NAs, and 
# the total number of cases.

drain <- data.frame(rainforest)
!is.na(drain)
count(!is.na(drain))
sum(!is.na(drain))
