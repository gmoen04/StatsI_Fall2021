#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("/Users/garethmoen/Documents/PG Dip - ASDS/M-Stats I/Problem Set Submissions")


#####################
# Problem 1
#####################

# Question1 - Part 1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

str(y) # sample size = 25 (t test needed)
mean(y) # sample mean = 98.44
sd(y) # sd of sample y = 13.09287
median(y) # median = 98
summary(y)

plot(density(y),
     main="Distribution of class IQ",
     xlab="IQ")

# confidence coefficient = .90 add and subtract 
# df (degrees of freedom) for a t-test is n-1 = 25-1 = 24

error <- qt(0.95, df=24)*13.09/sqrt(24) # to find the standard error with 90% confidernce interval, df of 24, an sd of 13.09 and an 'n' of 25.
print(error) # error of 4.571451
interval_1 <- 98.44-error # interval to the left side of the mean
interval_2 <- 98.44+error # interval to the right side of the mean

interval_1 # 93.86855
interval_2 # 103.0155

# So the 90% confidence interval for the average student IQ in the school is 93.87 - 103

# Question1 - Part 2

# Null Hypothesis > Average school IQ is equal or less than 100
# Alternative Hypothesis > Average school IQ is greater than 100
# α = 0.05
# 

t.test(y, mu = 100, alternative = "greater")

# data:  y
#t = 37.593, df = 24, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#  93.95993      Inf
#sample estimates:
#  mean of x 
# 98.44 

# Results
# p-value < 2.2e-16 which is not less than α (0.05), therefore I fail the reject the null hypothesis, so the average school IQ is not higher than the average IQ among all the schools in the country, but less than or equal to it. 

#####################
# Problem 2
#####################
# Q - Please plot the relationships among Y, X1, X2, and X3? What are the correlations among them (you just need to describe the graph and the relationships among them)?

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
str(expenditure) # an overall view of the data set
class(expenditure)
typeof(expenditure)
ls(expenditure)
ls.str(expenditure)
summary(expenditure)
View(expenditure)

plot(density(expenditure$Y), col = "blue", main = "Expenditure on shelters/housing")
Yplot(density(expenditure$X1), col ="Red", main = "Personal income in state")
plot(density(expenditure$X2), col = "Green", main = "Financially insecure residents per 100,000")
plot(density(expenditure$X3), col = "Orange", main = "People per thousand residing in urban areas")

library(tidyverse) #loads the tidyverse library

ggplot(data = expenditure) + geom_point(mapping = aes(x = Y, y = X1)) # create a scatterplot using Y, X1 as variables
                                        # main="Scatterplot Y-X1" # I hoped this would give it a title but that didn't work
                                        # xlab="per capita expenditure on shelters/housing assistance in state" # I hoped this would give the x-axis a label but that didn't work
                                        # ylab="per capita personal income in state" # I hoped this would give the y-axis a label but that didn't work
ggplot(data = expenditure) + geom_point(mapping = aes(x = Y, y = X2)) 
ggplot(data = expenditure) + geom_point(mapping = aes(x = Y, y = X3)) 
ggplot(data = expenditure) + geom_point(mapping = aes(x = X1, y = X2))
ggplot(data = expenditure) + geom_point(mapping = aes(x = X1, y = X3))
ggplot(data = expenditure) + geom_point(mapping = aes(x = X2, y = X3)) 

# Q - Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on housing assistance?

Region_1 <- subset(expenditure, Region == 1) #makes a subset of Region 1
Region_2 <- subset(expenditure, Region == 2) #makes a subset of Region 2
Region_3 <- subset(expenditure, Region == 3) #makes a subset of Region 3
Region_4 <- subset(expenditure, Region == 4) #makes a subset of Region 4

Region1_mean <- mean(Region_1$Y) #mean per capita expenditure in Region 1
Region2_mean <- mean(Region_2$Y) #mean per capita expenditure in Region 2
Region3_mean <- mean(Region_3$Y) #mean per capita expenditure in Region 3
Region4_mean <- mean(Region_4$Y) #mean per capita expenditure in Region 4

Region1_mean
Region2_mean
Region3_mean
Region4_mean #checking the means from each region

aggregate(expenditure$Y, by = list(expenditure$Region), FUN = mean) # finding the mean of variable Y by Region

Region_means <- aggregate(expenditure$Y, by = list(expenditure$Region), FUN = mean)
class(Region_means) # 'Region_means is now a data.frame

Northeast <- Region_1
North_Central <- Region_2
South <- Region_3
West <- Region_4 # trying to rename regions for graph, didn't work

barplot(Region_means$x,
        main = "Per capita expenditure on shelters/housing assistance in state by Region", #title
        names.arg = Region_means$Group.1, # vector with our axis names
        las = 1) #Rotating our axis labels)

# From the plot it seems that region 4 (West) spends the most per capita on shelters / housing assistance.

# Q - Please plot the relationship between Y and X1? Describe this graph and the relationship. Reproduce the above graph including one more variable Region and display diﬀerent regions with diﬀerent types of symbols and colors.

ggplot(data = expenditure) + geom_point(mapping = aes(x = Y, y = X1)) #relationship between Y and X1 (again)
# There seems to be a positive correlation between per capita expenditure on shelters/housing assistance in state and per capita personal income in state

ggplot(data = expenditure, mapping = aes(x = Y, y = X1)) + geom_point(aes(color = Region))
# Variable 'region', added with different coloured points added to the scatterplot for each region.

                                                                      