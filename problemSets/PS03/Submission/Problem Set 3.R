#Problem Set 03

Incumbents <- read.csv("/Users/garethmoen/Documents/GitHub/StatsI_Fall2021/datasets/incumbents_subset.csv")
View(Incumbents)
summary(Incumbents)
str(Incumbents)
library(tidyverse)

#Question 1

#Part 1
# Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.

Reg_01 <- lm(Incumbents$voteshare ~ Incumbents$difflog) #Runs a regression and assigns a name to it
lm(Incumbents$voteshare ~ Incumbents$difflog)
#Regression coefficients
#y-intercept = 0.579
#slope = b = 0.042

#Part 2
#normal scatterplot in Base R
plot(Incumbents$difflog, Incumbents$voteshare, main = "Plot 01", xlab = "Difflog", ylab = "Incumbent voteshare", col = "blue")

#a ggplot view of voteshare and difflog with a regression line
ggplot(aes(x = difflog, y = voteshare), data = Incumbents) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x)

#Part 3
summary(Reg_01) # summary of regression

#Saving the residuals from first regression as on object
resid_obj1 <- residuals(Reg_01)

#Part 4 - Write the prediction equation
#y-intercept = 0.579
#slope = b = 0.042

#Prediction equation formula is "^y = a + bx"
# ^y = 0.579 + (0.042)x


#Question 2
#Part 1
# Run a regression where the outcome variable is presvote and the explanatory variable is difflog.

Reg_02 <- lm(Incumbents$presvote ~ Incumbents$difflog) #Runs a regression and assigns a name to it
lm(Incumbents$presvote ~ Incumbents$difflog)
#Regression coefficients
#y-intercept = 0.508
#slope = b = 0.024

#Part 2
#a ggplot view of presvote and difflog with a regression line
ggplot(aes(x = difflog, y = presvote), data = Incumbents) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x)

#Part 3
summary(Reg_02) # summary of regression

#Saving the residuals from first regression as on object
resid_obj2 <- residuals(Reg_02)

#Part 4 - Write the prediction equation
#y-intercept = 0.508
#slope = b = 0.024

#Prediction equation formula is "^y = a + bx"
# ^y = 0.508 + (0.024)x

#Question 3
#Part 1
# Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.
Reg_03 <- lm(Incumbents$voteshare ~ Incumbents$presvote) #Runs a regression and assigns a name to it
lm(Incumbents$voteshare ~ Incumbents$presvote)
#Regression coefficients
# y-intercept is 0.4413
#slope = b = 0.388

#Part 2
#a ggplot view of voteshare and presvote with a regression line
ggplot(aes(x = presvote, y = voteshare), data = Incumbents) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x)

#Part 3 - prediction equation
#y-interecept = 0.441
#slope = b = 0.388

#Prediction equation formula is "^y = a + bx"
# ^y = 0.441 + (0.388)x

#Question 4
#Part 1
# Run a regression where the outcome variable is the residuals from Question 1 
#and the explanatory variable is the residuals from Question 2.
(Reg_04 <- lm(resid_obj1 ~ resid_obj2)) #Runs the regression, assigns a name to it and prints the values
# y-intercept = -1.674e-18
#slope = b = 2.569e-01

#Part 2
plot(resid_obj2, resid_obj1)

#a ggplot view of the residuals with a regression line, not working yet
ggplot(aes(x = resid_obj2, y = resid_obj1), data = Incumbents) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x)

#Part 3 - prediction equation
#a = y-interecept = -1.674e-18
#slope = b = 2.569e-01

#Prediction equation formula is ^y = -1.674e-18 + (2.569e-01)x
#When x = 0, y is almost = 0

#Question 5
#Part 1 
# Run a regression where the outcome variable is voteshare and the explanatory variables are difflog and presvote
(Reg_05 <- lm(Incumbents$voteshare ~ Incumbents$difflog + Incumbents$presvote))
#Regression coefficients
# y-intercept = 0.449
#slope for Incumbents$difflog = 0.036
#slope for Incumbents$presvote = 0.257

#Part 2 - Prediction equation
#a = y-interecept = 0.449

#Prediction equation formula is "^y = a + b1x1 + b2x2"
# ^y = 0.449 + (0.036)x1 + (0.257)x2

#Part 3
#The slope of 0.257 is the same in this equation as in question 4 