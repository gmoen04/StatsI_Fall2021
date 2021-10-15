# Problem set 2 - Submission 15/10/21

#Question 1
#import dataset created as csv file
PS02_data <- read.csv("/Users/garethmoen/Documents/PG Dip - ASDS/M-Stats I/problemSets/PS02/Submission/Stats - PS02 - Dataset.csv")
str(PS02_data) #overview of data

bribe <- factor(PS02_data$Bribe_requested)
not_stopped <- factor(PS02_data$Not_stopped)

class(bribe)
class(not_stopped)
levels(bribe)
levels(not_stopped)
table(not_stopped, bribe)

#Question 2

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

str(women) #overview of dataset
summary(women) #overview of dataset
class(women)
View(women)
plot(women$water, women$female) #scatterplot of two variables, not really what I need although it does provide some interesting data
plot(women$water, women$village) #not much use
plot(women$water, women$irrigation) #seems useful
plot(women$water, women$GP) #not much use
plot(women$water, women$reserved)
hist(women$water)
boxplot(women$water, women$reserved)

reserved_women_yes <- subset(women, reserved == 1) #subsets the GPs where women positions were reserved
reserved_women_no <- subset(women, reserved == 0) #subsets the GPs where women positions were NOT reserved
class(reserved_women_yes)

round(mean(reserved_women_yes$water), 2) #mean number of number of new or repaired drinking water facilities in the GPs reserved for women.
round(mean(reserved_women_no$water), 2) #mean number of number of new or repaired drinking water facilities in the GPs not reserved for women.
round(sd(reserved_women_yes$water), 2) #sd of number of new or repaired drinking water facilities in the GPs reserved for women.
round(sd(reserved_women_no$water), 2) 
hist(reserved_women_yes$water) # barchart
hist(reserved_women_no$water) # barchart
boxplot(reserved_women_yes$water, reserved_women_no$water)

#Bivariate regressoin of means
lm(reserved_women_yes$water ~ reserved_women_no$water, data=regressMat) #nope

#Question 3

data(fruitfly)
?fruitfly # explanation of the dataset 'fruitfly'
str(fruitfly) #overview of dataset
summary(fruitfly) #overview of dataset
View(fruitfly)
hist(fruitfly$longevity,
     main = "Histogram of the longevity of fruitflies",
     xlab = "Lifespan in days") # mapping a histogram of the longevity in a barchart

plot(fruitfly$longevity, fruitfly$thorax,
     main = "Scatter Plot of lifespan vs thorax",
     xlab = "Fruitfly lifespan",
     ylab = "Thorax") 
#plots the relationship between longevity and thorax

#measuring the corrleation between longevity and thorax
cor(fruitfly$longevity, fruitfly$thorax)

fruitfly_lm <- lm(longevity ~ thorax, data = fruitfly) #assigns
summary(fruitfly_lm)
str(fruitfly_lm)
class(fruitfly_lm)

load(ggplot())
#a ggplot view of longevity and thorax
ggplot(aes(x = longevity, y = thorax), data = fruitfly) +
        geom_point()

#a ggplot view of longevity and thorax with a regression line
ggplot(aes(longevity, thorax), data = fruitfly) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x)

#we can plot our residuals!
fruitfly_lm_resid <- resid(fruitfly_lm) # extracting the residuals

# Q3 Pt5 - 90% confidence interval 
confint(fruitfly_lm, level = .9)

#Predicting the lifespan of a fruitfly when thorax = 0.8
class(fruitfly_lm)
fruitfly_ndf <- data.frame(thorax = c(runif(.94, .64, 30))) #making a random vector 
#with similar characteristics to thorax but not something is wrong :-(

plot(fruitfly_ndf, predict(fruitfly_lm, newdata = fruitfly_ndf)) 
