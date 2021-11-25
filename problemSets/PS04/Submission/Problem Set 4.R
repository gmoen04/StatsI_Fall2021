#Problem Set 4 - Submission 26th November ####

install.packages(car)
library(car)
data(Prestige)
library(stargazer) # to get well-formatted tables of results

#some summary information about the data set
summary(Prestige)
head(Prestige)
tail(Prestige)

#load tidyverse
library(tidyverse)

## Question 1 ####
### (a) ####
#creating a new empty "professional" column
Prestige$professional <- NA

#filling the column with data
Prestige$professional <- as.factor(case_when(
  Prestige$type == "prof" ~ "1",
  Prestige$type == "wc" ~ "0",
  Prestige$type == "bc" ~ "0",
  ))

#view the new column
view(Prestige)
class(Prestige$professional)
#type is now factor

### (b) Linear model ####
mod_1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
#view the results
stargazer(mod_1, type = "text")
# insert image of results
summary(mod_1)

# R² = 0.787 which means that the variables used add a high degree of  
# predictive power to the model and also represent a 78.7% reduction in 
# prediction error as compared to using ȳ

# a plot of prestige and income, colour coded for professional
ggplot2::ggplot(aes(x = income, y = prestige), data = Prestige) +
  geom_point(aes(colour = professional), alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle("Relationship between income and prestige")

#there is a strong relationship between being a professional and prestige

### (c) Prediction equation ####
#Prediction equation formula is "^y = a + b1x1 + b2x2 + b3x1x2"
# ^y = 21.142 + (0.003)x1 + (37.781)x2 + (-0.002)x1x2

### (d) Interpret the coefficient for income ####
# There seems to be a significant (p = 7.55e-09) but weak correlation between 
# income and prestige. A 1 unit increase in prestige corresponds to a 
# $0.003 increase in income

### (e) Interpret the coefficient for professional ####
# There seems to be a significant (p = 4.14e-14) and strong correlation between 
# professional and prestige, with a 1 unit increase in professional equaling 
# a 37.781 unit increase in prestige

### (f) Effect of $1,000 increase in income on prestige for professionals ####
# ie professional = 1
# from (c): ^y = 21.142 + (0.003)x1 + (37.781)x2 + (-0.002)x1x2
# An increase in $1000 = a (0.003)(1000) increase in prestige = 3 units

### (g) ####
# from (c): ^y = 21.142 + (0.003)x1 + (37.781)x2 + (-0.002)x1x2

# Now substituting $6000 for income and 1 for professional

# 21.142 + (0.003)(6000) + (37.781)(1) + (-0.002)(6000)(1)
# 21.142 + 18 + 37.781 - 12
# 39.142 (non-professional prestige level at $6000) + 25.781 (increase in 
# prestige level of professional with $6000 salary)
# A switch from non-professional to professional represents a 25.781 unit 
# increase in prestige at a salary level of $6000

# drop NA values in table, didn't work
tidyr::drop_na(Prestige, professional)

# Plot of separate regression lines
ggplot2::ggplot(aes(x = income, y = prestige), data = Prestige) +
  geom_point(aes(colour = professional), alpha = 0.4) +
  geom_smooth(method = "lm", aes(colour = professional)) +
  ggtitle("Separate regression lines for professionals and non professionals")

# Plot of separate regression lines while trying to eliminate NA-value line
# didn't work
Prestige %>%
  tidyr::drop_na(Prestige, professional) %>%
  ggplot2::ggplot(Prestige, aes(x = income, y = prestige)) +
  geom_point(aes(colour = professional), alpha = 0.4) +
  geom_smooth(method = "lm", aes(colour = professional)) +
  ggtitle("Separate regression lines for professionals and non professionals")
              
## Question 2 ####
### (a) Do yard signs affect vote share? ####
# Hypothesis test with 𝛼= 0.05 
# Hypothesis test: H₀:βᵢ = 0, using sample estimate bᵢ for βᵢ
# Hₐ:βᵢ ≠ 0
# t = bᵢ/se
# Our test statistic = (0.042)/(0.016) = 2.625
# df = n - (k + 1) = 131 - (2 + 1) = 128
# This gives us a two-tailed p-value of .00972 which tells us that being
# in a precinct with signs has an effect

### (b) ####
# Again we have a hypothesis test with 𝛼= 0.05 
# Hypothesis test: H₀:βᵢ = 0, using sample estimate bᵢ for βᵢ
# Hₐ:βᵢ ≠ 0
# t = bᵢ/se
# Our test statistic = (0.042)/(0.013) = 3.23
# df = n - (k + 1) = 131 - (2 + 1) = 128
# This gives us a two-tailed p-value of .001573 which tells us that being
# next to a precinct with signs has an effect

### (c) ####
# The constant term is .302 which is the predicted y value when both 
# 'precinct assigned law signs' and 'precinct adjacent to lawn signs' equal 0.

### (d) Model fit ####
# R² = 0.094 which means that the variables used add a low degree of  
# predictive power to the model as they only represent a 9.4% reduction in 
# prediction error as compared to using ȳ. This tells us that there are
# other variables not in the model which are likely to have a much greater
# effect than those in the model.
              