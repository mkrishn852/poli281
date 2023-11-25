# POLI 281 Assignment 5 - 4/27/2022
# Maya Krishnamoorthy

rm(list = ls())
setwd("~/Documents/poli281/assignments")
stops2 <- read.csv("stops2.csv")

# Code for Q1
# create linear regression model
fit1 <- lm(stops2$searchoccur ~ stops2$age)
fit1
# print summary
summary(fit1)
# print number of observations
nobs(fit1)
# equation
45*(-0.0062) + 0.537 # 0.258

# Code for Q2
max(stops2$age) # 60
min(stops2$age) # 18
stops2$age_r <- (stops2$age - 18)/42
# create linear regression model
fit2 <- lm(stops2$searchoccur ~ stops2$age_r)
summary(fit2)
nobs(fit2)
# equation
(45-18)/42 * -0.261 + 0.426 # 0.2582

# Code for Q3
# create new column called male and if gender == Male, assign value to 1. else, assign to 0.
stops2$male <- ifelse(stops2$gender == "Male", 1, 0)
# use correlation function from the textbook to correlate two variables
cor(stops2$searchoccur, stops2$searchoccur) # 1
cor(stops2$searchoccur, stops2$age_r) # -0.1322639
cor(stops2$searchoccur, stops2$Berry) # -0.006407295
cor(stops2$searchoccur, stops2$male) # 0.2301162
cor(stops2$age_r, stops2$Berry) # 0.00303759
cor(stops2$age_r, stops2$male) # -0.0175985
cor(stops2$Berry, stops2$male) # -0.01144756

# Code for Q4
# male drivers
stops2m <- subset(stops2, stops2$male == 1)
fit3 <- lm(stops2m$searchoccur ~ stops2m$age_r)
summary(fit3)
nobs(fit3)

# female drivers
stops2f <- subset(stops2, stops2$male == 0)
fit4 <- lm(stops2f$searchoccur ~ stops2f$age_r)
summary(fit4)
nobs(fit4)

# Code for Q5
# IV: age and male == 1
fit5 <- lm(stops2$searchoccur ~ stops2$age_r + stops2$male)
summary(fit5)
nobs(fit5)
# IV: age and Berry
fit6 <- lm(stops2$searchoccur ~ stops2$age_r + stops2$Berry)
summary(fit6)
nobs(fit6)
# IV: age and male == 1 and Berry
fit7 <- lm(stops2$searchoccur ~ stops2$age_r + stops2$male + stops2$Berry)
summary(fit7)
nobs(fit7)

# Code for Q6