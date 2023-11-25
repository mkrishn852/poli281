# Poli281 Assignment 3, Part 1
# Name: Maya Krishnamoorthy

# Code to import data for Part 1 (Need to update this):
rm(list=ls())
setwd("~/Documents/poli281/assignments")
star <- read.csv("STAR.csv")

# add dplyr library
library(dplyr)

# Code for Q1.1
# create kinder as a factor variable annd add it to the data set
star$kinder <- NA
star$kinder[star$classtype == 1] <- "small" # if data type == 1, kinder == "small"
star$kinder[star$classtype == 2] <- "regular"
star$kinder[star$classtype == 3] <- "regular with aid"
star$kinder <- as.factor(star$kinder) # changes to factor variable
table(star$kinder) # pritns out table with count of students in each type of class

# Code for Q1.2
# changes race variable to have more semantic meaning
star$race[star$race == 1] <- "white" # if race == 1, then change it to mean that race == white
star$race[star$race == 2] <- "black"
star$race[star$race == 4] <- "hispanic"
star$race[star$race == 3] <- "other"
star$race[star$race == 5] <- "other"
star$race[star$race == 6] <- "other"
star$race <- as.factor(star$race) # change race to a factor variable
table(star$race, useNA = "always") # prints out count of students in each level

# Code for Q1.3
reading_mean <- tapply(star$g4reading, star$kinder, mean, na.rm=T) # print out reading test mean by grouping g4reading scores by kinder levels
math_mean <- tapply(star$g4math, star$kinder, mean, na.rm=T) # print math test mean by grouping g4math scores by kinder levels

# difference
small_rdiff <- 723.39 - 719.89 # reading difference of means in variable small_rdiff to show difference between small and regular class
rwa_rdiff <- 720.72 - 719.89 # reading score difference of means between regular with aid and regular classes
small_mdiff <- 709.19 - 709.52 # math score difference of means between small and regular classes
rwa_mdiff <- 707.63 - 709.52 # math score differennce of meansn between regular with aid and regular classes

# standard deviations
reading_sd <- sd(star$g4reading, na.rm=T) # 52.4 = standard deviation for all reading test scores (no class type differentiation)
math_sd <- sd(star$g4math, na.rm=T) # 43.09 = standard deviation for all math test scores

# standard deviation for each score
small_rdiff / reading_sd # prints proportion of standard deviation in small class reading scores
rwa_rdiff / reading_sd # prints proportion of standard deviation in regular with aid class reading scores
small_mdiff / math_sd # prints proportion of standard deviation in small class math scores
rwa_mdiff / math_sd # prints proportion of standard deviation in regular with aid class math scores

# Code for Q1.4
prop.table(table(star$kinder, star$yearssmall)) # prints table of proportions where each cell represents the percent of students in each type of class for 0-4 years
tapply(star$g4reading, star$yearssmall, mean, na.rm=T) # prints mean reading score for each category of years in small classes

# Code for Q1.5
reading_w <- star %>% # filter data set so that we look at white students and sort their reading score by class type
  filter(race == "white") %>%
  group_by(kinder) %>%
  summarise(rscore_wavg = mean(g4reading, na.rm=T)) # print average reading score within these "filters"

reading_m <- star %>% # filter data set so that we look at minority students and sort their reading score by class type
  filter(race == "black" | race == "hispanic") %>%
  group_by(kinder) %>%
  summarize(rscore_mavg = mean(g4reading, na.rm=T)) # print average reading score within these "filters"

math_w <- star %>% # filter data set so that we look at white students and sort their math score by class type
  filter(race == "white") %>%
  group_by(kinder) %>%
  summarise(mscore_wavg = mean(g4math, na.rm=T)) # print average math score within these "filters"

math_m <- star %>% # filter data set so that we look at minority students and sort their math score by class type
  filter(race == "black" | race == "hispanic") %>%
  group_by(kinder) %>%
  summarise(mscore_mavg = mean(g4math, na.rm=T)) # print average math score within these "filters"