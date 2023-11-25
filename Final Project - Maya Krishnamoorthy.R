# POLI 281 Final Project
# Maya Krishnamoorthy

# clear environment, set up directory
rm(list = ls())
setwd("~/Documents/poli281/assignments")
cces <- read.csv("cces18.csv")

# Question 2: How long do people wait?
prop.table(table(cces$wait))  # proportion of people who responded about their wait times
summary(cces$wait)  # find mean and median wait times

cces$longwait <- recode(cces$wait, "1" = F, "2" = F, "3" = F, "4" = T, "5" = T, "6" = NA)

# Question 3: Long waits by state
# import libraries
library(dplyr)
library(ggplot2)
wait_by_state <- cces %>%  # find proportion of voters who reported a long wait time in each state
  group_by(state) %>% # group by state
  summarize(longwait_mean = (mean(longwait, na.rm = TRUE))) # proportion mean of people who reported a long wait
wait_by_state

# create plot where order is highest to lowest proportion of wait times in each state
wait_by_state_graph <- ggplot(wait_by_state, aes(x = reorder(state, -longwait_mean), y = longwait_mean, fill = state)) +  geom_col() + 
  ylab("Proportion of Waiting Times Over 30 Minutes") +
  xlab("States") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))
wait_by_state_graph # print graph

# Question 4: Waiting times by state and region
wait_by_sr <- cces %>%  # find proportion of voters who reported a long wait time in each state and region
  group_by(state, region) %>% # group by state and region
  summarize(longwait_mean = (mean(longwait, na.rm = TRUE))) # proportion mean of people who reported a long wait
wait_by_sr
# create graph where states are organized into regions (check which region using legend)
wait_by_sr_graph <- ggplot(wait_by_sr, aes(x = reorder(state, -longwait_mean), y = longwait_mean, fill = region)) +  geom_col() + 
  ylab("Proportion of Waiting Times Over 30 Minutes") +
  xlab("States (grouped by Regions)") +
  theme(axis.text.x = element_text(angle = 90))
wait_by_sr_graph 

# Question 5: Waiting times by prior vote
# recode vote2016 variable to describe voter as Conservative, Liberal, or Other based on how they voted in 2016
cces$conserv_vote <- recode(cces$vote2016, "1" = "Conservative", "2" = "Liberal", "3" = "Other", "4" = "Other", "5" = "Other")
# table reports proportion of people who waited a long time to vote, depending on whether they are liberal or conservative
prop.table(table(cces$conserv_vote, cces$longwait), margin=1)

# Question 6: Waiting times by race
# create a race_5 variable that groups respondents into 5 racial categories
cces$race_5 <- recode(cces$race, "1" = "White Non-Hispanic", "2" = "Black", "3" = "Hispanic", "4" = "Asian", "5" = "Other", "6" = "Other", "7" = "Other", "8" = "Other")
cces$race_5 <- as.factor(cces$race_5)

# create a variable that measures whether voters waited for longer than 10 minutes
cces$wait_10 <- recode(cces$wait, "1" = FALSE, "2" = FALSE, "3" = TRUE, "4" = TRUE, "5"= TRUE, "6" = NA)

# find proportion of voters who reported a wait time over than 10 minutes, grouping them by racial category
more_than_10_minutes <- cces %>%
  group_by(race_5) %>%
  summarize(more_than_10_mean = mean(wait_10, na.rm=T)) # evaluates average for each group
more_than_10_minutes

# bar graph that reports the proprotions of wait time > 10 minutes, categorizing on race
more_than_10_graph <- ggplot(more_than_10_minutes, aes(x = reorder(race_5, -more_than_10_mean), y = more_than_10_mean, fill=race_5)) + geom_col() + 
  ylab("Proportion of Waiting Times Over 10 Minutes") + 
  xlab("Race")
more_than_10_graph
  
# Question 7: Income as a potential confounder
# recode faminc into new variable faminc_4 that groups by income
cces$faminc_4 <- NA
cces$faminc_4[cces$faminc < 6] <- 1 #variables under 50K 
cces$faminc_4[cces$faminc > 5 & cces$faminc < 10] <- 2 #over 50K 
cces$faminc_4[cces$faminc > 9 & cces$faminc < 13] <- 3 #over 100K
cces$faminc_4[cces$faminc > 12 & cces$faminc != 97] <- 4 #over 200K
cces$faminc_4[cces$faminc == 97] <- NA

# recode into factor variable with labels
cces$faminc_4 <- as.factor(cces$faminc_4)
cces$faminc_4 <- recode(cces$faminc_4, "1" = "Under 50K" , "2" = "Over 50K", "3" = "Over 100K", "4" = "Over 200K") 

# table shows what proportion of respondents falls into each of the income categories
prop.table(table(cces$faminc_4))

# Question 8: Examining income
# find proportion of voters who reported long wait times grouped by income
income_wait <- cces %>% 
  group_by(faminc_4) %>% 
  summarize(longwait = mean(longwait, na.rm = TRUE))

# make a bar graph that compares proportions of waiting times over 30 minutes by income group
income_wait_graph <- ggplot(income_wait, aes(x = reorder(faminc_4, longwait),y = longwait, fill = faminc_4)) +
  geom_col() + 
  ylab("Proportion of Waiting Times Over 30 Minutes") +  
  xlab("Income Group") + 
  theme(legend.position = "none")
income_wait_graph

# make a stacked bar graph that shows income distribution by race
income_by_race <- ggplot(cces, aes(x = "", y = faminc_4, fill = race_5)) + 
  geom_bar(stat = "identity", width = 1) + 
  facet_grid(.~faminc_4) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  xlab("Income") + 
  ylab("Income Distribution by Race") + 
  labs(fill = "Race Key")
income_by_race

# Question 9: Using subclassification to Account for the Effect of Income
# find average proportion of voters who reported long wait time (> 30 mins), grouping by race and income
income_wait_race_cat <- cces %>% 
  group_by(faminc_4, race_5) %>%
  summarize(longwait_mean = mean(longwait, na.rm = TRUE))

# creates multiple bar graphs - each graph with one income group and all race categories
effect_of_income_graph <- ggplot(income_wait_race_cat, aes(x = reorder(race_5, -longwait_mean), y = longwait_mean, fill = race_5)) + 
  geom_col() + 
  ylab("Proportion of Voters Who Waited for Over 30 Minutes") +
  xlab("Income Groups") + 
  theme(axis.text.x = element_blank()) +
  facet_grid(.~faminc_4)
effect_of_income_graph

# Question 10: Using Regression to Account for Multiple Confounders Simultaneously: Part 1
# recode county socioeconomic status variable so that we basically remove any values above $150,000
summary(cces$income_county)
cces$income_county[cces$income_county > 150] <- NA
summary(cces$income_county)

# create histogram that counts up the average income of voters within a county
avg_income_by_county <- ggplot(cces, aes(x = income_county, fill = income_county)) + 
  geom_histogram(color = "black", fill = "plum4", binwidth = 10) + 
  ylab("Total Observations") + 
  xlab("Average Income of Voters Within a County")
avg_income_by_county

# create a new variable measuring population density and recode variable so that values above 15 are ignored
cces <- cces %>% 
  mutate(pop_density = (county_pop/1000)/land_area)
summary(cces$pop_density)
cces$pop_density[cces$pop_density > 15] <- NA 
summary(cces$pop_density)

# histogram counting observations of population density in the counties
pop_density_graph <- ggplot(cces, aes(x = pop_density, fill = pop_d)) + 
  geom_histogram(color="black", fill="plum4", binwidth=1) + 
  ylab("Observations") + 
  xlab("Population Density (Thousand Ppl Per Sq Mile") 
pop_density_graph

# recode race_5 variable separating black, hispanic, asian, and other voters
cces$black <- recode(cces$race_5, "White Non-Hispanic" = 0, "Black" = 1, "Hispanic" = 0, "Asian" = 0, "Other" = 0)
cces$hispanic <- recode(cces$race_5, "White Non-Hispanic" = 0, "Black" = 0, "Hispanic" = 1, "Asian" = 0, "Other" = 0)
cces$asian <- recode(cces$race_5, "White Non-Hispanic" = 0, "Black" = 0, "Hispanic" = 0, "Asian" = 1, "Other" = 0)
cces$other <- recode(cces$race_5, "White Non-Hispanic" = 0, "Black" = 0, "Hispanic" = 0, "Asian" = 0, "Other" = 1)

# create a new variable "wait_reg" that replaces the wait == 6 with NA
cces <- mutate(cces, wait_reg = wait)
cces$wait_reg[cces$wait_reg == 6] <- NA

# create a new variable "faminc_reg" that replaces faminc == 97 with NA
cces <- mutate(cces, faminc_reg = faminc)
cces$faminc_reg[cces$faminc_reg == 97] <- NA 

# Question 11: Using Regression to Account for Multiple Confounders Simultaneously: Part 2
# Model 1: Regress waiting times on Black, Hispanic, Asian and other
mod1 <- lm(formula = cces$wait_reg ~ cces$black + cces$hispanic + cces$asian + cces$other)
summary(mod1)
nobs(mod1)
# Model 2: Regress waiting times on Black, Hispanic, Asian, other, personal income, county socioeconomic status and population density
mod2 <- lm(formula = cces$wait_reg ~ cces$black + cces$hispanic + 0cces$asian + cces$other + cces$faminc_reg + cces$income_county + cces$pop_density)
summary(mod2)
nobs(mod2)
