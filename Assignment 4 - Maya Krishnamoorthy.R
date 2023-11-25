## Name: Maya Krishnamoorthy
# Poli281 Assignment 4

# Setup
library(dplyr)
library(ggplot2)

# Import Traffic Stops Data
rm(list=ls())
setwd("~/Documents/poli281/class notes & group work") # You'll need to update this.
stops <- read.csv("stops.csv")

# Code for Q1:
stops$month <- factor(stops$month, labels = month.name) # This line and the next one get the factor structure of months set up right.
stops$month <- ordered(stops$month, levels = rev(month.name))

# create new dataset that only takes in month and tally of stops per month
new.graph <- stops %>%
  group_by(month) %>%
  tally()

# creates basic bar graph plot where x-axis contains month category and y-axis takes tally
plot <- ggplot(new.graph, aes(x=month, y=n, fill=month)) + geom_col() + theme(legend.position = "none")
plot

# edits plot so that axes are switched and gives the tally variable a label - "Number of Stops for Traffic Violations"
plot <- plot +
  coord_flip() +
  ylab("Number of Stops for Traffic Violations")
plot

## Load jobs data:
rm(list = ls())
jobs <- read.csv("~/Documents/poli281/assignments/jobs.csv")  # Import jobs dataset


## Code for Q2:

# create a new dataset that takes in a new variable, f_pct_m, which is the female earnings as a proportion of male earnings
jobs <- mutate(jobs, f_pct_m = total_earnings_female / total_earnings_male)

# find weighted average of f_pct_m for each major category
jobs_categories <- jobs %>%
  group_by(major_category) %>%
  summarize(results = weighted.mean(f_pct_m, total_workers, na.rm=T))

# create bar graph of wage gap between male and female workers in broad categories 
# graphing their relative proportions
plot_jobs <- ggplot(jobs_categories, aes(x=reorder(major_category, results), y=results, fill=major_category)) + 
  geom_col() + 
  theme(legend.position = "none")
plot_jobs

# fancy version of the graph - flips coordinates, labels x and y, and creates a comparison line at y = 1.
edited_jobs_plot <- plot_jobs +
  coord_flip() + 
  xlab("Major Category") +
  ylab("Weighted Average of Proportion of Female Earnings to Male Earnings") +
  geom_hline(yintercept = 1)
edited_jobs_plot

## Code for Q3
# creates smaller data set that filters to the Computer, Engineering, and Science category
computer <- jobs %>%
  filter(major_category=="Computer, Engineering, and Science")

# add prop_male to computer data frame
computer <- mutate(computer, prop_male = workers_male/total_workers)

# This is removing two outliers that make the figure ugly:
computer2 <- computer %>% 
  filter(prop_male > 0) %>%
  filter(f_pct_m < 2)

# create basic scatterplot comparing prop_male on x-axis to the average weighted wage gap on y-axis
computer_plot <- ggplot(computer2, aes(prop_male, f_pct_m)) + geom_point()
computer_plot

# fancy version of the computer plot - adds a y-intercept line at y=1 for comparison, labels the graph, creates a line of best fit
fancy_computer_plot <- computer_plot +
  geom_hline(yintercept = 1) + 
  ylab("Average Weighted Wage Gap by Gender") + 
  xlab("Proportion of Male Workers") + 
  stat_smooth(method="lm", se=FALSE)
fancy_computer_plot
