# Student Name: Maya Krishnamoorthy
# Poli281, Homework Assignment 2

rm(list=ls()) # Let's start with an empty workspace.

### Import the Dataset Programatically
setwd("~/Documents/poli281/assignments")
state_taxes <- read.csv("state_taxes.csv", stringsAsFactors = TRUE)

### Code for Question 1:
substance_tax <- c(state_taxes$Tobacco_Tax + state_taxes$Alcoholic_Beverage_Tax) # create a vector storing sum of tobacco & alcoholic tax for each state/year
state_taxes <- cbind(state_taxes, substance_tax) # added substance tax vector to state_taxes data frame as a column

state_taxes <- state_taxes[order(state_taxes$substance_tax, decreasing = TRUE),] # re-order data frame in decreasing order based on value of substance tax

### Code for Question 2:
low_tax <- subset(state_taxes, state_taxes$substance_tax < 100000) # create a subset of state_taxes data frame that only includes states/years with substance tax < $100,000
nrow(low_tax) # count number of rows in new subset data frame

### Code for Question 3:
perc_sub <- c((state_taxes$substance_tax / state_taxes$Total_Revenue) * 100) # create vector storing percent of substance tax affecting total revenue for each state/year
state_taxes <- cbind(state_taxes, perc_sub) # add perc_sub to state_taxes data frame
state_taxes <- state_taxes[order(state_taxes$perc_sub, decreasing = TRUE),] # re-order data frame in decreasing order of percent revenue from substance tax
print(mean(state_taxes$perc_sub))

### Code for Question 4:
regionnames <- levels(state_taxes$Region) # variable storing factor levels of region column in state_taxes data frame
for (x in regionnames) { # loop through regions
  print(x) # print each region name
  rgn <- subset(state_taxes, state_taxes$Region == x) # subset of state_taxes data frame that contains all the information from state_taxes for each region
  print(paste("Average Substance Tax:", mean(rgn$substance_tax))) # finds and prints average substance tax for each region
  print(paste("Average Percent:", mean(rgn$perc_sub))) # finds and prints average percent revenue from substance tax for each region
}

### Code for Question 5:
tax_rel <- c((state_taxes$Total_Taxes / state_taxes$Total_Revenue) * 100) # creates vector storing tax reliance on revenue for each state/year
state_taxes <- cbind(state_taxes, tax_rel) # adds tax_rel to dataframe state_taxes
for (x in regionnames) { # loops through regions
  print(x) # prints each region name
  rgn <- subset(state_taxes, state_taxes$Region == x) # subset of state_taxes data frame that contains all the information from state_taxes for each region
  print(paste("Average Level of Tax Reliance:", mean(rgn$tax_rel))) # finds and prints average level of tax reliance for each region
}

### Code for Question 6:
high <- 0 # stores count of "high" tax reliance
medium <- 0 # stores count of "medium" tax reliance
low <- 0 # stores count of "low" tax reliance
for (x in 1:nrow(state_taxes)) { # loops through rows in state_taxes dataframe
  if (state_taxes$tax_rel[x] > 40 & state_taxes$Year[x] == 2012) { # increase high count if tax_rel > 40 and the year is 2012
    high = high + 1
  }
  if (state_taxes$tax_rel[x] >= 33 & state_taxes$tax_rel[x] <= 40 & state_taxes$Year[x] == 2013) { # increase medium count if 33 <= tax_rel <= 40 and year == 2013
    medium = medium + 1
  }
  if (state_taxes$tax_rel[x] < 33 & state_taxes$Year[x] == 2012 & state_taxes$Region[x] == "South") { # increase low count if tax_rel < 33 and year is 2012 and state is in the South
    low = low + 1
  }
}
low_high = 0 # stores low OR high tax reliance count
south_subset <- subset(state_taxes, state_taxes$Region == "South") # create subset of southern region
for (x in 1:nrow(south_subset)) { # loop through south subset to fit constraints of question - tax_rel < 33 and 2012 OR tax_rel > 40 and 2012
  if (south_subset$tax_rel[x] < 33 & south_subset$Year[x] == 2012 | south_subset$tax_rel[x] > 40 & south_subset$Year[x] == 2012) {
    low_high = low_high + 1 # increment count if conditional is true
  }
}
print(paste("high", high, "medium", medium, "low", low, "low/high", low_high)) # print out results
