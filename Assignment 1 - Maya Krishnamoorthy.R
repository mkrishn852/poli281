# Student Name: Maya Krishnamoorthy
# Section (11am or 2pm): 11am
# Poli281, Spring 2022, Homework Assignment 1

rm(list=ls()) # Let's start with an empty workspace.

### Code for Question 0: Import the Dataset
# Note: See the document "Programmatic Importing in R" under on Sakai for a refresher on how to do this. Please make sure to import programmatically. This is part of the assignment.
setwd("~/Documents/poli281/assignments")
cong_votes <- read.csv("house_votes2020.csv", stringsAsFactors = TRUE)

### Code for Question 1:
# Note: The easiest way to do this is with a command we haven't explicitly learned yet. But I bet you can figure out how to use it here. The command you want is "nrow()" All you need to do is figure out what belongs in the parentheses.
nrow(cong_votes)  # tells us there are 435 rows in the dataset


### Code for Question 2:
# Note: To answer this question, place the California-related rows alone in an object called "california". Then, count the number of rows in that object. You might want to review the Datacamp lesson focused on the subset() command.
nrow(subset(cong_votes, cong_votes$State == "California")) # tells us there are 53 districts in CA

### Code for Question 3:
cong_votes[288, 3] # cell with "Price, David E."

### Code for Question 4:
other_votes <- sum(cong_votes[,"Other"]) # adds up 3566362 votes for "Other" candidates

### Code for Question 5:
total_votes <- sum(cong_votes[,"Dem"], cong_votes[,"Rep"], other_votes) # total votes from registered voters was 152758402

### Code for Question 6:
dem_percent_votes <- sum(cong_votes[,"Dem"]) / total_votes # 50.08% of all votes went to Democratic candidates

### Code for Question 7:
fresh_dems <- sum(subset(cong_votes$Party == "D", cong_votes$Fresh == 1)) / sum(cong_votes[,"Party"] == "D") # proportion of Democrats that were freshmen was 6.76%

### Code for Question 8:
# Note: For this question, remember that, when R evaluates conditional statements, it treats TRUE values as the number 1, and FALSE statements as the number 0. See the first three lines below. You want to extend that approach to apply to cong_votes
example <- factor(c("Dog", "Cat", "Dog", "Dog"))
example == "Dog"
mean(example == "Dog") # Evaluates to .75.
# Explanation of the above:
sum(c(TRUE, FALSE, TRUE, TRUE)) # Can see here that R is treating each TRUE as a 1, each FALSE as a zero. So...
mean(example == "Dog") # evaluates to .75, since R has substituted 1, 0, 1, 1 and taken the average.

dem_seats <- mean(cong_votes["Party"] == "D") # proportion of seats won by Democrats is 51.03%

### NOTES FOR QUESTIONS 9 & 10:
# Mario Diaz-Balart in Florida 25 ran unopposed and so had no votes cast for or against him. To make your life easier for the remaining questions, we are deleting him from the dataset. To do this, run the code below:
cong_votes <- cong_votes[cong_votes$Winner!="Diaz-Balart, Mario",]

### Code for Question 9
pDem <- c(cong_votes[,"Dem"] / (cong_votes[,"Dem"] + cong_votes[,"Rep"])) # save proportion of Dems based on new equation to variable pDem
cong_votes$pDem <- pDem # add column pDem to cong_votes data frame
avg_pDem <- mean(cong_votes$pDem) # find mean value of two-party proportion Dems
avg_pDem*435 -dem_seats*435 # compare the number of seats - multiplied number of seats by proportion of seats won

### Code for Question 10
dem_change <- pDem - 0.05 # reduce proportion of Dem votes by 0.05
cong_votes <- cbind(cong_votes, dem_change) # add the new proportion to the data frame
new_seats <- sum(cong_votes$dem_change >= 0.5) # number of seats won by Dems with the new proportion of votes in each district
seat_percent <- new_seats / 435 # proportion of seats won by Dems with the new district votes

### Code for Question 11
nc_votes <- subset(cong_votes, cong_votes$State == "North Carolina") # subset of cong_votes data set that only includes NC districts
dem_percent_nc <- sum(nc_votes["Party"] == "D") / nrow(nc_votes) # percent of Democrats in NC congressional delegation

majority_prop <- sum(nc_votes$pDem >= 0.5) # guess value of 0.05 increase to the proportion of the two-party vote and check if that gives the Dems a majority (7 seats)
majority_prop <- sum(nc_votes$pDem + 0.06 >= 0.5) # 0.05 did not lead to majority, try 0.06
dem_majority <- nc_votes$pDem + 0.06 # add 0.06 to each proportion in the data frame to check if there are 7 rows above 0.5 - which woulld give them the majority
nc_votes <- cbind(nc_votes, dem_majority) # compare in the data frame - yes, there are 7 seats held by Dems
