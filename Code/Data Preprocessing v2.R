# Reading in libraries
library(ggplot2)
library(rms)
library(arm)
library(e1071)
library(caret)
library(pROC)
# library(ggdark)
library(ggeasy)
library(dplyr)
# library(tidyverse)

# Reading in datasets
getwd()
#setwd("TeamProject2/team-project-2-streetrx-and-voting-in-nc-go-blue/Data/")
voter <- read.csv("voter_stats_20201103.txt",  sep = '\t', skipNul = T)
history <- read.csv("history_stats_20201103.txt", sep = '\t')

# Preliminary check of the datasets
head(voter)
summary(voter)
str(voter)

head(history)
summary(history)
str(history)

# Dropping unwanted columns from both datasets
drop <- c("election_date","stats_type","update_date")
voter <- voter[, !names(voter) %in% drop]
history <- history[, !names(history) %in% drop]

# Aggregating the history dataset to match the level 
# (of uniqueness of obs) of the voter dataset
agg_history <- aggregate(list(turnout=history$total_voters), 
                         list(county_desc=history$county_desc, 
                              party_cd=history$voted_party_cd,
                              age=history$age, race_code=history$race_code,
                              ethnic_code=history$ethnic_code, 
                              sex_code=history$sex_code,
                              precinct_abbrv=history$precinct_abbrv, 
                              vtd_abbrv=history$vtd_abbrv), sum)

# Merging the two datasets on the following cols:
# county_desc,	precinct_abbrv,	vtd_abbrv,	party_cd,	
# race_code,	ethnic_code,	sex_code,	age
merged <- left_join(voter, agg_history, by = NULL, copy = FALSE)

# Replacing nulls in turnout by zeroes
merged$turnout[is.na(merged$turnout)] <- 0

# quick QC on the voter numbers before and after merge --> match!
agg_voter <- aggregate(voter$total_voters,
                       list(Age=voter$age,Party=voter$party_cd),sum)
agg_data <- aggregate(merged$total_voters,
                             list(Age=merged$age,Party=merged$party_cd),sum)

# Checking the proportion of voters who turned up to vote --> ~75%
sum(merged$turnout)/sum(merged$total_voters)

# Taking a sample of 25 counties from the entire dataset 
# and filtering dataset on those counties
set.seed(123) #set your own seed to be able to replicate results
all_counties <- unique(merged[c("county_desc")])
county_sample <- merged$county_desc[c(sample(all_counties$county_desc,
                                             size=25,replace=F))]
merged_reduced <- merged[is.element(merged$county_desc,county_sample),]

merged_reduced$turnout_rate <- merged_reduced$turnout/merged_reduced$total_voters

