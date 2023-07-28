# https://bijean.io/blog/tutorials/anesrake/anesrake-tutorial/
### Only have to install packages once! ###
install.packages("dplyr")
install.packages("survey")
install.packages("tidyverse")
install.packages("weights")
install.packages("anesrake")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("pastecs")
install.packages("survey")
library(survey)
library(dplyr)
library(survey)
library(weights)
library(anesrake)
library(tidyverse)

#Set working directory to tell the script where to look for files
setwd("/Users/ericwiemer/Documents/CitizenData/CTCL")

#Import the .csv dataset
df <- read.csv("CTCL_July Survey_numeric raw data.csv")
df <- as.data.frame(df)

#Find missing data in columns we're weighting on
missing_rows <- complete.cases(df$demo_race, df$demo_age_buckets, df$demo_edu, df$demo_party)

#Get rid of missing rows
df_clean <- df[missing_rows,]

#Show unique values of a variable
#unique(df$demo_race)

# gender: 1=male, 2=female, 999=self-describe
# ___
# ideology: 1=very conservative, 2=somewhat conservative, 3=moderate, 4=somewhat liberal,
# 5=very liberal, 999=not sure
# ___
# party: 1=republican, 2=ind-rep, 3=nonpartisan/ind, 4=ind-dem, 5=democrat,
# 6=other party, 999=not sure
# ___
# education: 1=less than bach, 2=bach, 3=grad, 999=not sure
# ___
# race: 1=white, 2=hispanic, 3=black, 4=asian, 5=american indian/alaska native,
# 6=middle eastern, 7=hawaiian/pacific islander, 8=multiracial, 9=other, -999=not sure
# ___
# age_buckets: 1=18-24, 2=25-34, 3=35-44, 4=45-54, 5=55-64, 6=65+

#Recode
df_clean <- df_clean %>% mutate(demo_edu = factor(demo_edu,
                                                  levels = c(1, 2, 3, 4, 5, 999),
                                                  labels = c(1, 1, 1, 2, 3, 999)))

df_clean <- df_clean %>% mutate(demo_race = factor(demo_race,
                                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, -999),
                                                  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 999)))

#Show unique values of a variable
#unique(df_clean$demo_race)

#Convert demographic variables to factors with numeric values
df_clean$demo_race <- as.numeric(as.factor(df_clean$demo_race))
df_clean$demo_age_buckets <- as.numeric(as.factor(df_clean$demo_age_buckets))
df_clean$demo_edu <- as.numeric(as.factor(df_clean$demo_edu))
df_clean$demo_party <- as.numeric(as.factor(df_clean$demo_party))

#Create a unique ID column
df_clean$id <- 1:length(df_clean$demo_age)

#Set weighting proportions for all values in weighting variables
### NOTE none of these proportions can be 0 ###

#demo_race <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 999)
demo_race_prop <- c(.6923, .1080, .1167, .0237, .0077, .0037, .0037, .0225, .0189, .0028)
#demo_age <- c(1, 2, 3, 4, 5, 6)
demo_age_prop <- c(.0617, .1630, .1610, .1547, .1853, .2743)
#demo_edu <- c(1, 2, 3, 999)
demo_edu_prop <- c(.4637, .3163, .2087, .0113)
#demo_party <- c(1, 2, 3, 4, 5, 6, 999)
demo_party_prop <- c(.2707, .1003, .1510, .1243, .3067, .0283, .0187)

#Compile weighting variables into a variable called "targets"
targets <- list(demo_race_prop, demo_age_prop, demo_edu_prop, demo_party_prop)

#Name the targets the exact same names as the variable names in the dataset
names(targets) <- c("demo_race", "demo_age_buckets", "demo_edu", "demo_party")
outsave <- anesrake(targets, df_clean, caseid = df_clean$id,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 4,
                    iterate = TRUE , force1 = TRUE)

#Add a column to your dataset for the newly calculated weights
df_clean$weights <- outsave$weightvec

#Function to assign experimental groups to each participant
assign_ex_group <- function(x) {
  ifelse(is.na(x) | x == "", 2, 1)
}

#Apply the function to create the ex_group column where 1=A group, 2=B group
df_clean$ex_group <- assign_ex_group(df_clean$A_message1)

#Function to calculate summary statistics
calculate_summary_stats <- function(variable) {
  frequency <- table(variable)
  percentage <- prop.table(frequency) * 100
  totals <- sum(frequency)
  summary_stats <- data.frame(Frequency = frequency,
                              Percentage = paste0(format(percentage,
                                                         digits = 2),
                                                  "%"),
                              Total = totals)
  return(summary_stats)
}

#Calculate summary statistics for each variable
stats_race <- calculate_summary_stats(df_clean$demo_race)
stats_age <- calculate_summary_stats(df_clean$demo_age_buckets)
stats_edu <- calculate_summary_stats(df_clean$demo_edu)
stats_party <- calculate_summary_stats(df_clean$demo_party)

#Save the weighted dataset to a new .csv file
write.csv(df_clean, "CTCL_July Survey_weighted.csv")

################## ANALYSIS ##########################

library(dplyr)
library(survey)
library(weights)
library(anesrake)
library(tidyverse)
library(pastecs)
library(survey)

#Set working directory to tell the script where to look for files
setwd("/Users/ericwiemer/Documents/CitizenData/CTCL")

#Import the .csv dataset
df <- read.csv("CTCL_July Survey_weighted.csv")
df <- as.data.frame(df)

#Instantiate survey design to apply the weights
design <- svydesign(ids = ~id, weights = ~weights, data = df)

#Function to calculate summary statistics for a survey design
summary_stats <- function(design, variable_name) {
  variable_formula <- as.formula(paste0("~", variable_name))
  frequency <- svytable(variable_formula, design)
  percentage <- prop.table(frequency) * 100
  totals <- sum(frequency)
  summary_stats <- data.frame(Frequency = frequency,
                              Percentage = paste0(format(percentage, digits = 2), "%"),
                              Total = totals)
  return(summary_stats)
}

#Calculate summary statistics
stats_race <- summary_stats(design, "demo_race")
stats_age <- summary_stats(design, "demo_age_buckets")
stats_party <- summary_stats(design, "demo_party")
stats_edu <- summary_stats(design, "demo_edu")
stats_race
stats_age
stats_party
stats_edu

#Conduct the t-test
ttest <- svyttest(post_messenger ~ ex_group, design=design)
ttest