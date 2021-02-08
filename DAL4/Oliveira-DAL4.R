# Title: ####
# Author: Cierra Oliveira
# Date Created: 2/3/2021

# Set Up####
# Install Packages
install.packages("tidyverse")

# Libraries
library(tidyverse)

# Data 
# Load your Data and assign to gss_df ####
gss_df <- read.csv("gss_df.csv")

# OR you can load .Rdata file like
# gss_df <- load("path/to/file/gss_df.Rdata)


# Script ####

# Question A ####
# View the data and read the code book to answer the following question. What do the columns and rows represent. Assign you answer as a text vector the below object. 
AnswerA <- "The rows are individual respondents to the survey. The columns represent variables or questions answered by the responder."

# Question B ####
# What is the start and end year present in the data? Code as a NUMERIC vector and assign to AnswerB.  Plot how many observations per year there are. Assign your code to AnswerB_plot
summary(gss_df$YEAR)
AnswerB. <- c(1972,2018)

AnswerB_plot <- gss_df %>% 
  filter(!is.na(YEAR)) %>% 
  group_by(YEAR) %>% 
  count() %>% 
  rename(observations = n, year = YEAR) %>% 
  ggplot(aes(x=year, y=observations))+
  geom_bar(stat = "identity")
  
# Question C ####
# Is every question asked each year? Use the code book or the GSS Data Explorer (link posted on Canvas) to answer this question. Code your answer as a LOGICAL vector and assign to AnswerC. 

AnswerC <- FALSE
  
  # Question D ####
# What are the names of all the variables in the data frame? Assign your answer to col.names. 
col.names<- colnames(gss_df)
# can also just use names(gss_df)
  
  # Question 1 ####
# Our ultimate goal is to understand what causes people to support federal funding for science. Our starting point for this inquiry should be to explore the dataset by developing a baseline understanding of the respondents and the sample. The most basic disaggregator in our society is gender. Make a plot of how many men and women respondents there in the sample overall. 

# When you make a plot that graphs number of respondents by sex, what do you expect to see. What ratio do you expect to see between men and women? Write your answer as a text vector and assign to Answer1_expectation. 

# Create Answer1_expectation
Answer1_expectation <- "I expect that there will be a bar graph with sex on the x axis and the number of observations on the y axis. I expect there to be around a 50-50 split."

# Create Answer1_plot
Answer1_plot<- gss_df %>% 
  group_by(SEX) %>% 
  count() %>% 
  rename(Number = n, Sex = SEX) %>% 
  ggplot(aes(x = Sex, y=Number)) +
  geom_bar(stat="identity")

# Use a text vector to explain what is peculiar about the result that you see. 
Answer1 <- "There are actually more female responders to the survey. Also, there is an 'NA' group of responders."


# Question 2 ####
# Repeat the steps we used in Question 1 for SEX to explore RACE 

# Create Answer2 Expectation 
Answer2_Expectation <- "I expect a bar graph with race on the x axis and number of oberservations on the y axis. I expect that there will be more white responders to the survey."

# Create Answer2_plot
Answer2_plot<- gss_df %>% 
  group_by(RACE) %>% 
  count() %>% 
  rename(Observations = n, Race = RACE) %>% 
  ggplot(aes(x=Race, y=Observations))+
  geom_bar(stat='identity')

# Use a text vector to explain what is peculiar about the result that you see. 
Answer2<- "There is a group for those who responded NA to the question. Like expected, there is a large racial disparity amound responders."

# Question 3 ####
# When using GSS data, it is vital that we compare weighted percentages. In this question we will learn more about sampling weights. 

# View the structure of the WTSSALL variable. What is peculiar about the values of this variable. Describe your answer in a text vector and assign to Question3_part1. 

Question3_part1 <- "It is a decimal value. There are several decimal places and there doesn't seem to be any meaningful minimum or maximum for the values."

# Find the description wtssall in the code book. What is this variable and why is it in the data? Describe your answer in a text vector and assign to Question3_part2.

Question3_part2 <- "It is a weight variable. These represent the probability of being including in the survey and are used to make samples more representative of the population."

# Important Takeaway #### 
# Our takeaway should be that we will need to compare the proportions. The GSS Team has surveyed a representative sample of Americans, which required oversampling some demographic groups to make sure there was enough data. The goal of this survey is NOT to take a Census of how many Americans in each demographic there are; rather, its goal is to understand the views and opinions Americans of each demographic group. 

# Therefore, analyzing data in the GSS will require us to find what percentage of a demographic group hold a certain opinion.

# Question 4 - Pointing out Ballot Difference ####
# Further, each respondent is also given a different ballot, which includes a different set of questions. You can read more about this in the Codebook or online. But we need to be aware that for each question in each year, we may have to filter to keep only the results for specific ballots. 

# How many different values are there for `BALLOT`?
Question4A <- unique(gss_df$"BALLOT")

# Let's say we want to understand which Ballots asked a specific question `ADVFRONT` (look up what it means in the codebook) in a specific year (2018), now let's use tidyverse to find this answer. 
gss_df %>% 
  filter(YEAR == 2018) %>% 
  group_by(BALLOT, ADVFRONT) %>% 
  count() %>% 
  View()

# Code a list of text vectors with the Ballots that asked this question. 
Question4B <- c("A", "B")

# As you can see, the GSS lends itself to cross-sectional study but makes time-series study difficult because we have to develop a data frame for each question and variable we want to study and then combine these data frames together. We will learn how to do this next week. 

# Question 4 ####
# Here IS how we want to compare data in the GSS within 1 year. We will use the 2018 data. 

# 1) How many respondents were male and female (fill in the blanks)
gss_df %>% 
  filter(YEAR == 2018 & (BALLOT == "Ballot a" | BALLOT == "Ballot b")) %>% 
  group_by(SEX) %>% 
  count()

# 2A) Now create a dataframe of Male respondents who answered specific Ballots. 
Male_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Male" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 2B) Now create a dataframe of Female respondents who answered specific Ballots. 
Female_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Female" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 3A) Group_by SEX and ADVFRONT, summarize data to calculate proportion of men who responded each value
Male_ADVFRONT_df<- Male_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  summarize(prop = n() / 691 * 100 )

# 3B) Group_by SEX and ADVFRONT, summarize data to calculate proportion of women who responded each value
Female_ADVFRONT_df <- Female_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  summarize(prop = n()/ 868 * 100)

# 4) Combine these dataframes using the bind_rows function 
ADVFRONT_by_SEX_df <-bind_rows(Male_ADVFRONT_df, Female_ADVFRONT_df)

# 5A) Create ggplot bar graph  
ADVFRONT_by_SEX_df %>% 
  ggplot(aes(x=SEX, y= prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5B) Filter out "Not Applicable" & "No answer" and make a ggplot. 
ADVFRONT_by_SEX_df %>% 
  filter(ADVFRONT != "Not applicable" & ADVFRONT != "No answer") %>% 
  ggplot(aes(x=SEX, y= prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5C) What are some aspects of this visualization that are wonky? Describe in detail what you want to change about this graph in the future. Assing your answer to Answer5C 
Answer5C <- "The proportions accumulate and don't add to 100% because not applicable and no answer were removed. I would add numerical labels on the visualization to indicate what the proportions are for each. Also, the two greens for 'disagree' and 'dont know' are too similar and hard to distinguish. In the future I would change these colors."

