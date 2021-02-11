# Title: DAL4 - Part 2
# Author: Cierra Oliveira
# Author's Email: colivei@clemson.edu
# Date Created: 2021-02-10

# Purpose:
# We will learn how to work with the weights in the GSS survey to analyze questions using data visualizations. 

# Set Up####
# Libraries
library(tidyverse)

# Data
# Load data 
load('gss_df.Rdata')

# Let's Look Only at the 2018 Data 
gss_2018_df <- gss_df %>% 
  filter(YEAR == 2018)

# Question 1 ####
# Let's look at the relationship between sex and approval of federal support for science. We are testing a theory that says sex causes people to form their opinion about whether the federal government should support science. 

# We have translated this theory into a set of hypotheses that we are testing. 

H0 <- "The percent of men and women who approve of federal support for science the same." 

Ha <- "The percent of men and women who approve of federal support for science is not the same." 

# First let's check the unique value of Advfront_df
unique(gss_2018_df$ADVFRONT)

# We need to filter out observations where the value of ADVFRONT is "Not applicable". We also need to summize the data by SEX and ADVFRONT. 
sex_Advfront_df <- gss_2018_df %>%
  filter(ADVFRONT != "Not applicable") %>% 
  group_by(SEX, ADVFRONT) %>% 
  summarize(n = sum(WTSSALL))

# Calculate percent of men who feel a certain way
male_Advfront_df <- sex_Advfront_df %>% 
  filter(SEX == "Male")

# Total Number of men 
male_Advfront_df %>% 
  summarise(n=sum(n))

# Calculate men's responses as percentages. 
male_Advfront_df <- male_Advfront_df %>% 
  mutate(Advfront_perc = n/491 * 100) %>% 
  select(SEX, ADVFRONT,  Advfront_perc)

# Calculate percent of women who feel a certain way
female_Advfront_df <- sex_Advfront_df %>% 
  filter(SEX == "Female")

# Total Number of women 
female_Advfront_df %>% 
  summarise(n=sum(n))

# Calculate women's responses as percentages. 
female_Advfront_df <- female_Advfront_df %>% 
  mutate(Advfront_perc = n/670 * 100) %>% 
  select(SEX, ADVFRONT,  Advfront_perc)

# Create unified data frame of men and women by using bind_rows. 
sex_Advfront_per_df <- bind_rows(female_Advfront_df, male_Advfront_df)

# Text vector for Reordering ADVFRONT when we code it as a vector
levels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree", "Dont know", "No answer")

# ggplot bar graph of sex_Advfront_per_df
sex_Advfront_per_df %>% 
  mutate(ADVFRONT = factor(ADVFRONT, levels=levels)) %>% 
  ggplot(aes(x= SEX, y=Advfront_perc, fill=ADVFRONT)) +
  geom_bar(stat="identity")


# Question 1: Which hypothesis does the visualization provide evidence against? Why?

Answer1 <- "Because the bar graphs look to be about the same, it provides evidence against Ha, or the hypothesis that the percent of men and women who approve of federal support for science is not the same." 

# Question 2 #### 
# Now we are going to test a second theory related to Sex. We are testing the theory that Sex causes differences in how people view space exploration. 

# Hypotheses
H0 <- "The percent of men and women who approve of federal support for space exploration is the same." 

Ha <- "The percent of men and women who approve of federal support for space exploration is not the same." 

unique(gss_2018_df$NATSPAC)

# We will use repeat the process from question 1. 
sex_Natspac_df <-  gss_2018_df %>%
  filter(NATSPAC != "Not applicable") %>% 
  group_by(SEX, NATSPAC) %>% 
  summarize(n = sum(WTSSALL))

# Calculate percent of men who feel a certain way
male_Natspac_df <- sex_Natspac_df %>% 
  filter(SEX == "Male")

# Total Number of men 
male_Natspac_df %>% 
  summarise(n = sum(n))

# Calculate men's responses as percentages. 
male_Natspac_df <- male_Natspac_df %>% 
  mutate(natspac_perc = n/473 * 100) %>% 
  select(SEX, NATSPAC, natspac_perc)
  
# Calculate percent of women who feel a certain way
female_Natspac_df <- sex_Natspac_df %>% 
  filter(SEX=="Female")

# Total Number of Women 
female_Natspac_df %>% 
  summarise(n=sum(n))

# Calculate women's responses as percentages. 
female_Natspac_df <- female_Natspac_df %>% 
  mutate(natspac_perc = n/750 * 100) %>% 
  select(SEX, NATSPAC, natspac_perc)

# Combine into single df using bind_rows()
sex_Natspac_per_df <- bind_rows(male_Natspac_df, female_Natspac_df)

# Create text vector for reordering 
levels <- c("Too much", "About right", "Too little", "Don't know", "No answer")

# Create ggplot 
sex_Natspac_per_df %>% 
  mutate(NATSPAC = factor(NATSPAC, levels = levels)) %>% 
  ggplot(aes(x=SEX, y=natspac_perc, fill=NATSPAC)) +
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = levels, values = c("#FFCC00", "#33FF00", "#0033FF", "#999999", "#FFFFFF"))
  
# Question 2: Which hypothesis does this visualization provide evidence against? 
Answer2 <- "This visualization provides evidence against the hypothesis H0, or the hypothesis that the percent of men and women who approve of federal support for space exploration is the same. This visualization shows a disparity between the percent of men and women who believe we spend too little time on space exploration. A larger percentage of men think we spend too little or too much time on space exploration. Meanwhile, a larger percentage of women believe we spend about the right amount of time or don't know. This indicates that men tend to hold more polarized beliefs about the amount of time we spend on space exploration. "

