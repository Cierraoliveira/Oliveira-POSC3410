# Title: Lecture 15: Linear Regression 
# Author: Cierra Oliveiera
# Author's Email: colivei@clemson.edu
# Date Created: 2021-03-23

# Purpose:
# We will practice re-categorizing variables. We will learn how to run and interpret regressions. 

# Our toy data set we will be using are university patents data from the AUTM Dataset. Tech transfer is the process of translating scientific findings into technologies someone buys the right to integrate into a product or service. Since the Bayh-Dole Act of 1980, universities have the right to own all inventions created under federal research. Owning inventions is important because the royalties that universities receive from companies that bought the right to use the technology helps to offset research and administrative costs. 

# Set Up####
# Libraries
library(tidyverse)
library(stargazer)

# Load Data and create example data set ####
load("~/GitHub/Oliveira-POSC3410/autm_example_df.Rdata")


# Our Question of the Day ####
# Does giving a university technology transfer office more resources (treasure and bodies) lead to increased tech transfer activity? 

# Theory: 
# More resources (i.e., funding, people, invention disclosure) -->  more licenses, options, gross licensing income, and start ups formed. 

#H0: More resources have no effect on licenses, options, invention disclosure, gross licensing income, and start ups formed.

# Ha: There is a positive relationship between resources and licenses, options, invention disclosure, gross licensing income, and start ups formed.

# Explore the Dataset ####

# Let's understand our dependent variables 
# Gross Licensing Income (the amount of money TTOs make)
summary(autm_example_df$grossLicInc)

# Start Ups Formed (new businesses created with university technologies)
summary(autm_example_df$St_UpsFormed)

# Licenses Issues 
summary(autm_example_df$licIss)

# Let's understand our independent variables 

# Research Expenditure 
summary(autm_example_df$totResExp)

#Invention Disclosures Received (the amount new technologies disclosed by researchers to the TTO that could be licensed) 
summary(autm_example_df$invDisRec)

# Licensing FTEs (Number of Full Time Equivalents dedicated to licensing. )
summary(autm_example_df$licFTEs)

# Histogram of data
hist(autm_example_df$licFTEs)
d <- density(autm_example_df$licFTEs)
plot(d)


# Add Region Codes #### 
unique(autm_example_df$institution)

# Southeast 
southeast <- autm_example_df %>% 
  filter(institution == "Clemson Univ." | 
          institution == "North Carolina State Univ." |
          institution == "Univ. of Central Florida"
           ) %>% 
  mutate(region = "Southeast")

# West 
west <- autm_example_df %>% 
  filter(institution == "Univ. of Arizona" | 
           institution == "Univ. of California System" 
  ) %>% 
  mutate(region = "West")

# Midwest 
midwest <- autm_example_df %>% 
  filter(institution == "Northwestern Univ." | 
           institution == "W.A.R.F./University of Wisconsin Madison" 
  ) %>% 
  mutate(region = "Midwest")

autm_example_df <- bind_rows(southeast, southeast, midwest, west)

# Descriptive Statistics ####
# Do universities with more research funding earn more licensing income? 
low_res_exp_df <- autm_example_df %>% 
  filter(totResExp <= 1.178e+08)

high_res_exp_df <- autm_example_df %>% 
  filter(totResExp >= 4.042e+08)

# t.test
t.test(low_res_exp_df$grossLicInc, high_res_exp_df$grossLicInc, a = "t")
 # we reject the null hypothesis

# Linear Regression ####

# The purpose of linear regression is to model the relationship between an interval/scalar dependent variable and one or more explanatory variables. We an use this model to explain the relationship between two variables that have LINEAR relationship. 


simple_model <- lm(data = autm_example_df, grossLicInc ~ totResExp )
summary(simple_model)

# Let's pretty this up. 
stargazer(simple_model, type = "html", out = "~/GitHub/Oliveira-POSC3410/simple_model.html")

  # R^2 is 0.154 
  # TRE explains 15.4% of the variation

# Let's think about this graphically 
scatterplot <- autm_example_df %>% 
  ggplot(aes(x=totResExp, y = grossLicInc ))+
  geom_point(aes(color = institution)) 
scatterplot
  # we can see that universities cluster
  # WARF behaves how we expect 

scatterplot +
  geom_smooth(method="lm") +
  theme(legend.position = "bottom")


# What about intervening variables? Such as who does the work? We can improve models by adding variables that can help us explain some of the variance in the dependent variable. 

better_model <- lm(data = autm_example_df, grossLicInc ~ totResExp + licFTEs + lag(licFTEs) + lag(invDisRec, n=3)  ) 
summary(better_model)

# Let's pretty this up. 
stargazer(simple_model, better_model, type = "html", out = "~/GitHub/Oliveira-POSC3410/simple_and_better_model.html")

# Let's see an example with very good fit. 
data(mpg)

# Scatterplot 
mpg %>% 
  ggplot(aes(x = displ, y =hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(method="lm")

# Regression with output
fitted_regression <- lm(data = mpg,  hwy ~ displ)
summary(fitted_regression)

stargazer(simple_model, better_model, fitted_regression, type = "html", out = "~/GitHub/Oliveira-POSC3410/final_table.html")

# Copyright (c) Grant Allard, 2021
