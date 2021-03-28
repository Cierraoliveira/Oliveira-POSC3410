# Title: DAL6 (In-Class Exercise)
# Author: Cierra Oliveira
# Author's Email: colivei@clemson.edu
# Date Created: 2021-03-26

# Purpose:
# 

# Set Up####
# Libraries
library(tidyverse)

# Data
load("~/GitHub/Oliveira-POSC3410/autm_example_df.Rdata")
gtd_raw<-read.csv("~/GitHub/Oliveira-POSC3410/gtd_raw.csv")

# Make analysis df 
gtd_df <- gtd_raw

# Regression Practice - AUTM Dataset ####

# scatterplot 
autm_example_df %>% 
  ggplot(aes(x = totResExp, y = grossLicInc, color = institution)) +
  geom_point() +
  theme(legend.position = "bottom")

# Create Northwestern Data
nu_df <- autm_example_df %>% 
  filter(institution == "Northwestern Univ.")

warf_df <- autm_example_df %>% 
  filter(institution == "W.A.R.F./University of Wisconsin Madison")

# 
# lm NU  
summary(lm(data = nu_df, grossLicInc ~ totResExp))
  # p value is 0.2

# lm Warf
summary(lm(data=warf_df, grossLicInc ~ totResExp ))

# scatterplot with categorical variable 
autm_example_df %>% 
  ggplot(aes(x = totResExp, y = grossLicInc, color = institution)) +
  geom_point() +
  theme(legend.position = "bottom") +
  geom_smooth(method="lm", se=FALSE) + 
  facet_wrap(~institution, scales="free")
  

# lm autm_example_df + institution variable 
summary(lm(data=autm_example_df, grossLicInc ~ totResExp + institution))


# GTD Data #### 

# Does the number of attacks increase over time? 
gtd_df %>% 
  group_by(iyear) %>% 
  summarize(attacks = n()) %>% 
  arrange(iyear) %>% 
  ggplot(aes(x=iyear, y=attacks)) +
  geom_point() + 
  geom_smooth(method="lm")

# By how much? 
# Create gtd_by_year_df
gtd_by_year_df <- gtd_df %>% 
  group_by(iyear) %>% 
  summarize(attacks = n())

# lm gtd_by_year_df
summary(lm(data = gtd_by_year_df, attacks ~ iyear)) 

# How do we better fit the line? 

# Divide data into three separate periods 
first_data <- gtd_by_year_df %>% 
  filter(iyear<1995)

middle_data <- gtd_by_year_df %>% 
  filter(iyear>=1995 & iyear<2014)

late_data <- gtd_by_year_df %>% 
  filter(iyear>=2014)

# Regressions
summary(lm(data=first_data, attacks ~ iyear)) 

summary(lm(data=middle_data, attacks ~ iyear)) 

summary(lm(data=late_data, attacks ~ iyear)) 

# Want to see this in the same readout? 
first_data <- gtd_by_year_df %>% 
  filter(iyear<1995) %>% 
  mutate(period="first")

middle_data <- gtd_by_year_df %>% 
  filter(iyear>=1995 & iyear<2014) %>% 
  mutate(period="middle")

late_data <- gtd_by_year_df %>% 
  filter(iyear>=2014)%>% 
  mutate(period="late")

# Bind Rows 
gtd_by_year_df <- bind_rows(first_data, middle_data, late_data)

#lm gtd_by_year_df with peirod as factor 
summary(lm(data=gtd_by_year_df, attacks ~ iyear + period)) 


gtd_by_year_df %>% 
  ggplot(aes(x=iyear, y=attacks, color = period)) +
  geom_point() + 
  geom_smooth(method="lm")


# Copyright (c) Grant Allard, 2021
