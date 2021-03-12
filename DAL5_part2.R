# Title: DAL 5 Part II ####
# Author: Cierra Oliveira
# Date Created: 3/12/2021

# Set Up####
library(tidyverse)
library(stringr)
library(infer)

# Data
load("~/GitHub/Oliveira-POSC3410/gss_df.Rdata")

# GSS Example ####
# Theory: Support of science and technology -> Views on space exploration
# Null Hypothesis: A person's views on science and tech are NOT related to their views on space exploration. 
# Alternative Hypothesis: A person's views on science and tech ARE related to their views on space exploration. 

# Variables: 
# support of science and tech: nextgen
# support of space exploration: intscore

# look at unique values of nextgen
unique(gss_df$NEXTGEN)
  # this is a categorical variable

# look at unique values of intspace
unique(gss_df$INTSPACE)
  # this is a categorical variable

# Get data in shape to analyze ####

analysis_df <- gss_df %>% 
  # filter to keep rows we want
  filter(YEAR==2018 & !is.na(INTSPACE) & (NEXTGEN == "Agree" | NEXTGEN == "Agree"
         | NEXTGEN == " Strongly Agree" | NEXTGEN == "Strongly Diagree" | NEXTGEN == "Disagree") &
        (INTSPACE == "Moderatly interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>% 
  select(INTSPACE, NEXTGEN, WTSSALL) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(WTSSALL))

# theres an issue here
analysis_df <- gss_df %>% 
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly"),
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(count))


analysis_df <- analysis_df %>% 
  mutate(INTSPACE = factor(INTSPACE))

# add rows
add_rows <- tribble(~NEXTGEN, ~INTSPACE, ~count, "agree", "Not at all interested", 1, "disagree", "Not at all interested", 1)

analysis_df <- bind_rows(analysis_df, add_rows)


