# Title: DAL 2 ####
# Author: Cierra Oliveira
# Date: 1/19/21

# Lesson 2 ####

# Load tidyverse package
library(tidyverse)

# Load data
data(mpg)

# Check the structure of mpg using str()
str(mpg)

# Answer below (using a comment) what type of data structure mpg is?
  # This is a tibble data frame, which is a simplified data frame. 
  # Tibbles are very similar to data frames, but they do less. They don't create row names, for example.

# Create analysis data frame: We do this so we preserve a raw version of the dataframe to which we can refer as we make
mpg_df<-mpg

# Check the structure of mpg_df using str()
str(mpg_df)

# Scatter plot of displ x hwy
# engine size–engine displacement in litres (displ)–and highway mileage (hwy)
ggplot(mpg_df, aes(x=displ , y=hwy ))+
  geom_point()

# Scatter plot of displ x hwy by fuel type (fl)
ggplot(mpg_df, aes(x=displ , y=hwy, color=fl ) )+
  geom_point()

# On Your Own, Scatter plot of displ x hwy by class
ggplot(mpg_df, aes(x=displ , y=hwy, color=class ) )+
  geom_point()

# Lesson 3 ####

# Call help for ggplot
?ggplot()

# Call help for filter
?filter()

# Call help for geom_bar()
?geom_bar()

# Call help for facet_wrap()
?facet_wrap()

# ggplot - data layer, assign it to mpg_data
mpg_data<-ggplot(data= mpg)

# Call mpg_data
mpg_data

# Describe the output you see in the console.
  # A gray plot is displayed
  # The command above just sets up the plot; no axis has been set

# Now, let's add an aesthetic layer to the plot.
# Put engine size on the x-axis, highway mileage on y-axis, and color by class.
mpg_plot<-mpg_data +
  geom_point(mapping = aes(x=displ , y=hwy, color=class))

# Call mpg_plot.
mpg_plot

# What do you see?
  #A scatter plot showing the relationship of engine size and highway mileage by car class

# Now, let's add an aesthetic layer to change the axis labels
mpg_axis <- mpg_plot +
  xlab("Engine Size in Liters")+
  ylab("Highway Mileage")+
  ggtitle("Engine Size and Mileage by Vehicle Class")

# Call mpg_axis
mpg_axis

# What do you see?
  # The same scatter plot from above, but with a new title and axis labels.

# Create different graphs for each class
mpg_wrap<-mpg_axis +
  facet_wrap(~class, nrow=2)

# Call mpg_wrap
mpg_wrap

# What do you see?
  # Several scatter plots for each car class
  # Each shows the relationship between engine size and mileage

# Re-assign mpg_wrap by adding facet wrapping, change nrow to 3.
mpg_wrap<-mpg_wrap + facet_wrap(~class, nrow=3)

# What do you see?
mpg_wrap
  #Now, there are several scatter plots but organized in 3 rows

# Lesson 4: Tidying Data ####

# load tidyverse package
library(tidyverse)

# load nycflights12 package
library(nycflights13)

# load flights data
data(flights)

# Filter to keep only the flights that happened on 3-14.
filter(flights, month==3, day==14)

# Assign flights that happened on 3-14 to new variable: `Mar14`.
Mar14<-filter(flights, month==3, day==14)

# Select flights that happened in either May or June
filter(flights, month == 5 | month == 6)

# Filter flights whose arrival was delayed over 60 minutes AND departed on time.
filter(flights, (arr_delay > 60 & dep_delay < 1))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed less than 15 minutes.
filter(flights, (arr_delay > 60 & dep_delay < 15))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed 15 minutes or more (i.e., remember to use your Greater Than or Equal To Operator).
filter(flights, (arr_delay > 60 & dep_delay >= 15))

# Filter flights whose destination is Greenville-Spartanburg International Airport (GSP)
filter(flights, dest=="GSP")

# Filter flights that were in the air over 3 hours (180 mins)
filter(flights, air_time==180)

# Effects of NAs on Logical Operations
NA == 99
NA > 3
NA < 3

# Number of Wins for Clemson Football in the 2021 season
Clemson <- NA
# Number of Wins for Ohio State football in the 2021 season
OSU <- NA
# Do Clemson and OSU have the same number of wins
Clemson == OSU
# Interpret this result using a comment your script file.
  # Here, neither Clemson or OSU have a value for the number of wins in the season
  # It doesn't indicate 0 wins, but rather an unavailable value.

# use is.na(varName) to find whether it is NA. This command returns a logical vector of either TRUE or FALSE. See the example below.
is.na(Clemson)

# see if OSU is NA
is.na(OSU)

# Now let's see how it works on a data frame.
is.na(flights)
# Describe what you see.
  # A table was return, showing the first 52 rows for all 19 data points. 
  # For each flight, either TRUE or FALSE values indicated whether the flight had a NA for that data point. 

# Let's see what happens when we combine filter() and is.na(). We will search for NAs in arrival delay.
filter(flights, is.na(arr_delay))

# In the real world, we often want to know how many observations are in each subset data frame. We will do this by calling nrow() outside of the filter commands.
# Number of observations in flights data set with missing data for arrival delay.
nrow(filter(flights, is.na(arr_delay)))

# Number of observations in flights data set with NO missing data for arrival delay.
nrow(filter(flights, !is.na(arr_delay)))

# On your own, number of observations in flights data set with missing data for departure delay.
nrow(filter(flights, is.na(dep_delay)))

# On your own, number of observations in flights data set with no missing data for departure delay.
nrow(filter(flights, !is.na(dep_delay)))

# In the real world, we often will filter out NAs across several variables and assign the resulting data frame to a new variable.
flights_df<-filter(flights, (!is.na(arr_delay) & !is.na(dep_delay)))

# Call flights_df
flights_df

# Create sample dataframe using sequence (use the help command to learn more about sequence)
sample<- sequence(10, from=10L, by=-1L )
sample_df <- as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
  # A tibble of size 10x1 is returned. The cells contain ints 
  # The ints are in a sequences from 10 to 1, decreasing in value. 

# Arrange from 1 to 10.
arrange(sample_df, value)

# Describe what the results that were returned.
  # The values in the tibble are reordered to be in increasing order. 

# Create sample dataframe with values from 1:100 using sequence.
sample<- sequence(100, from=1L, by=1L )
sample_df <- as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
  # A tibble of size 100x1 is returned. The cells contain ints 
  # The ints are in a sequences from 1 to 100, increasing in value.

# Arrange sample_df$value in descending order using the syntax, arrange(df, desc(varible))
arrange(sample_df, desc(value))

# Describe what the results that were returned.
  # The values in the tibble are reordered to be in decreasing order.

# What happens when we use arrange on a real life data frame (i.e., one that has missing values).
  # The rows with the missing values are put at the bottom when in increasing order.

# Arrange flights by arrival delay and assign to arrange_df
arrange_df<-arrange(flights, arr_delay)

# Call the last six rows of (call help on tail() to find out more)
tail(arrange_df)

# Lets try arranging data on your own.
# Arrange flights to find the most delayed flights.
arrange_df<-arrange(flights, desc(arr_delay))

# Arrange the flights that left earliest.
arrange_df<-arrange(flights, dep_delay)

# Example use of select
select(flights, year, month, day, carrier, flight)

# Select call columns between year and arrival delay
select(flights, year:arr_delay)

# Select all columns except tailnum
select(flights, -tailnum)

# Select all columns between dep_delay and time_hour.
select(flights, dep_delay:time_hour)

# Select the following columns: year, month, day, dep_delay, arr_delay, dest, distance. Assign to flights_df
flights_df<-select(flights, year, month, day, dep_delay, arr_delay, dest, distance)

# call flights_df
flights_df

# Check the column names
names(sample_df)

# Rename sample_df$value as sample_df$count
rename(sample_df, count = value)

# Rename sample_df$value as sample_df$count. Assign to sample_df
sample_df <- rename(sample_df, count = value)

# To be completed on your own.
# Check column names in flights
names(flights)

# Rename arr_delay as `arrival_delay`. Assignt to flights.
flights <- rename(flights, arrival_delay=arr_delay)
# Rename dep_delay as `departure_delay`. Assign to flights.
flights <- rename(flights, departure_delay=dep_delay)
# Call flights
flights

# Create dataframe with fewer variables: flights_small
flights_small <- select(flights,
                        year:day,
                        ends_with("delay"),
                        distance,
                        air_time,
                        dest)

# Create new column: avg_speed.
mutate(flights_small,
       speed = distance / air_time * 60)

# You can also use mutate to change existing variables.
# Change destination to factor
mutate(flights_small,
       dest = factor(dest))

# Create column `gain` using formula `dep_delay` - `arr_delay`.
mutate(flights_small, gain = departure_delay - arrival_delay)

# Create new column for arrival delay in hours (i.e., divide by 60)
mutate(flights_small, arrival_delay_hrs = arrival_delay / 60)

# Create new column for departure delay in hours (i.e., divide by 60)
mutate(flights_small, departure_delay_hrs = departure_delay / 60)

# Change year to character
mutate(flights_small, year = as.character(year))

# Create new character variable using paste0 (call help to learn more and check the hint below) to combine year, month, and day (yyyy/m/d).
mutate(flights, date = paste0(year, "/", month, "/", day))

# I need to change arr_delay and dep_delay back here
flights<-rename(flights, arr_delay=arrival_delay)
flights<-rename(flights, dep_delay=departure_delay)

# Use summarise to calculate avg (mean) arrival delay. 
summarise(flights, avgArrDelay= mean(arr_delay, na.rm=TRUE))

# Use group_by and summarise to calculate avg arrival delay for all the carriers.

# Group flights by carrier: flights_carrier
flights_carrier <- group_by(flights, carrier)

#summarise average delay 
summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm=TRUE))

# Summarise standard deviation of distance to destinations
summarise(flights, )

# Create new data frame: not_cancelled 
not_cancelled <- filter(flights, !is.na(dep_delay) & !is.na(arr_delay))

# Group by month, mean dep_delay
not_cancelled_year<- group_by(not_cancelled, month)

# Group by carrier, summarise mean dep_delay and mean arrival delay 
summarise(flights_carrier, avgDepDelay=mean(dep_delay, na.rm = TRUE))
summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm = TRUE))

# Group by carrier, summarise sd of distance traveled 
summarise(flights_carrier, sdDistance=sd(distance,na.rm=TRUE))

# Filter for origin=="LGA". When do the first flights depart? when do they arrive?
flights_LGA=filter(flights, origin=="LGA")
arrange(flights_LGA, dep_time)
  # The first flights depart at 00:01am and arrive at 01:24am, 01:11, 01:05, and 02:49.

# Lesson 5: Piping Data ####

# Load nycflights13 packages 
library(nycflights13)

# Load data 
data("flights")

# Filter the data for November and December flights, count number of flights that arrived later than 59 minutes. Which destination airports are worst? 

# Filter, assign new varName
flights_extract<-filter(flights, month==11 | month == 12 & arr_delay>60)

# Group by destination
flights_extract <- group_by(flights_extract,dest )

# Count 
flights_extract <- count(flights_extract)

# Rename 
flights_extract <- rename(flights_extract, number = n )

# Arrange 
flights_extract <- arrange(flights_extract, desc(number))

# Keep only top 6 rows
flights_extract <- head(flights_extract, n=6)

# Make ggplot bar graph
ggplot(flights_extract, aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Now look at the same operations with pipes. 
flights %>% 
  filter(month==11 | month == 12 & arr_delay>60) %>% 
  group_by(dest) %>% 
  count() %>% 
  rename(number=n) %>% 
  arrange(desc(number)) %>% 
  head(6) %>% 
  ggplot(aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Filter the flights data to keep only May and June flights
flights %>% 
  filter(month==5 | month==6)
# Filter the flights data to keep only April
flights %>% 
  filter(month==4)
# Filter the flights data to keep only Delta flights
flights %>% 
  filter(carrier=="DL")
# Which airport had the most flights from it to NYC's 3 airports? Filter flights data to keep only flights that arrived on-time or early, group by origin airport, arrange in descending order. 
flights %>% 
  filter((origin=="LGA" | origin=="JFK" | origin=="EWR") & arr_delay<=0) %>% 
  group_by(dest) %>%
  count() %>% 
  rename(number=n) %>% 
  arrange(desc(number)) %>% 
  head(6)
  
  # ORD, or Chicago O'Hare, was the airport that recieved the most flights from NYC's airports

# Filter flights to keep only flights that departed JFK and arrived at ATL. Keep only origin, destination, arrival delay,  departure delay,and carrier. Create a ggplot scatterplot showing the relationship between departure delay and  arrival delay. Add carrier as color. 
flights %>% 
  filter(origin=="JFK" & dest=="ATL" & !is.na(arr_delay) & !is.na(dep_delay)) %>% 
  select(origin, dest, arr_delay, dep_delay, carrier) %>% 
  ggplot(aes(x=dep_delay, y=arr_delay, color=carrier)) +
    geom_point() +
    ggtitle("Effect of Departure Delay on Arrival Delay")

# To which airports did the most flights from NYC go? Make a ggplot visualization. 
flights %>% 
  filter((origin=="LGA" | origin=="JFK" | origin=="EWR")) %>% 
  group_by(dest) %>%
  count() %>% 
  rename(number=n) %>% 
  arrange(desc(number)) %>%
  head(6) %>%
  ggplot(aes(x=reorder(dest,-number), y=number))+
    geom_bar(stat="identity")

  # ORD, ATL, and LAX are the top 3 destinations from NYC


# Which carrier had the shortest mean arrival delay? 
flights %>% 
  group_by(carrier) %>% 
  summarise(avgArrDelay=mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(avgArrDelay)

  # AS had the shortest arrival delay


# Which carrier experienced the most canceled flights? 
flights %>% 
  filter(is.na(arr_delay) & is.na(dep_delay)) %>% 
  group_by(carrier) %>% 
  count() %>% 
  rename(number=n) %>% 
  arrange(desc(number)) %>%
  head(6)
  
  # EV has the most canceled flights

# On average, which carrier flew fastest. 
flights %>% 
  group_by(carrier) %>% 
  summarise(avgAirTime=mean(air_time, na.rm = TRUE)) %>% 
  arrange(avgAirTime)

  # YV flew the fastest
