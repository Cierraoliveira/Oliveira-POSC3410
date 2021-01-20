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
