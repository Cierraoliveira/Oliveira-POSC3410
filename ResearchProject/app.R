#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggpubr)
load("~/migration_rightattacks_df.Rdata")

# final_df %>% 
#   ggplot(aes(x=log(net_mig), y=log(count))) +
#   geom_point() +
#   geom_smooth(method="lm", col="black")+
#   stat_regline_equation(label.x.npc = "left", label.y.npc = "top")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Right Wing Terrorism and Immigration in the Western World"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h3("Figure Controls"),
            sliderInput("years",
                        "Select the years for analysis:",
                        min = min(final_df$year),
                        max = max(final_df$year),
                        value = c(min(final_df$year),max(final_df$year)),
                        sep=""),
          h3("Application Purpose:"),
          h6("This application showcases the relationship between immigration
             and right-wing terrorism."),
          h3("Theory and Hypotheses:"),
          h5("Theory:"),
          h6("Increases in immigration aggravate the grievances of the far-right, and increasing 
             anti-immigrant sentiment drives attacks by right-wing groups."),
          h5("H0:"),
          h6("There is NOT a relationship between immigration and the number of right-wing terrorism."),
          h5("HA:"),
          h6("There IS a relationship between immigration and the number of right-wing terrorism."),
          h3("Research Design:"),
          h6("Observational Time Series"),
          h5("Explanatory Variable:"),
          h6("The Number of Immigrants in a given year, in a given country."),
          h5("Explained Variable:"),
          h6("The Number of Far-Right Terrorist Attacks"),
          h3("Interpretation of Statistical Test:"),
          h6("Based on the results of my statistical analysis, I had a P value less than 0.01. 
             Therefore, I reject my Null hypothesis with 99% confidence"),
          h6("For every percent change of immigration, the number of right-wing attacks changes by 0.234%."),
          h3("Analytical Conclusion:"),
          h6("Based on my statistical analysis, I reject the null hypothesis that there is no relationship 
             between immigration and right wing terrorist attacks. There is sufficient evidence to suggest 
             that there is a positive relationship between immigration and the number of attacks performed 
             by far-right groups. "),
          h3("Next Steps in Research Agenda:"),
          h6("First, I would like to further investigate any shortfalls in codebook's documentation of 
             right-wing groups. Next, I would like to dive deeper into what drives far-right terrorism or 
             right-wing hate, more broadly."),
          h3("References:"),
          h6("Miller, Erin, Gary LaFree, and Laura Dugan. 2021. Global Terrorism Database. University of 
             Maryland: National Consortium for the Study of Terrorism and Responses to Terrorism."),
          h6("United Nations Population Division. World Population Prospects: 2019 Revision.
             Net Migration Data.")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Number of Right-Wing Attacks per year"),
           plotOutput("distPlot1"),
          h3("Immigration and Right-Wing Attacks"),
           plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
      #Figure 1 Plot Data Set Up
      filtered_df <- final_df %>% 
        filter(year>=input$years[1] & year<=input$years[2])
      
      data <- aggregate(count ~ year, data = filtered_df, sum)
      #Figure 1 plot call 
      data %>% 
        ggplot(aes(x=year, y=count)) +
        geom_bar(stat="identity")+
        labs(x="Year", y="Number of Right-Wing Attacks Recorded")
    })
    
    output$distPlot2 <- renderPlot({
      #Figure 2 Plot Data Set Up
      filtered_df <- final_df %>% 
        filter(year>=input$years[1] & year<=input$years[2])
      
      filtered_df %>% 
        ggplot(aes(x=log(net_mig), y=log(count))) +
        geom_point() +
        geom_smooth(method="lm", col="black")+
        stat_regline_equation(label.x.npc = "left", label.y.npc = "top", size = 8)+
        labs(x="Net Migration (logged)", y="Number of Right-Wing Attacks (logged)")
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
