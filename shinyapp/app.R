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
load("~/GitHub/Oliveira-POSC3410/ResearchProposal/migration_rightattacks_df.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Right Wing Terrorism in the Western World by Year"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Select the years for analysis:",
                        min = min(final_df$year),
                        max = max(final_df$year),
                        value = c(min(final_df$year),max(final_df$year)),
                        sep="")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      #Figure 1 Plot Data Set Up
      filtered_df <- final_df %>% 
        filter(year>=input$years[1] & year<=input$years[2])
      
      data <- aggregate(count ~ year, data = filtered_df, sum)
      #Figure 1 plot call 
      data %>% 
        ggplot(aes(x=year, y=count)) +
        geom_bar(stat="identity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
