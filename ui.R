#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Natural Language Processing - Next Word Predictor."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(position = "left",
                     textInput("text1", label = "Insert sentence here:"), 
                     actionButton("update","Submit")
        ),
        
        # Show a plot of the generated distribution
        mainPanel("The upper panel shows the top 5 words and the bottom panel the scores",
                  fluidRow(splitLayout(cellWidths = c("50%","50%")),
                           tableOutput("plot1"),plotOutput("plot2")
                  )
        )
    )
)
)
