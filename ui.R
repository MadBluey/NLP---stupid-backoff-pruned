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
    
    tags$head(
        tags$style(
            HTML("#dashboard{margin-bottom:50px;}")
        )
    ),
    
    
    # Application title
    titlePanel("Natural Language Processing - Next Word Predictor using the stupid backoff scoring algorithm."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(position = "left",
                     textInput("text1", label = "Insert sentence here:"), 
                     actionButton("update","UPDATE")
        ),
        
        # Show a plot of the generated distribution
        mainPanel("The upper panel shows the top 5 words their respective stupid backoff scores. The scores and the words are used in the wordcloud to represent the data more clearly.
                  To update the table and the prediction the user has to press Submit!
                  
                  ",
                  fluidRow(splitLayout(cellWidths = c("20%","30%","50")), 
                           
                           tableOutput("plot1"),
                           uiOutput("textO"),
                           plotOutput("plot2")
                  )
        )
    )
    
)
)
