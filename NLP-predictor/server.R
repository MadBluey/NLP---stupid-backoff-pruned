#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    source("modelOutput.R")
    load("bigTablePruned.Rdata")
    
    
  set.seed(1234)
    plot1 <- qplot(rnorm(500),fill = I("red"),binwidth = 0.2)
    plot2 <- qplot(rnorm(500),fill = I("blue"),binwidth = 0.2)
  
    output$plot1 <- renderPlot({plot1})
    output$plot2 <- renderPlot({plot2})
})
