#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("modelOutput.R")

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
    
    
    terms <- reactive({ 
        # Only do something if "submit" button is pressed. 
        input$update
        
        isolate({
            withProgress({
                setProgress(message = "Calculating...")
                modelOutput(input$text1)
                
            })
        })
        
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    output$plot2 <- renderPlot({
        v <- terms()
        wordcloud_rep(words = v$predWord,freq = v$score, colors = brewer.pal(8, "Dark2"), scale = c(4,0.5))
    })
    
    tags$style(type="text/css",".shiny-output-error{visibility:hidden;}",".shiny-output-error:before{visibility:hidden;]")
    output$plot1 <- renderTable({
        a <- terms()
        a <- a[,!names(a) %in% "n"]
           })
    
    output$textO <- renderText({
        a <- terms()
        HTML(paste0("Prediction: ", input$text1," <b>", a$predWord[1],"</b>"))
    })
    
    # Supress Error messages...
    
    
})
