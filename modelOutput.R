library(dplyr)
library(tidyr)
library(stringi)
library(wordcloud)

load("bigTablePruned.RData")

stupidBackoffScores <- function(input_split,output, df = bigTablePruned){
    
    class <- output$class[1]
    
    # Take in a n-gram find the score of n-gram and find the score of n-gram -1. 
    
    if (is.na(class)) {
        
        output <- filter(df, class == "unigram")
        output$predWord <- output$word1
        output$score <- round(output$n / sum(output$n),3)
        
    } else if(class == "trigram") {
        
        output$predWord <- output$word3
        
        output$score <- 
            output$n / 
            sum(
                filter(
                    df,
                    word1==input_split[length(input_split)-1],
                    word2 == input_split[length(input_split)] & class == "bigram")$n)
        
    } else if (class == "bigram") {
        
        output$predWord <- output$word2
        
        output$score <- 
            0.4 * output$n / 
            sum(
                filter(
                    df, 
                    class == "unigram" & word1 == input_split[length(input_split)])$n)
        
    }  
        
    output
    
}

modelOutput <- function(input, df = bigTablePruned){
    
    input <- stri_trim_both(tolower(input))
    
    if (stri_count_words(input) > 1) {
        
        input_split <- stri_split(input,regex = " ")[[1]]
        len <- length(input_split)*1 # As numeric... 
        
        trigram <- stupidBackoffScores(input_split,
            filter(
                df, word1 == input_split[len-1] & word2 == input_split[len] & class == "trigram")
        )
        
        bigram <-  stupidBackoffScores(input_split, 
            filter(
                df, word1 == input_split[len] & class == "bigram")
            )
        
        output <- rbind(trigram,bigram)
        
    } else if (stri_count_words(input) == 1) { 
        
        input_split <- stri_split(input,regex = " ")[[1]]
        len <- length(input_split)*1 # As numeric... 
        
        output <-  stupidBackoffScores(input_split, 
                                filter(
                                    df, word1 == input_split[len] & class == "bigram")
        )
        
        
    } else {
        
        output <- filter(df, class == "unigram")
        output$predWord <- output$word1
        output$score <- round(output$n / sum(output$n),3)
        
    }
    
    output <- arrange(output,desc(score))
    drops <- c("word1","word2","word3","class","n") 
    output[1:5, !names(output) %in% drops]
    
}
