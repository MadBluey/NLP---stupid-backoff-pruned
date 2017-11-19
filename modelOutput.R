stupidBackoffScores <- function(input_split,output){
    
    
    class <- output$class[1]
    # Take in a n-gram find the score of n-gram and find the score of n-gram -1. 
    if(class == "trigram"){
        output$score <- 
            output$n / 
            sum(
                filter(
                    df,
                    word1==input_split[length(input_split)-1],
                    word2 == input_split[length(input_split)] & class == "bigram")$n)
        
    } else if (class == "bigram"){
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
    input <- tolower(input)
    if(stri_count_words(input) > 1) {
        input_split <- stri_split(input,regex = " ")[[1]]
        len <- length(input_split)*1 # As numeric... 

        output <- rbind(
            stupidBackoffScores(
                input_split, 
            filter(
                df, word1 == input_split[len-1] & word2 == input_split[len] & class == "trigram")
            )
            ,stupidBackoffScores(
                input_split, 
            filter(
                df, word1 == input_split[len] & class == "bigram")
            )
        )
        
    }
    else{
        
        output$probrability <- round(output$n / sum(output$n),3)
        drops <- c("word1","word3","n","class")
        print(output[, !names(output) %in% drops]) #1:5
        
    }
}










bigramF <- function(input,df = bigTablePruned){
    input <- tolower(input)
    input_split <- stri_split(input,regex = " ")[[1]]
    len <- length(input_split)*1
    output <- filter(df, word1==input_split[len] & class == "bigram")
    if(is.na(output$word1[1])){
        output <- filter(df, class == "unigram")
        output$probrability <- round(output$n / sum(output$n),3)
        drops <- c("word3","word2","n","class")
        print(output[, !names(output) %in% drops]) #1:5
    } else { 
        output$probrability <- round(output$n / sum(output$n),3)
        drops <- c("word1","word3","n","class")
        print(output[, !names(output) %in% drops]) #1:5
    }
}

trigramF <- function(input,df = bigTablePruned){
    input <- tolower(input)
    if(stri_count_words(input) > 1) {
        input_split <- stri_split(input,regex = " ")[[1]]
        len <- length(input_split)*1 # As numeric... 
        output <- filter(df, word1 == input_split[len-1] & word2 == input_split[len] & class == "trigram")
        if(is.na(output$word1[1])){
            bigramF(input)
        } else {
            output$probrability <- round(output$n / sum(output$n),3)
            drops <- c("word1","word2","n","class")
            print(output[, !names(output) %in% drops]) #1:5
        }
    }
}



