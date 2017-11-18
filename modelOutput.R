bigramF <- function(input,df){
    input <- tolower(input)
    input_split <- stri_split(input,regex = " ")[[1]]
    len <- length(input_split)*1
    output <- filter(df, word1==input_split[len] & class == "bigram")
    if(is.na(output$word1[1])){
        output <- filter(df, class == "unigram")
        output$probrability <- round(output$n / sum(output$n),3)
        drops <- c("word3","word2","n","class")
        print(output[1:5, !names(output) %in% drops])
    } else { 
        output$probrability <- round(output$n / sum(output$n),3)
        drops <- c("word1","word3","n","class")
        print(output[1:5, !names(output) %in% drops])
    }
}

trigramF <- function(input,df){
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
            print(output[1:5, !names(output) %in% drops])
        }
    }
}

modelOutput <- function(input,df) {
    input <- stri_trim_right(input)
    if(stri_count_words(input) <= 1) {     
        bigramF(input,df)
    } else { 
        trigramF(input,df)
    }
}
