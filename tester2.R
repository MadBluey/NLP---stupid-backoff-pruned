# Create the trigrams in pieces - for cycles. 
library(dplyr)
library(tidytext)
library(tidyr)
library(stringi)
library(Kmisc)

load("datagrams.RData")
if(!file.exists("datagrams.RData")){
    
    readIN <- function(filename, dataset) {
        df <-
            as_tibble(data.frame(
                text = readlines(filename),
                dataset = dataset
            ))
        
        df$text <- as.character(df$text)
        df
        
    }
    
    removeNonUTF8 <- function(data) {
        
        data$text <- iconv(data$text, "UTF-8", "UTF-8", sub = '')
        data$text <- stri_replace_all_regex(data$text, "\u2019|`", "'")
        data$text <-
            stri_replace_all_regex(data$text, "\u201c|\u201d|u201f|``", '"')
        data$text <- stri_replace_all_regex(data$text, '\\<[^\\>]*\\>|[[:digit:]]|[[:punct:]]|$<f0><U+009F><U+0091><U+00A6>
|<|>',"")
        data
        
    }
    
    # Read in the data
    df <- rbind(
        readIN("final/en_US/en_US.twitter.txt", "Twitter"),
        readIN("final/en_US/en_US.blogs.txt", "Blogs"),
        readIN("final/en_US/en_US.news.txt", "News"))
    
    # Function for removing non UTF-8 characters from text
    df <- removeNonUTF8(df)
    save(df,file = "datagrams.RData")
    
}

if(file.exists("ngram.RData")){ load("ngram.RData") 
} else {
    
    #unigram <- df %>% unnest_tokens(word1, text) %>% count(word1, sort = TRUE) %>% ungroup()
    
    #bigram <- df %>% unnest_tokens(bigram,text,token = "ngrams", n=2) %>% count(bigram, sort = TRUE)
    
    #trigram <- df %>% unnest_tokens(trigram,text,token = "ngrams", n=3) %>% count(trigram, sort = TRUE)
    
    ptm <- proc.time()
    quadgram <- df %>% unnest_tokens(quadgram,text, token = "ngrams", n = 4) %>% count(quadgram, sort = TRUE) %>% 
        separate(quadgram, c("word1","word2","word3","word4"), sep = " ") 
    
    #fivegram <- df %>% unnest_tokens(fivegram, text, token = "ngrams", n = 5) %>% count(fivegram,sort = TRUE) %>% 
    #    separate(fivegram, c("word1","word2","word3","word4","word5"), sep = " ")
    proc.time() - ptm
    save(unigram,bigram,trigram,quadgram, file = "ngram.RData")
}

# Seperate trigrams

bigramsSeparated <- bigram %>% separate(bigram, c("word1","word2"), sep = " ")
trigramSeparated <- trigram %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
remove(bigram,trigram)

unigram$word2 <- NA
unigram$word3 <- NA 
unigram$word4 <- NA
unigram$class <- "unigram"

bigramsSeparated$word3 <- NA
bigramsSeparated$word4 <- NA
bigramsSeparated$class <- "bigram"

trigramsSeparated$word4 <- NA
trigramSeparated$class <- "trigram"

quadgram$class <- "quadgram"

bigTable <- rbind(unigram,bigramsSeparated,trigramSeparated,quadgram)
save(bigTable, file = "bigTable.RData")
remove(trigramSeparated, bigramsSeparated,unigram)

# Prune that those that have less than a count of 4 are removed.
bigTablePruned <- filter(bigTable, n > 4)

bigTablePruned$word1 <- stri_replace_all_regex(bigTablePruned$word1, "[^\\x00-\\x7F]","")
bigTablePruned$word2 <- stri_replace_all_regex(bigTablePruned$word2, "[^\\x00-\\x7F]","")
bigTablePruned$word3 <- stri_replace_all_regex(bigTablePruned$word3, "[^\\x00-\\x7F]","")
bigTablePruned$word4 <- stri_replace_all_regex(bigTablePruned$word3, "[^\\x00-\\x7F]","")
bigTablePruned <- filter(bigTablePruned, bigTablePruned$word1 != "" | bigTablePruned$word2 != "" & bigTablePruned$class ==  " bigram" | bigTablePruned$word3 != "" & bigTablePruned$class == "trigram " | bigTablePruned$word4 & bigTablePruned$class = "quadgram")

save(bigTablePruned,file = "bigTablePruned.RData")
