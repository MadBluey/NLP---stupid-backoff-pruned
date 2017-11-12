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
  # Create that each piece is 10MB 
  
}

if(file.exists("ngram.RData")){ load("ngram.RData") 
} else {
  
  unigram <- df %>% unnest_tokens(word1, text) %>% count(word1, sort = TRUE) %>% ungroup()
 
  bigram <- df %>% unnest_tokens(bigram,text,token = "ngrams", n=2) %>% count(bigram, sort = TRUE)
  
  ptm <- proc.time()
  trigram <- df %>% unnest_tokens(trigram,text,token = "ngrams", n=3) %>% count(trigram, sort = TRUE)
  proc.time() - ptm
  
  save(unigram,bigram,trigram,file = "ngram.RData")
}

# Seperate trigrams

bigramsSeparated <- bigram %>% separate(bigram, c("word1","word2"), sep = " ")
trigramSeparated <- trigram %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
remove(bigram,trigram)

unigram$word2 <- NA
unigram$word3 <- NA 
unigram$class <- "unigram"

bigramsSeperated$word3 <- NA
bigramsSeperated$class <- "bigram"

trigramSeparated$class <- "trigram"

bigTable <- rbind(unigram,bigramsSeperated,trigramSeparated)
save(bigTable, file = "bigTable.RData")
remove(trigramSeparated, bigramsSeperated,unigram)

# Prune that those that have less than a count of 4 are removed.
bigTablePruned <- filter(bigTable, n > 4)

bigTablePruned$word1 <- stri_replace_all_regex(bigTablePruned$word1, "[^\\x00-\\x7F]","")
bigTablePruned$word2 <- stri_replace_all_regex(bigTablePruned$word2, "[^\\x00-\\x7F]","")
bigTablePruned$word3 <- stri_replace_all_regex(bigTablePruned$word3, "[^\\x00-\\x7F]","")
bigTablePruned <- filter(bigTablePruned, bigTablePruned$word1 != "" | bigTablePruned$word2 != "" & bigTablePruned$class ==  " bigram" | bigTablePruned$word3 != "" & bigTablePruned$class == "trigram ")

save(bigTablePruned,file = "bigTablePruned.RData")



# That n-gram which is not in the top 5 will be remooved. 