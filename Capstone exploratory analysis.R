##Coursera Capstone project

library(wordcloud)

NLP_project = 'C:/Users/Ed/Documents/GitHub/Coursera Capstone/Data/final/en_US'

setwd(NLP_project)

length(readLines("en_US.twitter.txt"))

head(readLines("en_US.twitter.txt"))

twitter <- readLines("en_US.twitter.txt", n=-1)
news <- readLines("en_US.news.txt", n=-1)
blogs <- readLines("en_US.blogs.txt", n=-1)

length(twitter)
length(news)
length(blogs)

##Appends text fiels together
d <- strsplit(news," ")
d[[2]] <- strsplit(twitter," ")
d[[3]] <- strsplit(blogs," ")

d <- unlist(d)

##Tokenizing the data
d2 <- rep(d,1)
d2 <- as.data.frame(table(d2))
d2 <- d2[with(d2, order(-Freq, d2)), ]
d2$pct <- d2$Freq/sum(d2$Freq)

##wordcloud graphic (optional)
wordcloud(d2$d2, d2$Freq, c(7,1), max.words = 250)

##Function determines which words need to be included for a given percentile
req_words <- function(x,y){
  i_aggregate = 0
  sum_data = 0
  find_aggregate = 0
  i = 0
  
  sum_data <- sum(y)
  find_aggregate <- x*sum_data
  
  for(i in 1:length(y)){
      i_aggregate <- i_aggregate + y[i]
      if(i_aggregate > find_aggregate){break}}
  i
}

##Use Function to determins data that will need to be subsetted and processed
req_words(.5,d2$Freq)
req_words(.9,d2$Freq)

sum(grepl("[Tt]he", d))
head(d2)
