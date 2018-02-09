
library(tm)

MAX_N_GRAMS = 3
TOP_FREQ = 6

# read data files to configure the app
df2 <- readRDS("bigrams.rds")
df3 <- readRDS("trigrams.rds")
df4 <- readRDS("fourgrams.rds")

msg <- ""

# these functions makes cleansing in input parameter

removeWhiteSpaces <- function(str) {
  return( gsub("\\s+"," ",str) )
}

# dataframe 'profanity' was previously saved
# prof <- read.csv("profanity.txt", header=FALSE, stringsAsFactors=FALSE)
# saveRDS(prof,"profanity.rds")

removeProfanity <- function(str) {
  dfProf <- readRDS("profanity.rds")
  nWords <- nrow(dfProf)
  for(i in 1:nWords) 
    str <- gsub(dfProf[i,1],"*",str, perl=TRUE) 
  return(str)
}

clean <- function(input) {
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- removeWhiteSpaces(input)
  #input <- rev(unlist(strsplit(input," ")))
  input <- tolower(input) 
  input <- removeProfanity(input)
}


get_last_words <- function(lst, n) {
  x <- as.data.frame(strsplit(lst, " "))
  colnames(x) <- c("words")
  len <- nrow(x)
  if(len<n) n<-len 
  lst <- paste(x[ (len-(n-1)) : len ,], collapse=" ")
  return(lst)
}



contractions <- function(token) {
  token <- gsub("^s\\s","\'s ",token)
  token <- gsub("^n\\s","\'n ",token)
  token <- gsub("^d\\s","\'d ",token)
  token <- gsub("^t\\s","\'t ",token)
  token <- gsub("^ve\\s","\'ve ",token)
  token <- gsub("^ll\\s","\'ll ",token)
  token <- gsub("^re\\s","\'re ",token)
  return(token)
}

## return the top frequent words from unigram table

get_top <- function(n=TOP_FREQ) {
  df1 <- readRDS("unigrams.rds")
  nrows <- nrow(df1)
  vTop <- vector("list", length=25)
  for(i in seq_len(25))
     vTop[i] <-  as.character(df1[i,1])
       extradetails <- paste("~ [Frequency: ", as.character(df1[i,2]), "]~   [Prob: ", as.character(df1[i,3]), "] ", sep="")
    vTop[i] <-  paste( vTop[i], extradetails,  sep=" ")
  return(head(vTop,n))
}


## Stupid back-off method to find next word in different n-gram dataframe

find_in_grams <- function(lastW, n) {
  lastW <- paste0('^',lastW,' ')
  
  # points the search to correspondent 'n-gram' dataframe 
  dfSearch <- data.frame()
  
  # subset 'n-gram' dataframe to find the most probable occurrences 
  if(n==3)
    dfsub <- subset(df4, grepl(lastW, df4$Word))
  else
    if(n==2)
      dfsub <- subset(df3, grepl(lastW, df3$Word))
  else
    if(n==1)
      dfsub <- subset(df2, grepl(lastW, df2$Word))
  
  print(head(dfsub)); 
  print(nrow(dfsub))
  
  if(nrow(dfsub) > 0) {
     top5words <- head(contractions(gsub(lastW,"",dfsub$Word)),TOP_FREQ)
     predictWordsOnly <-top5words
    #top5words <- head(contractions(gsub(lastW,"",dfsub$Freq)),TOP_FREQ)
     extradetails <- paste("~ [Frequency: ", as.character(dfsub$Freq), "]~   [Prob: ", as.character(dfsub$Prob), "] ", sep="")
     print(extradetails)
      
   # top5words <- head(contractions(gsub(lastW,"",paste( dfsub$Word,"([Frequency:", as.character(dfsub$Freq), " - [Prob: ",as.character(dfsub$Prob), "]",sep=""))),TOP_FREQ)
     top5words <- head(contractions(gsub(lastW,"",paste( dfsub$Word,extradetails,sep=" "))),TOP_FREQ)
     
     
    #return( gsub("[[:space:]]"," ",top5words) )
  }
  else{
    n <- n-1;
    if(n > 0) { 
      lastW <- substr(lastW,2,nchar(lastW))
      find_in_grams( get_last_words(lastW,2), n )
    }
    else {
      lastW <- substr(lastW,2,nchar(lastW))
      return(get_top(TOP_FREQ))
    }
  }
}

predict_model <- function(user_input) {
  return( find_in_grams( get_last_words(user_input, MAX_N_GRAMS), n=MAX_N_GRAMS) )
}