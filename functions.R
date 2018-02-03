##Loading libraries
library(tm)

##Loading data
load("dat/1gram.Rda")
load("dat/2gram.Rda")
load("dat/3gram.Rda")
load("dat/4gram.Rda")
load("dat/1gram_ns.Rda")

##The alpha factor in "stupid back-off"
alpha<-0.4

##Define the names of nGrams dataframes
nGramDfNames <- c("quad_freq", "tri_freq", "bi_freq", "uni_freq") 
nGramDfNames_ns <- c("uni_freq_ns") 

## Clean the input 
cleanInput <- function(inputTxt) {
  inputTxt <- gsub("'", "", inputTxt) 
  inputTxt <- gsub("[[:punct:][:blank:]]+", " ", inputTxt) 
  inputTxt <- removeNumbers(inputTxt) 
  tolower(inputTxt)
}

## Get last word out of a string
getLastWord <- function (txt, seperator = " ") {
  txtElem <- strsplit(txt, seperator)[[1]]
  txtElem[length(txtElem)]
}

## Get each last word from a vector of strings
getLastWords <- function(txts) {
  lastWords <- c()
  if (length(txts) != 0) {
    for(i in c(1:length(txts)))
      lastWords[i] <- getLastWord(txts[i])
  }
  lastWords
}

## Get the number of words in a string
getLengthOfWords <- function(txt, seperator = " ") {
  length(strsplit(txt, seperator)[[1]])
}

## Get the last N words of a string
getLastNwords <- function(txt, n, seperator = " ") {
  txtElems <- strsplit(txt, seperator)[[1]]
  if (length(txtElems) < n) {
    stop("Text length invalid.")
  } else {
    lowerBound <- (length(txtElems) - n + 1)
    txtElems <- txtElems[lowerBound:length(txtElems)]
  }
  lastWords <- paste(txtElems, collapse = " ")
}

filterNgrams <- function(i, searchTxt) {
  if(i<4){
    nGramDf1<-get(nGramDfNames[i])     ##n_gram df
    nGramDf2<-get(nGramDfNames[i+1])   ##(n-1)_grame df
    words_temp<-nGramDf1[grep(paste("^", searchTxt, " ", sep = ""), as.character(nGramDf1$word), perl = TRUE), ][, c("word")]
    freq1<-nGramDf1[grep(paste("^", searchTxt, " ", sep = ""), as.character(nGramDf1$word), perl = TRUE), ][, c("freq")]
    freq2<-nGramDf2[grep(paste("^", searchTxt, "$", sep = ""), as.character(nGramDf2$word), perl = TRUE), ][, c("freq")]
    words_temp<-getLastWords(as.character(words_temp))
    n_regular<-0
    words<-c()
    for (x in words_temp){
      if (x %in% df_temp$words){next}
      words<-c(words,x)
      if (!(x %in% stopwords())){n_regular<-n_regular+1}
      if (n_regular>=10){break}
    }
    if(length(freq2)!=length(freq1)){score<-freq1/sum(freq1)}
    else{score<-freq1/freq2}
    score<-score[1:length(words)]
    if(length(words)>0){return(data.frame(words=words,score=score*alpha^(i-1),ngram=(5-i)))}
    }
  
  else{
    nGramDf1<-get(nGramDfNames[4])
    nGramDf2<-get(nGramDfNames_ns[1])
    words<-c(as.character(nGramDf1$word[1:10]),as.character(nGramDf2$word[1:10]))
    score<-c(nGramDf1$freq[1:10]/sum(nGramDf1$freq),nGramDf2$freq[1:10]/sum(nGramDf2$freq)) 

    return(data.frame(words=words,score=score*alpha^3,ngram=(5-i)))
    }
  
   return(NULL)
  }

getNextWordsSuggestion <- function(inputTxt) {
  
  search_complete <- FALSE
  inputTxt <- cleanInput(inputTxt)
  df_temp<<-data.frame()
  
  for (i in 1:length(nGramDfNames)) {
    N_input <- 4 - i
    
    if (getLengthOfWords(inputTxt) < N_input) {
      next} 
    
    else {
        lastNwords <- getLastNwords(inputTxt, N_input)
        df_temp<<-rbind(df_temp,filterNgrams(i,lastNwords))}
  }

  suggested_list<-data.frame() #This is the final output
  suggested_list<-rbind(suggested_list, df_temp[1:10,])
  final_df <- subset(df_temp, !(df_temp$words %in% stopwords()))
  suggested_list<-rbind(suggested_list, df_temp[1:10,])
  
  return(suggested_list)
  
} #end of getNextWordsSuggestion function


