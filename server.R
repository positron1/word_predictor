##Packages
library(shiny)
library(ggplot2)
library(wordcloud)
library(tm)
library(RWeka)

source("functions.R")


topwords<-function(nGrams,n)
{
  total<-sum(nGrams$freq)
  score<-nGrams$freq/total
  return(data.frame(word=nGrams$word[1:n],score=score[1:n]))
}


shinyServer(
  function(input, output) {
    
    set.seed(1234)
    
    variables <- reactiveValues(wordlist=NULL)
    
    plots<-reactiveValues(g1=NULL,g2=NULL,g3=NULL)
    
    ##initial output  
    output$topwords1 <- renderTable(topwords(uni_freq,10))
    output$topwords2 <- renderTable(topwords(uni_freq_ns,10))
    
    output$wordcloud1 <- renderPlot(
      wordcloud(uni_freq$word[1:10],uni_freq$freq[1:10], colors = brewer.pal(10,"Set3"),scale=c(6,2))
    )
    output$wordcloud2 <- renderPlot(
      wordcloud(uni_freq_ns$word[1:10],uni_freq_ns$freq[1:10], colors = brewer.pal(10,"Set3"),scale=c(6,2))
    )
    
    
    observeEvent(input$predict, {
      shinyjs::disable("predict")
      withProgress(message = 'Working on it...',
                   {variables$wordlist<-getNextWordsSuggestion(input$inputText)
                   })
  
      output$topwords1 <- renderTable(variables$wordlist[1:10,])
      output$topwords2 <- renderTable(variables$wordlist[11:20,])
      
      output$wordcloud1 <- renderPlot(
        wordcloud(na.omit(variables$wordlist$word[1:10]),na.omit(variables$wordlist$score[1:10]*10), colors = brewer.pal(10,"Set3"),scale=c(6,2))
      )
      output$wordcloud2 <- renderPlot(
        wordcloud(na.omit(variables$wordlist$word[11:20]),na.omit(variables$wordlist$score[11:20]*10), colors = brewer.pal(10,"Set3"),scale=c(6,2))
      )
      shinyjs::enable("predict")

    })
    
    
    
    observeEvent(input$reset, {
      updateTextInput(session, "inputText", value = "")
    })
  }
)