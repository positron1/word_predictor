library(shiny)

shinyUI(
  navbarPage("",
    tabPanel("Use the App",
    
             h1("Next word predictor", 
                style = "font-family: 'Lobster', cursive;
                font-weight: 500; line-height: 1.1; 
                color: #4d3a7d;"),
             
    fluidRow(shinyjs::useShinyjs(),
      column(4,
             textInput("inputText", h3("Enter your text here: ",style = "font-family: 'Lobster', cursive;
                font-weight: 500; line-height: 1.1; color: #4d3a7d;"), value = "", width = NULL, placeholder = NULL),
             
             actionButton(
               inputId = "reset",
               label = h5("reset")),
             actionButton(
               inputId = "predict",
               label = h5("predict"))
             
             )
      
     
      
    ),  #end of fluidRow
    
     
    hr(),
    
    fluidRow(
      
      column(2,h4("Prediction 1"),
      
             tableOutput('topwords1')
      ),
      
      column(3,
             
             plotOutput('wordcloud1')
             
      ),
      
      column(2,h4("Prediction 2 (no stopwords)"), 
             
             tableOutput('topwords2')
             
      ),
      
      
      column(3,
             
             plotOutput('wordcloud2')
      )
   ) #end of fluidRow
    
   
    
  ),
  
  
  tabPanel("Tutorial",
           includeMarkdown("tutorial.rmd")
  )
  
  )
)
  