
library(DT)
ui <- fluidPage(
  
  titlePanel("Capstone: Word Prediction Application"),
  sidebarLayout(
    sidebarPanel(
      
      p("Enter Words/Text and click \"Predict\" to see the next predicted words"),	
      textInput(inputId="text", label = ""),
      preditwords <- "",
      submitButton("Predict"),
      HTML('<script type="text/javascript"> 
           document.getElementById("text").focus();
           </script>')
      ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Word Prediction Results", 
                 htmlOutput("topword"), 
                 DT::dataTableOutput("pwordsdf1")
                 #conditionalPanel(
                 #p(" Goes here\n"),
                 # radioButtons("predicts","Top 6 Possible next words:",choices=c(""), inline = TRUE, width = '800px' )
                 # radioButtons("predicts","Top 6 Possible next words:",choices=c(""), inline = TRUE, width = '800px' )
                 #  submitButton("select", "Select One option"),
                 # condition = "input.text != ''",
                 # verbatimTextOutput("predicts"),
                 # verbatimTextOutput("cleaned"), 
                 #verbatimTextOutput("msg")
                 
                 #                 )
                 
        ),
        
        tabPanel("Behind The Scenes", 
                 verbatimTextOutput("text"),
                 verbatimTextOutput("cleaned"),
                 radioButtons("predicts","Top 6 Possible next words:",choices=c(""), inline = TRUE, width = '800px' ),
                 
                 # DT::dataTableOutput("pwordsdf1"),
                 verbatimTextOutput("iniTime"),
                 verbatimTextOutput("endTime"), 
                 verbatimTextOutput("msg") ),
                                                    
        tabPanel("Documentation", htmlOutput("blankline"), uiOutput("tab1"), htmlOutput("blankline1"),  uiOutput("tab2") ) 
 
              )
    )
      )
  
  )
