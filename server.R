library(shiny)
source("core.R")

server <- function(input, output, session) {
  
  output$text <- renderText({
    paste("Input text :", input$text)
  })
  
  
  observe({
    iniTime <- Sys.time()
    
   
    
    textCleansed <- clean(input$text)
    textCleansed <- trimws(textCleansed, "b")
    if(textCleansed != " ") 
    {
      output$cleaned <- renderText({
        paste0("Cleaned Text: [",textCleansed, " ", "",  "]")
      })
      
      
      url1 <- a("Contains the RPub Information to view the PresentationL", href="https://www.google.com/")
      url2 <- a("Contains the GitHub Repository Information ", href="https://www.google.com/")
      
      
      
      output$tab1 <- renderUI({
        tagList("Presentation Information:", url1)
      })
      
      output$blankline <- renderText({
        HTML(paste0(" <br>  </br>",  "</br>"))
      })
      
      output$blankline1 <- renderText({
        HTML(paste0(" <br>  </br>",  "</br>"))
      })
      
      output$tab2 <- renderUI({
        tagList("GitHub Package:", url2)
      })
      
      textCleansed <- gsub(" \\* ","",textCleansed)  
      
     
      predictWords <- ""
      if (textCleansed != "")
        predictWords <- predict_model(textCleansed)
      
      
      print("Predicted Words1")
      print(predictWords)
      
      if( grepl("Frequency",predictWords))
      {
        print("Cntains Frequncy and Proability")
      }
      else
          predictWords <- paste(predictWords, "~[Frequency: >100] ~[Probability: Not Applicable]", sep="")
      
      print("Predicted Words2")
      print(predictWords)
      pwordss<-unlist(strsplit(predictWords,  "\\~" ))
      pwordss<-data.frame(do.call(rbind, strsplit(predictWords,  "\\~" )))
      
      
      pwordss1 <-pwordss$X1
      print("Pwords 1st Column")
      print(pwordss1[1])
      #print(names(pwordss))
      
      #pwordsss <- unlist(strsplit(predictWords,"\\(", fixed = FALSE))
      
      #print("splitwords")
      print(typeof(pwordss))
      print(pwordss$X1)
      
      pwordsdf <-data.frame(predictWords)
      
      print("pwordsdf - Data Frame")
      print(pwordsdf)
      names(pwordsdf) <- c("Word")
      
     # if(pwordss1[1] == "")
     # {
     #   pwordss$X2 <- ""
     #   pwordss$X3 >- ""
     # }
      
      if(pwordss1[1] != "")
      {
      
      output$pwordsdf1 <-DT::renderDataTable({
          pwordss
        })
      }  
      output$pwords <- renderText({
       # paste0("Processed words: [",as.data.frame(predictWords), " ", "",  "]")
        paste(predictWords)
      })
      
      
      output$topword <- renderText({
        HTML(paste0(" <br>  </br>", "Predicted Next Word: ",  "<b>","<font size = 5, color=\"#FF0000\"><b>", pwordss1[1],"</b></font>", "</br>","<br>----------------------------------------------------------------------------------------------  </br>", "</br>", "Other Possible Words", "</br>"))
      })
      
      print("Print Words Only")
      print(typeof(pwordsdf))
      print(pwordsdf)
      print(predictWords)
      
      #updateSelectInput(session = session ,inputId = "predicts", choices = predictWords, selected = NULL)
      
      #updateSelectInput(session = session ,inputId = "predicts", choices = predictWords)
      
      updateRadioButtons(session = session ,inputId = "predicts", choices = predictWords, selected = NULL)
      #radioButtons("Test", inputId = "predicts", choices = predictWords, selected = NULL)
      
      print("Text")
      print(input$text)
      print("predicts")
      print(input$predicts)
      print(input$Select)
      
      if (input$predicts == "")
        y<-""
      else
        y <- input$predicts
      
      #selectInput("predict", "predicts", choices = predictWords
     
        if (input$text == "")
          x<- ""
        else
          x<- " "
      
       x <- paste(input$text)
        
       output$predicts1 <-renderDataTable(predictWords)
       
       updateTextInput(session = session,inputId =  "text", value =  x )
       
       
       predictWords <- ""
       # updateRadioButtons(session = session ,inputId = "predicts", choices = predictWords, selected = NULL)
       
       
      endTime <- Sys.time()
      output$iniTime <- renderText({
        paste("Start Time: ", as.character(iniTime))
      
      })
      
      output$endTime <- renderText({
        paste("End Time: ", as.character(endTime))
        
      })
     
      output$msg <- renderText({
        paste(msg, "\n", sprintf(" Time taken = %5.2f milliSeconds:",1000*(endTime-iniTime)))
      })
      gc()
    }  
  })
}

