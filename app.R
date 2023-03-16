library(shiny)
library(dplyr)
library("tm")
library(wordcloud2)
library(wordcloud)
library(readxl)
library(rsconnect)

#setwd("D:/Projects/Wordcloud Generator")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Wordcloud Generator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      helpText("*Upload an excel file with text in 'textlower' column"),
      
      # Input: Select a file ----
      fileInput("file1", "Upload Brand Conversations File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".xlsx")),
      
      sliderInput("maxwords","Number of words to show:",
                  min = 1,  max = 500,  value = 150),
      
      sliderInput("textsize","Choose size of the text:",
                  min = 0,  max = 1,  value = 0.7, step=0.05),
      
      
      textAreaInput("omit", 
                    "Words to remove (separate with commas)", 
                    height = "50px", width="500px"),
      
      actionButton("show", "Show wordcloud")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      br(),
      br(),
      br(),
      wordcloud2Output('wordcloud',width="60%", height="300px"),
      downloadButton("downloadtable", "Download Freq", width="60%")
    )
  )
)


server <- function(input, output, session) {
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read_xlsx(inFile$datapath, sheet=1)
    data<-data%>%select(textlower)
    
    # Form a corpus from the text
    corpus = VCorpus(VectorSource(data))
    corpus = tm_map(corpus, removeWords, stopwords("english")) ## kill all stopwords from the corpus
    tdm = TermDocumentMatrix(corpus,
                             control = list(removePunctuation = TRUE,
                                            stopwords = c(stopwords("english")),
                                            removeNumbers = TRUE, tolower = TRUE))
    # define tdm as matrix
    m = as.matrix(tdm)
    # get word counts in decreasing order
    word_freqs = sort(rowSums(m), decreasing=TRUE) 
    # create a data frame with words and their frequencies
    whole = data.frame(word=names(word_freqs), freq=word_freqs)
    rownames(whole) <- NULL
    whole
    
    omitted <- input$omit %>%tolower() %>%strsplit("(\\s|,)+") %>% unlist()
    
    v<-whole
    v<- v[order(v$freq, decreasing = TRUE), ]
    v<-v%>%filter(!(word %in% omitted))
    v1<-v[1:input$maxwords, ]
    
  })
  
  
  observeEvent(input$show, {
    
    output$wordcloud = renderWordcloud2({
      
      wordcloud2(myData(),color = "random-dark", backgroundColor = "white", size=input$textsize, fontWeight = "normal",
                 fontFamily = 'cambria')
    })
    
  })
  
  output$downloadtable <-
    downloadHandler(
      filename = function () {
        paste("Word-Freq.csv", sep = "")
      },
      
      content = function(file) {
        write.csv(myData(), file)
      }
    )
  
} 

shinyApp(ui, server)

rsconnect::setAccountInfo(name='demoprojects',
                          token='4588A7EE79FC8E45D955E586DB026925',
                          secret='Dqph+nleyRRPGyoMk+iv5snsQwI5LbKNxQtEgiok')

deployApp()


#https://demoprojects.shinyapps.io/wordcloud_generator/







