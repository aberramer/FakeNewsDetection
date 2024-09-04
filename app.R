library(shiny)
library(readr)
library(tidyverse)
library(tidytext)
library(syuzhet)


ui <- fluidPage(
  titlePanel(""),
  mainPanel(
    tags$h2("Fake News Detection", style = "text-align:center;"),
    tags$hr(),
    tags$p("This app analyzes news articles and user-provided text to determine whether the content is likely to be 'real' or 'fake'.", style = "text-align:center;"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose file", accept = c(".csv", ".xlsx", ".xls", ".txt")),
        tags$hr(),
        helpText("Supported file types: .csv, .xlsx, .xls, .txt. File size should not exceed 10MB."),
        tags$hr(),
        textOutput("fileSize"),
        textAreaInput("userText", "Enter your news text:", "", rows = 5),
        actionButton("analyzeText", "Analyze Text")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Analysis",
                   plotOutput("exclamationPlot"),
                   plotOutput("questionMarkPlot"),
                   plotOutput("wordCountPlot")),
          tabPanel("News Summary",
                   verbatimTextOutput("newsSummary")),
          tabPanel("User Text Analysis",
                   verbatimTextOutput("userTextResult"))
        )
      )
    )
  )
)


server <- function(input, output) {
 
  fileData <- reactive({
    req(input$file)
    validate(
      need(input$file$size <= 10*1024^2, "File size exceeds 10MB. Please choose a smaller file.")
    )
    
    file_ext <- tools::file_ext(input$file$name)
    
    if (file_ext %in% c("csv", "txt")) {
      df <- read_csv(input$file$datapath)
    } else if (file_ext %in% c("xlsx", "xls")) {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Unsupported file type. Please choose a .csv, .xlsx, .xls, or .txt file.")
    }
    
    df
  })
  
 
  output$fileSize <- renderText({
    req(input$file)
    paste("File size:", round(input$file$size / 1024, 2), "KB")
  })
  
 
  output$newsSummary <- renderText({
    news <- fileData()
    total_count <- nrow(news)
    real_count <- sum(news$type == "real")
    fake_count <- sum(news$type == "fake")
    
    real_percent <- round((real_count / total_count) * 100, 2)
    fake_percent <- round((fake_count / total_count) * 100, 2)
    
    explanation <- if (real_percent > fake_percent) {
      "The dataset contains more articles classified as 'real' news."
    } else if (fake_percent > real_percent) {
      "The dataset contains more articles classified as 'fake' news."
    } else {
      "The dataset contains an equal number of 'real' and 'fake' news articles."
    }
    
    paste("News Type Summary:",
          "\n- Real News:", real_percent, "%",
          "\n- Fake News:", fake_percent, "%",
          "\n\nExplanation:\n", explanation)
  })
  
  
  output$exclamationPlot <- renderPlot({
    news <- fileData()
    news$exc <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))
    par(mar = c(4, 4, 4, 4)) 
    boxplot(exc ~ type, news, ylim = c(0, 20), ylab = "Exclamations", xlab = "Type", col = c("red", "orange"), main = "Exclamations by News Type")
  })
  
 
  output$questionMarkPlot <- renderPlot({
    news <- fileData()
    news$que <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))
    par(mar = c(4, 4, 4, 4)) 
    boxplot(que ~ type, news, ylim = c(0, 20), ylab = "Question Marks", xlab = "Type", col = c("red", "orange"), main = "Question Marks by News Type")
  })
  

  output$wordCountPlot <- renderPlot({
    news <- fileData()
    terms <- function(fake, text_column, group_column) {
      group_column <- enquo(group_column)
      text_column <- enquo(text_column)
      
      words <- fake %>%
        unnest_tokens(word, !!text_column) %>%
        count(!!group_column, word) %>%
        ungroup()
      
      return(words)
    }
    
    df <- terms(news, text, type)
    boxplot(n ~ type, df, log = "y", xlab = "Type", ylab = "Number of words", col = c("green", "pink"))
  })
  

  observeEvent(input$analyzeText, {
    user_text <- input$userText
    sentiment <- get_nrc_sentiment(user_text)
    positive_score <- sentiment$positive
    negative_score <- sentiment$negative
    
    total_score <- positive_score + negative_score
    reliability <- if (total_score > 0) {
      positive_score / total_score * 100
    } else {
      50
    }
    
    reliability_text <- if (positive_score > negative_score) {
      "Real"
    } else {
      "Fake"
    }
    
    output$userTextResult <- renderText({
      paste("The text is likely", reliability_text, "with a reliability of", round(reliability, 2), "%.")
    })
  })
}


shinyApp(ui = ui, server = server)
