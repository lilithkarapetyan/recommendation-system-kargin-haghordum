library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(RCurl)
library(wesanderson)
library(shinythemes)
library(xlsx)
library(rio)
library(readxl)
library(utf8)
library(purrr)
library(stringi)
library(stringr)
library(dplyr)

getScore <- function(ref, words) {
  wordlist <- expand.grid(words = words, ref = ref, stringsAsFactors = FALSE)
  res <- wordlist %>% group_by(words) %>% mutate(match_score = jarowinkler(words, ref)) %>%
    summarise(match = match_score[which.max(match_score)])
  Reduce(sum, res[2:2], 0)
}


search <- function(data, words){
  unsorted <- apply(data, 1, function(row){ 
    getScore(unlist(strsplit(row[7], ";|,")),words) + getScore(unlist(strsplit(row[11], ";|,")),words)
  })
  data[order(unsorted,decreasing=T)[1:5],]
}

download.file(url = "https://arcane-castle-95125.herokuapp.com/kargin.xlsx", destfile = 'kargin.xlsx', mode="wb")

data <- data.frame(read_excel("kargin.xlsx"))
data$KeywordsEng = str_remove_all(stri_trans_general(data$Keywords, "armenian-latin/bgn"), "’")

data$Title <- as_utf8(data$Title)
data$Category <- as_utf8(data$Category)
data$Keywords <- as_utf8(data$Keywords)

ui <- dashboardPage(skin='black',
  dashboardHeader(title="Kargin Project"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quiz", tabName = "Quiz", icon = icon("question")),
      menuItem("Search", tabName = "Search", icon = icon("search")),
      menuItem("Data Analysis", tabName = "Analysis", icon = icon("chart-pie"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Quiz",
              fluidRow(
                box(htmlOutput('question'), width = "100%")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Search",
              h2("Widgets tab content")
      ),
      tabItem(tabName = "Analysis",
              h2("Widgets tab content")
      )
    )
  )
)



server <- function(input, output) {
  currQuiz <- reactiveVal(1)
  questionsValues <- rbind(c('tun', 'office', 'hivandanoc'), c('tun1', 'office1', 'hivandanoc1'), c('tun2', 'office2', 'hivandanoc2'))
  questionsLabels <- rbind(c('tun', 'office', 'hivandanoc'), c('tun1', 'office1', 'hivandanoc1'), c('tun2', 'office2', 'hivandanoc2'))
  questions <- c('question 1', 'question 2', 'question3')
  observeEvent(input$do, {
   newVal <- currQuiz()
   currQuiz(newVal+1)
  })
  
  output$question <- renderUI({
    if(currQuiz() <= 3){
      list(
        radioButtons(inputId = paste0("q",currQuiz()), label = questions[currQuiz()],choiceValues = questionsValues[currQuiz(),], choiceNames = questionsLabels[currQuiz(),]),
        actionButton("do", "Next")
      )
    }
    else{
      tags$div(
        list(
          tags$iframe(
            width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
          ),
          tags$iframe(
            width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
          ),
          tags$iframe(
            width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
          )
        )
      )
    }
  })
  
  output$videos <- renderUI({
    tags$div(
      list(
        tags$input(
          width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
        ),
        tags$iframe(
          width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
        ),
        tags$iframe(
          width="478", height="269", src="https://www.youtube.com/embed/gwu63_WO7O8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=T
        )
      )
    )
  })
}

shinyApp(ui, server)