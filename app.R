library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(RCurl)
library(RecordLinkage)
library(wesanderson)
library(shinythemes)
library(xlsx)
library(rio)
library(readxl)
library(utf8)
library(purrr)
library(stringi)
library(stringr)

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
  data[order(unsorted,decreasing=T)[1:5],c('Url')]
}



searching_input<-function(kargin, input){
  
  if (input%in%kargin$Place){
    a<-which(kargin$Place==input)
    
  }else if(input%in%kargin$Category){
    a<-which(kargin$Category==input)
    
  }else if(input%in%kargin$Keywords){
    a<-which(kargin$Keywords==input)
    
  }else{
    a<-which(kargin$Category=="other")
  }
  
  for (variable in  a) {
    print(kargin$Url[variable])
  }
}



download.file(url = "https://arcane-castle-95125.herokuapp.com/kargin.xlsx", destfile = 'kargin.xlsx', mode="wb")

data <- data.frame(read_excel("kargin.xlsx"))
data$KeywordsEng = str_remove_all(stri_trans_general(data$Keywords, "armenian-latin/bgn"), "’")

data$Title <- as_utf8(data$Title)
data$Category <- as_utf8(data$Category)
data$Keywords <- as_utf8(data$Keywords)


searching_input(data, "տուն")

search(data, c("տուն"))

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
              fluidRow(
                box(htmlOutput('searchInput'), width = "100%")
              )
      ),
      tabItem(tabName = "Analysis",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {
  currQuiz <- reactiveVal(1)
  questions <- c('question 1', 'question 2', 'question3')
  questionsValues <- rbind(c('tun', 'office', 'hivandanoc'), c('tun1', 'office1', 'hivandanoc1'), c('tun2', 'office2', 'hivandanoc2'))
  questionsLabels <- rbind(c('tun', 'office', 'hivandanoc'), c('tun1', 'office1', 'hivandanoc1'), c('tun2', 'office2', 'hivandanoc2'))
  
  observeEvent(input$do, {
   newVal <- currQuiz()
   currQuiz(newVal+1)
  })
  
  output$searchInput <- renderUI({
    textInput(inputId = 'searchText', label = 'Search', placeholder = 'type...')
  })
  
  output$question <- renderUI({
    if(currQuiz() <= 3){
      list(
        radioButtons(inputId = paste0("q",currQuiz()), label = questions[currQuiz()],choiceValues = questionsValues[currQuiz(),], choiceNames = questionsLabels[currQuiz(),]),
        actionButton("do", "Next")
      )
    }
    else{
      searchData <- search(data, 'տուն')
  
      iframes <- as.list(sapply(searchData,function(x){ 
            paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      }))
      tags$div(HTML(paste(iframes, collapse = "")))
    }
  })
}

shinyApp(ui, server)