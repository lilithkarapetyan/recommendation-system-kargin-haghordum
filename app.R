library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(rvest)
library(RCurl)
library(ggplot2)
library(httr)
library(devtools)
library(martirossaryan)
library(wesanderson)
library(RColorBrewer)
library(colourpicker)
library(shinythemes)
library(readr)
library(xlsx)
library(rio)
library(readxl)
library(utf8)
library(purrr)
library(stringi)
library(stringr)
library(RecordLinkage)
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
  dashboardHeader(title = "Kargin Recommendation System"),
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
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
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
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)