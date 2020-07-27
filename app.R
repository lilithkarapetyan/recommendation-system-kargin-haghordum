library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(RCurl)
library(RecordLinkage)
library(wesanderson)
library(shinythemes)
library(rio)
library(readxl)
library(utf8)
library(purrr)
library(stringi)
library(stringr)
library(wesanderson)

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

searching_input<-function(kargin, input, input1, input2,input3 ){
  if(input%in%kargin$Age_limit){
    a0<-which(kargin$Age_limit==input)
  }else{
    a0<-which(kargin$Keywords=='other')
  }
  if (input1%in%kargin$Place){
    a1<-which(kargin$Place==input1)
    
  }else{
    a1<-which(kargin$Category=='other')
  }
  
  if(input2%in%kargin$Category){
    a2<-which(kargin$Category==input2)
  }else{
    a2<-which(kargin$Category=='other')
  }
  if(input3%in%kargin$Keywords){
    a3<-which(kargin$Keywords==input3)
  }else{
    a3<-which(kargin$Category=='other')
  }
  a<-c(a0, a1, a2, a3)
  
  a<-unique(a)
  a <- sapply(a, function(x){
      kargin$Url[x]
    })
  return(a)
}

download.file(url = "https://arcane-castle-95125.herokuapp.com/kargin.xlsx", destfile = 'kargin.xlsx', mode="wb")

data <- data.frame(read_excel("kargin.xlsx"))

data$KeywordsEng = str_remove_all(stri_trans_general(data$Keywords, "armenian-latin/bgn"), "’")

data$Title <- as_utf8(data$Title)
data$Category <- as_utf8(data$Category)
data$Keywords <- as_utf8(data$Keywords)


search(data, c("տուն"))

ui <- dashboardPage(skin='black',
  dashboardHeader(title="Kargin Project"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quiz", tabName = "Quiz", icon = icon("question")),
      menuItem("Search", tabName = "Search", icon = icon("search")),
      menuItem("Top Kargins", tabName = "Top", icon = icon("star")),
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
                box(htmlOutput('searchInput'), width = "100%"),
                box(htmlOutput('searchedVideos'), width = "100%")
              )
      ),
      tabItem(tabName = "Top",
              fluidRow(
                box(htmlOutput('topKargins'), width = "100%")
              )
      ),
      tabItem(tabName = "Analysis",
              
              fluidRow(
                plotOutput("moodViewsPlot"),
                plotOutput("ageViewsPlot"),
                plotOutput("viewsHist"),
              )
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
  
  ageViews <- setNames(aggregate(data[, 2:2], list(data$Age_limit), mean), c("AgeLimit", "AverageViews")) %>%
    mutate(prop = round(AverageViews, digits = 1)) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  output$ageViewsPlot <- renderPlot(
    ggplot(ageViews, aes(x="", y=AverageViews, fill=AgeLimit)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(y = ypos, label = prop), color = "white", size=6) +
      scale_fill_manual(values = wes_palette( "Darjeeling1"),  name = "Relationship status")  +
      labs(title = "Avergae Views By Age Limit")

  )
  moodViews <- setNames(aggregate(data[,2:2], list(data$Mood), mean), c("Mood", "AverageViews")) %>%
    mutate(prop = round(AverageViews, digits = 1)) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  output$moodViewsPlot <- renderPlot(
   ggplot(moodViews, aes(x="", y=AverageViews, fill=Mood)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(y = ypos, label = prop), color = "white", size=6) +
     scale_fill_manual(values = wes_palette( "Rushmore"),  name = "Relationship status")  +
     labs(title = "Avergae Views By Mood")
  )
  output$viewsHist <- renderPlot(
    hist(data$Views, xlab = "Views", main = "Histogram of Views")
  )
  
  output$searchedVideos <- renderUI({
    if(input$searchText == ''){
      return() 
    }

    searchOutput <- search(data, input$searchText)
    iframes <- as.list(sapply(searchOutput, function(x){ 
      paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    }))
    tags$div(HTML(paste(iframes, collapse = "")))
  })
  
  output$question <- renderUI({
    if(currQuiz() <= 3){
      list(
        radioButtons(inputId = paste0("q",currQuiz()), label = questions[currQuiz()],choiceValues = questionsValues[currQuiz(),], choiceNames = questionsLabels[currQuiz(),]),
        actionButton("do", "Next")
      )
    }
    else{
      searchData <- searching_input(data, "12+", "տուն", "harevanner", "gugo")
  
      iframes <- as.list(sapply(searchData,function(x){ 
            paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      }))
      tags$div(HTML(paste(iframes, collapse = "")))
    }
  })
  
  output$topKargins <- renderUI({
    tops <- head(data[order(-data$Views),], 20)
    urls <- unique(tops$Url)
    
    
    iframes <- as.list(sapply(urls,function(x){ 
      paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    }))
    tags$div(HTML(paste(iframes, collapse = "")))
  })
}

shinyApp(ui, server)