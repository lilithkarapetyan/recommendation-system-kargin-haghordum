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

searching_input<-function(kargin, input, input1, input2,input3, input4 ){
  kargin<-kargin%>%filter(Age_limit==input)

 
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
  if(input4%in%kargin$Place){
    a4<-which(kargin$Place==input4)
  }else{
    a4<-which(kargin$Category=='other')
  }
  a<-c( a1, a2, a3, a4)
  
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


ui <- dashboardPage(skin='black',
  dashboardHeader(title="Kargin Project"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quiz", tabName = "Quiz", icon = icon("question")),
      menuItem("Search", tabName = "Search", icon = icon("search")),
      menuItem("Top Kargins", tabName = "Top", icon = icon("star")),
      menuItem("Random Kargins", tabName = "Random", icon = icon("random")),
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
      tabItem(tabName = "Random",
              fluidRow(
                box(htmlOutput('randomKargins'), width = "100%")
              )
      ),
      tabItem(tabName = "Analysis",
              
              fluidRow(
                plotOutput("moodViewsPlot"),
                plotOutput("ageViewsPlot"),
                plotOutput("viewsHist"),
                plotOutput("placePlot")
              )
      )
    )
  )
)


server <- function(input, output) {
  currQuiz <- reactiveVal(1)
  questions <- c('Please,specify your age!',
                 'Where do you like to spend you free time?',
                 'With whom do you like to spend most of your time?',
                 'Which one?',
                 'Where did you got acquainted with your Best Friend?')
  questionsValues <- rbind(c('12+', '16+', '18+'),
                           c('գյուղ', 'ռեստորան', 'տուն'), 
                           c('ընկերներ', 'կին', 'տնօրեն'),
                           c('ավտո', 'տաքսի', 'տրանսպորտ'),
                           c('բակ', 'օֆիս', 'խանութ'))
  questionsLabels <- rbind(c('12-16', '16-18', '18+'), 
                           c('in the nature', 'restaurant', 'at home'), 
                           c('friends', 'family', 'collegues'),
                           c('Personal Car', 'Taxi', 'Public Transport'),
                           c('Outdoors', 'Workplace', 'At the Store'))
  
  observeEvent(input$do, {
   newVal <- currQuiz()
   currQuiz(newVal+1)
  })
  
  output$searchInput <- renderUI({
    textInput(inputId = 'searchText', label = 'Search', placeholder = 'type...')
  })
  
  ageViews <- setNames(aggregate(data[, 2:2], list(data$Age_limit), sum), c("AgeLimit", "Views"))
  
  output$ageViewsPlot <- renderPlot(
    ggplot(ageViews, aes(x=1, y=Views, fill=AgeLimit)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(y = Views, label = Views), color = "white", position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = wes_palette( "Darjeeling1"),  name = "Relationship status")  +
      labs(title = "Total Views By Age Limit")

  )
  moodViews <- setNames(aggregate(data[,2:2], list(data$Mood), sum), c("Mood", "Views"))
  
  output$moodViewsPlot <- renderPlot(
   ggplot(moodViews, aes(x="", y=Views, fill=Mood)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void() +
     geom_text(aes(y = Views, label = Views), color = "white", position = position_stack(vjust = 0.5)) +
     scale_fill_manual(values = wes_palette( "Rushmore"),  name = "Relationship status")  +
     labs(title = "Total Views By Mood")
  )
  options(scipen=999)
  output$viewsHist <- renderPlot(
    ggplot(data) + 
      geom_histogram(aes(x=Views, position = "identity"), fill = wes_palette( "Rushmore")[3])+
      theme_bw()
  )
  
  
  output$placePlot <- renderPlot({
    place_sub<-data$Place
    place_sub<-str_replace_all(place_sub, "rr", "r")
    place_sub<-str_remove_all(place_sub,"\\’")
    place_sub<-str_replace_all(place_sub, pattern = "\\;", replacement = "  ")
    place_sub<-str_replace_all(place_sub, pattern = "\\,", replacement = "  ")
    place_sub<-lapply(place_sub, tolower)
    place_sub<-unlist(place_sub)
    place_sub<-strsplit(place_sub, "\\s")
    place_sub<-place_sub[place_sub!=""]
    a<-c()
    a<-place_sub
    place_sub<-a
    place_sub<-unlist(place_sub)
    
    unique_places<-unique(place_sub)
    unique_places
    place_sub<-unlist(lapply(place_sub, setdiff, ''))
    place_sub<-as.data.frame(place_sub)
    
    place_sub <- place_sub %>% 
    group_by(place_sub) %>%
    summarise(Count=n()) %>% arrange(desc(Count)) %>% head(5)
    place_sub$place_sub <- factor(place_sub$place_sub, levels=place_sub$place_sub[order(place_sub$Count)])
    place_sub %>%  
      ggplot(aes(x = place_sub, y = Count, fill=place_sub)) +
      geom_bar(stat ="identity",width = 1, colour = "black", position=position_dodge())+
      coord_flip()+
      theme(legend.position="none",
            axis.text.x = element_text(angle=90),
            plot.title = element_text(size=15,hjust=0.5))+
      labs(x="Count",y="Most Repeated Words",title="Top 10 Most Common places of Kargins")+
      scale_fill_manual(values = wes_palette( "Darjeeling1"),  name = "Relationship status")
  })
  
  output$searchedVideos <- renderUI({
    if(input$searchText == '' || is.na(input$searchText)){
      return() 
    }

    searchOutput <- search(data, input$searchText)
    iframes <- as.list(sapply(searchOutput, function(x){ 
      paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    }))
    tags$div(HTML(paste(iframes, collapse = "")))
  })
  
  output$question <- renderUI({
    if(currQuiz() <= 5){
      list(
        radioButtons(inputId = paste0("q",currQuiz()), label = questions[currQuiz()],choiceValues = questionsValues[currQuiz(),], choiceNames = questionsLabels[currQuiz(),]),
        actionButton("do", "Next")
      )
    }
    else{
      searchData <- searching_input(data, input$q1, input$q2, input$q3, input$q4, input$q5)
  
      iframes <- as.list(sapply(searchData,function(x){ 
            paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      }))
      tags$div(HTML(paste(iframes, collapse = "")))
    }
  })
  
  
  output$randomKargins <- renderUI({
    urls <- unique(data[sample(1:length(data[,1]), 6, replace=F),]$Url)
    
    iframes <- as.list(sapply(urls,function(x){ 
      paste0('<iframe width="478" height="269" src="https://www.youtube.com/embed/', str_extract(x, '.{11}$') , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    }))
    tags$div(HTML(paste(iframes, collapse = "")))
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