library(shinydashboard)
?icon
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