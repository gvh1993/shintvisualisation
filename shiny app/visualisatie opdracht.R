library('shiny')
library('ggplot2')
library('plyr')

ui <- fluidPage(
  titlePanel("popularity of games genres in time"),
  sidebarLayout(
    sidebarPanel(
      helpText("Filteroptions:"),
      
      sliderInput(inputId = "year",
                  label = "Year:",
                  value = min(ign$release_year), 
                  min = min(ign$release_year), 
                  max = max(ign$release_year),
                  step = 1)
    ),
  
  
  
  mainPanel(
    textOutput("selected"),
    plotOutput("genrePlot"),
    textOutput("source")
    )
  )
)

server <- function(input, output) {
  
  output$selected <- renderText({
    paste("you have selected:", input$year)
  })
  
  
  output$genrePlot <- reactivePlot(function(){
    year <- input$year
    plotdata = ign[ign$release_year == year,]
    
    #get frequency
    frequency <- count(plotdata, 'genre')
    p <- ggplot(frequency, aes(x = genre, 
                               y=freq)) + 
      geom_bar(stat = "identity") + 
      coord_flip()
    
    print(p)
  })
  
  
  
  
  
  output$source <- renderText({
    paste("data source: https://www.kaggle.com/egrinstein/20-years-of-games")
  })
}

shinyApp(server = server, ui = ui)

