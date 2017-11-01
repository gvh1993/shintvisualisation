library('shiny')
library('ggplot2')
library('plyr')

#get the data
library(readr)
ign <- read_csv("ign.csv")

#remove outlier from 1970
ign<-ign[!(ign$release_year==1970),]


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
  
  
  output$genrePlot <- renderPlot({
    year <- input$year
    plotdata = ign[ign$release_year == year,]
    title = paste("Popularity of games genres in", input$year, sep = " ")
    
    #get frequency
    frequency <- count(plotdata, 'genre')
    p <- ggplot(frequency, aes(x = genre, y=freq)) + 
      geom_bar(stat = "identity", fill = "#FF6666") + 
      coord_flip() +
      labs(y = "Frequency", x = "Genre") +
      ggtitle(title) +
      scale_fill_brewer(palette="Set1") +
      theme(
      plot.title = element_text(color="red", size=14, face="bold"),
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")
      )
      
    p
  })
  
  
  
  
  
  output$source <- renderText({
    paste("data source: https://www.kaggle.com/egrinstein/20-years-of-games")
  })
}

shinyApp(server = server, ui = ui)

