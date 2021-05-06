library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)

#Data to shiny app
state_energy_consumption<-state_energy_consumption


ui <- fluidPage(
  # Application title
  titlePanel("Total energy production per powerplant type"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Select Name
      selectInput(inputId = "st", 
                  label = "State:",
                  choices = unique(state_energy_consumption$State),
                  selected = "Delaware",
                  multiple = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    dat <- state_energy_consumption %>%
      filter(State %in% input$st)
    
    ggplot(data = dat, 
      aes(x = energy_type, y = total_energy)) +
      geom_bar(stat = "identity") + 
      labs(x = "Energy type", y = "Total energy production"
           , title = paste("This is energy production for", input$st))
    
  })
}

shinyApp(ui = ui, server = server)



