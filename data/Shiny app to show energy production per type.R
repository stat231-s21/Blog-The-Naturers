library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)

#Data to shiny app
combined <- read_csv("combined_energy_locations")
state_energy_consumption<- combined%>%
  group_by(State, energy_type)%>%
  summarise(
    total_energy=sum(Energy_Produced)
  )

#part 2 of shinyapp
Energy_type_per_state <- combined %>%
  group_by(State, energy_type) %>%
  summarise(
    no_powerplant_type=n()
  )

ui <- fluidPage(
  # Application title
  title = "Energy summary",
  
  tabPanel(
    title = "Total energy production per powerplant type",
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
),

tabPanel(
  title = "Total number of powerplant type in a state",
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Select Name
      selectInput(inputId = "st", 
                  label = "State:",
                  choices = unique(Energy_type_per_state $State),
                  selected = "Delaware",
                  multiple = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot2")
    )
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
  
  output$distPlot2 <- renderPlot({
    
    dat <- Energy_type_per_state %>%
      filter(State %in% input$st)
    
    ggplot(data = dat, 
           aes(x = energy_type, y = no_powerplant_type)) +
      geom_bar(stat = "identity") + 
      labs(x = "Energy type", y = "No. of powerplant"
           , title = paste("This is the number of powerplants in ", input$st))
    
  })
}

shinyApp(ui = ui, server = server)



