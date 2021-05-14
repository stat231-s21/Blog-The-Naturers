library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)
library(leaflet)

##Team Contributions
## Lorraine was responsible for Energy Consumption Section
## Zac was responsible for the CO2 Emission Section
##Graham was responsible for the Energy Production Section


##getting our data into Shiny App
state_co2_adjusted<-read_csv("state_co2_adjusted")
stateCO2_perCapita<-read_csv("stateCO2_perCapita")
state_renewables<-read_csv("state_renewables")
US_renewables<-read_csv("US_renewables")
state_energy_consumption<-read_csv("state_energy_consumption")
energy<-read_csv("state_energy_consumption")
state_co2_2 <- read.csv("state_co2_adjusted2")
stateCO2_perCapita2<-read_csv("stateCO2_perCapita2")
new_energy<- read_csv("combined_energy_locations") 

#This for Lorraines plot
state_energy_consumption<- new_energy %>%
    group_by(State, energy_type)%>%
    summarise(
        total_energy=sum(Energy_Produced)
    )

Energy_type_per_state <- new_energy %>%
    group_by(State, energy_type) %>%
    summarise(
        no_powerplant_type=n()
    )



##This is necessary for renewables section of app
US_renewables_col_names<-colnames(US_renewables)
state_renewables_col_names<-colnames(state_renewables)

line_var_values<-c("coal", "natural_gas", "crude_oil", "nuclear", "biofuels","wood_waste", "other", "total")
line_var_names<-c("Coal", "Natural Gas", "Crude Oil", "Nuclear", "Biofuels","Wood Waste", "Other Renewables", "Total" )
names(line_var_values) <- line_var_names

### This for US renewables section of app
US_line_var_values<-c("Total","Coal", "Petroleum_Liquids", "Petroleum_Coke", "Natural_Gas", "Other_Gases","Nuclear", "Hydroelectric", "Other_Renewables","Hydroelectric_pumped", "Other")
US_line_var_names<-c("Total", "Coal", "Petroleum Liquids", "Petroleum Coke", "Natural Gas", "Other Gases","Nuclear", "Hydroelectric", "Other Renewables","Hydroelectric Pumped", "Other" )
names(US_line_var_values) <- US_line_var_names

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"),
                 setBackgroundColor(
                     color = c("#218766", "#adccb0 "),
                     gradient = "linear",
                     direction = "bottom"
                 ),
                 title="Energy and CO2 Production in the US",
                 navbarMenu("Line Plot",
                            tabPanel(
                                title="State Energy Production",
                                titlePanel("State Energy Production Estimates (1960-2018)"),
                                hr(),
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(inputId = "state_renewables_line_vars"
                                                    , label = "Choose a variable of interest to plot:"
                                                    , choices = line_var_values
                                                    , selected = "total"),
                                        selectizeInput(inputId = "state_renewables_line_state"
                                                       , label = "Choose one or more States:"
                                                       , choices = unique(state_renewables$state)
                                                       , selected = FALSE
                                                       , multiple = TRUE),
                                        selectizeInput(inputId = "state_renewables_line_regions"
                                                       , label = "Choose one or more regions:"
                                                       , choices = unique(state_renewables$Region)
                                                       , selected = c("New England"),
                                                       , multiple = TRUE),
                                        sliderInput(inputId = "state_renewables_line_years", 
                                                    label = "Choose Years", 
                                                    value =c(1960,2018) , min = 1960, max = 2018,sep=""),
                                        
                                        width= 4
                                    ),
                                    
                                    mainPanel(
                                        plotOutput(outputId = "state_renewable_lineplot"), width=8
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                titlePanel("US Electricity Generation By Energy Source and Sector (2001-2020)"),
                                hr(),
                                title="US Electricity Generation",
                                sidebarLayout(
                                    sidebarPanel(
                                        selectizeInput(inputId = "US_renewables_line_industry"
                                                       , label = "Choose one or more Industries:"
                                                       , choices = unique(US_renewables$Type)
                                                       , selected = c("All", "Industrial", "Commercial", "Electric"),
                                                       , multiple = TRUE),
                                        selectInput(inputId = "US_renewables_line_vars"
                                                    , label = "Choose a variable of interest to plot:"
                                                    , choices = US_line_var_values
                                                    , selected = "Total"),
                                        sliderInput(inputId = "US_renewables_line_years", 
                                                    label = "Choose Years", 
                                                    value =c(2001,2020) , min = 2001, max = 2020,sep=""),
                                        
                                    ),
                                    
                                    mainPanel(
                                        plotOutput(outputId = "US_renewable_lineplot")
                                    )
                                    
                                )
                                
                            ),
                            
                            tabPanel(
                                title= "Energy Consumption",
                                sidebarLayout(
                                    sidebarPanel(
                                        # Select Name
                                        selectInput(inputId = "st", 
                                                    label = "State:",
                                                    choices = unique(energy$state),
                                                    selected = c("California", "Oregon", "Washington"),
                                                    multiple = TRUE),
                                        sliderInput(inputId = "energy_years", 
                                                    label = "Choose Years", 
                                                    value =c(1997,2018) , min = 1997, max = 2018,sep="")
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        plotOutput("distPlotEnergy")
                                    )
                                )
                                
                            ),
                            tabPanel(
                                title = " CO2 Emissions: State",
                                sidebarLayout(
                                    sidebarPanel(
                                        selectizeInput(inputId = "state_co2_state"
                                                       , label = "Choose one or more States:"
                                                       , choices = unique(state_co2_2$state)
                                                       , selected = c("Oregon", "Washington", "California")
                                                       , multiple = TRUE),
                                        sliderInput(inputId = "state_co2_year", 
                                                    label = "Choose Years", 
                                                    value =c(1980,2018) , min = 1980, max = 2018,sep=""),
                                        width =4 ,
                                        
                                        
                                    ), 
                                    
                                    
                                    mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
                                    )
                                )
                            ),
                            
                            tabPanel("CO2 Emissions: Per Capita",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(inputId = "percapita_state"
                                                         , label = "Choose one or more States:"
                                                         , choices = unique(stateCO2_perCapita2$state)
                                                         , selected = c("Oregon", "Washington", "California")
                                                         , multiple = TRUE),
                                             sliderInput(inputId = "percapita_year", 
                                                         label = "Choose Years", 
                                                         value =c(1980,2018) , min = 1980, max = 2018,sep=""),
                                             width= 4
                                             
                                             
                                         ), 
                                         
                                         mainPanel(
                                             plotOutput(outputId = "percapita_plot"), width = 8
                                         )
                                         
                                     )
                            )
                 ),
                 
                 
                 
                 
                 tabPanel(
                     title= "Cloropleth Maps"),
                 tabPanel(
                     title= "Leaflet Map",
                     sidebarLayout(
                         sidebarPanel(
                             # Select Energy Type to Look At 
                             selectizeInput(inputId = "energies"
                                            , label = "Choose one or more Energy Types:"
                                            , choices = unique(new_energy$energy_type)
                                            , selected = "Wind"
                                            , multiple = TRUE),
                             sliderInput(inputId = "energy_produced_leafet", 
                                         label = "Choose Amount of Energy Produced", 
                                         value =c(0,6765) , min = 0, max = 6765,sep="")
                             ),
                         
                        
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             leafletOutput("leafletPlot")
                         )
                         
                     ),
                     br(),
                     sidebarLayout(
                         sidebarPanel(
                             # Select Name
                             selectInput(inputId = "leaflet_state1", 
                                         label = "State:",
                                         choices = unique(state_energy_consumption$State),
                                         selected = "Delaware",
                                         multiple = FALSE)
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             plotOutput("distPlot1Leaflet", height = "300px")
                         )
                     ), 
                     br(),
                     sidebarLayout(
                         sidebarPanel(
                             # Select Name
                             selectInput(inputId = "leaflet_state2", 
                                         label = "State:",
                                         choices = unique(Energy_type_per_state$State),
                                         selected = "Delaware",
                                         multiple = FALSE)
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             plotOutput("distPlot2Leaflet")
                         )
                     )
                
                     )
                 

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ##This is for the Line Plots 
    ##For US_state renewable line plot
    
    data_for_state_renewable_lineplot<-reactive({
        data <- state_renewables %>%
            filter(state %in% input$state_renewables_line_state | Region %in% input$state_renewables_line_regions) %>%
            filter(year >= input$state_renewables_line_years[1] & year<=input$state_renewables_line_years[2])
    })
    
    output$state_renewable_lineplot<- renderPlot({ 
        ggplot(data=data_for_state_renewable_lineplot(),aes_string(x="year",y=input$state_renewables_line_vars, color="state", shape="Region")) +
            geom_point() +
            geom_line() +
            labs(x = "Year",
                 y =paste(line_var_names[line_var_values == input$state_renewables_line_vars],sep=" ", "Production (Trillion Btu)") ,
                 title = paste("Line Plot of Primary Energy Production Estimates of",line_var_names[line_var_values == input$state_renewables_line_vars] ),
                 subtitle= paste(input$state_renewables_line_years[1],"-", input$state_renewables_line_years[2])) 
        
        
    })
    
    #For US_total renewable line plot
    data_for_US_renewable_lineplot<-reactive({
        data <- US_renewables %>%
            filter(Type %in% input$US_renewables_line_industry) %>%
            filter(Year >= input$US_renewables_line_years[1] & Year <= input$US_renewables_line_years[2])
    })
    
    output$US_renewable_lineplot<- renderPlot({ 
        ggplot(data=data_for_US_renewable_lineplot(),aes_string(x="Year",y=input$US_renewables_line_vars, color="Type")) +
            geom_point() +
            geom_line() +
            labs(x = "Year",
                 y =paste(US_line_var_names[US_line_var_values == input$US_renewables_line_vars],sep=" ", "Production (Thousand Megawatthours)") ,
                 title = paste("Line Plot of US Electricity Generation By ",US_line_var_names[US_line_var_values == input$US_renewables_line_vars] ),
                 subtitle= paste(input$US_renewables_line_years[1],"-", input$US_renewables_line_years[2]),
                 color= "Sector") 
    })
    
    
    
    ## Plot for Energy Consumption by State
    output$distPlotEnergy<- renderPlot({

        dat <- energy %>% 
            filter(state %in% input$st) %>% 
            filter(year >= input$energy_years[1] & year <= input$energy_years[2] )


        ggplot(data = dat, aes(x = year, y = energy_intensity, color= state)) + 
        geom_line() + labs(x = "Year", y = "Total energy consumption
        (Thousand BTU per 2012$GDP)" , title = paste("Total Energy Consumption
        by State"))

    })
    ##Plot for co2 emissions by state
    data_for_co2_plot <- reactive({
        data <- state_co2_2 %>%
            filter(state %in% input$state_co2_state) %>%
            filter(year >= input$state_co2_year[1] & year<=input$state_co2_year[2])
    })
    output$state_co2_plot <- renderPlot({
        ggplot(data = data_for_co2_plot(), aes_string(x = "year", y = "co2_emissions", color = "state")) +
            geom_point() +
            geom_line() +
            labs(title = "Carbon Dioxide Emissions by State", x = "Year", y = "CO2 Emissions (million metric tons)")
        
        
        
    })
    ##Plot for co2 emissions per capita
    data_for_percapita_plot <- reactive({
        data <- stateCO2_perCapita2 %>%
            filter(state %in% input$percapita_state) %>%
            filter(year >= input$percapita_year[1] & year<=input$percapita_year[2])
    })
    output$percapita_plot <- renderPlot({
        ggplot(data = data_for_percapita_plot(), aes_string(x = "year", y = "co2_emissions", color = "state")) +
            geom_point() +
            geom_line() +
            labs(title = "Per Capita Carbon Dioxide Emissions by State", x = "Year", y = "CO2 Emissions (million metric tons)")
        
    })
    
    
    
    
    
    ##This is for the leaflet plot
    
    output$leafletPlot <- renderLeaflet({
        data <- new_energy %>% filter(energy_type %in% input$energies) %>%
            filter(Energy_Produced >= input$energy_produced_leafet[1] & Energy_Produced <= input$energy_produced_leafet[2])
        #This is for the leaflet plot
        energyIcons <- icons(
            iconUrl = case_when(
                data$energy_type == "Nuclear" ~"images/new_nuclear.png",
                data$energy_type == "Wind" ~"images/new_wind.png",
                data$energy_type == "Hydroelectric" ~"images/new_water.png",
                data$energy_type == "Solar" ~"images/new_sun.png",
                data$energy_type == "Geothermal" ~"images/new_geothermal.png",
                data$energy_type == "Coal" ~"images/new_coal.png",
                data$energy_type == "Natural_Gas" ~"images/new_gas.png",
                data$energy_type == "Petroleum" ~"images/new_petroleum.png"
            ),
            iconWidth = 10, iconHeight = 10,
        )
        
        leaflet() %>%
            addTiles()%>%
            addMarkers(lng = data$Longitude,
                       lat = data$Latitude, icon = energyIcons, popup = paste("Name: ",data$Power_Plant_Name,"<br>",
                                                                              "Location: ", data$City, ",", data$State, "<br>",
                                                                              "Energy Produced: ", data$Energy_Produced, "<br>",
                                                                              "Energy Type: ", data$energy_type )
            )
       
    })
    
    output$distPlot1Leaflet <- renderPlot({
        
        dat <- state_energy_consumption %>%
            filter(State %in% input$leaflet_state1)
        
        ggplot(data = dat, 
               aes(x = energy_type, y = total_energy)) +
            geom_bar(stat = "identity") + 
            labs(x = "Energy type", y = "Total energy production"
                 , title = paste("This is energy production for", input$leaflet_state1))
        
    })
    
    output$distPlot2Leaflet <- renderPlot({
        
        dat <- Energy_type_per_state %>%
            filter(State %in% input$leaflet_state2)
        
        ggplot(data = dat, 
               aes(x = energy_type, y = no_powerplant_type)) +
            geom_bar(stat = "identity") + 
            labs(x = "Energy type", y = "No. of powerplant"
                 , title = paste("This is the number of powerplants in ", input$leaflet_state2))
        
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
