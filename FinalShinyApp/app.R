library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)
library(leaflet)
library(readxl)
library(robotstxt)
library(rvest)
library(knitr)
library(pdftools)
library(janitor)
library(viridis)
library(ggnetwork)
library(igraph)
library(datasets)
library(mdsr)
library(gapminder)
library(maps)


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
coal_adjusted <- read_csv("coal_adjusted")
geothermal_adjusted <- read_csv("geothermal_adjusted")
hydroelectric_adjusted <- read_csv("hydroelectric_adjusted")
natural_gas_adjusted <- read_csv("natural_gas_adjusted")
nuclear_adjusted <- read_csv("nuclear_adjusted")
petroleum_adjusted <- read_csv("petroleum_adjusted")
solar_adjusted <- read_csv("solar_adjusted")
wind_adjusted <- read_csv("wind_adjusted")

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
##This is for Choropleths
state_co2 <- read_csv("state_co2_adjusted")
stateCO2_perCapita <- read_csv("stateCO2_perCapita")

usa_states <- map_data(map = "state"
                       , region = ".")
state_info <- data.frame(state_full = tolower(state.name)
                         , State = state.abb
                         , Region = state.region)


#for co2
state_co2 <- state_co2 %>%
    mutate(State = tolower(state.name))

state_co2_map <- state_co2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region")) %>%
    rename(Y1990 = "1990", Y2018 = "2018")

#for per capita
stateCO2_perCapita <- stateCO2_perCapita %>%
    mutate(State = tolower(state.name))
percapita_co2_map <- stateCO2_perCapita %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region")) %>%
    rename(Y1990 = "1990", Y2018 = "2018") 

#for geothermal
geothermal_adjusted2 <- geothermal_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

geo_map <- geothermal_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))


#for hydro
hydroelectric_adjusted2 <- hydroelectric_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

hydro_map <- hydroelectric_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

#for natural gas
natural_gas_adjusted2 <- natural_gas_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

ng_map <- natural_gas_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

#for coal
coal_adjusted2 <- coal_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

coal_map <- coal_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

#for nuclear
nuclear_adjusted2 <- nuclear_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

nuclear_map <- nuclear_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

#for petroleum
petroleum_adjusted2 <- petroleum_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

petroleum_map <- petroleum_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

#for solar
solar_adjusted2 <- solar_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

solar_map <- solar_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))


#for wind
wind_adjusted2 <- wind_adjusted %>%
    group_by(State) %>%
    summarize(Energy_Produced = sum(Energy_Produced)) %>%
    rename(state = State) %>%
    mutate(State = tolower(state))

wind_map <- wind_adjusted2 %>%
    left_join(state_info, by = "State") %>%
    right_join(usa_states, by = c("State" = "region"))

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
                 
                 
                 
                 
                 navbarMenu(
                     title= "Cloropleth Maps",
                 tabPanel(
                         title="Petroleum",
                         mainPanel(plotOutput(outputId = "petroleum_choro"), width = 12
                         )
                     ),
                 
                 tabPanel(
                     title="Natural Gas",
                     mainPanel(plotOutput(outputId = "ng_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title= "Coal",
                     mainPanel(plotOutput("coal_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = "Geothermal",
                     mainPanel(plotOutput(outputId = "geo_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = "Hydroelectric",
                     mainPanel(plotOutput(outputId = "hydro_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = "Nuclear",
                     mainPanel(plotOutput(outputId = "nuclear_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = "Wind",
                     mainPanel(plotOutput(outputId = "wind_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = "Solar",
                     mainPanel(plotOutput(outputId = "solar_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel(
                     title = " CO2 Emissions: State",
                     mainPanel(plotOutput(outputId = "state_co2_choro"), width = 12 
                     )
                 ),
                 
                 tabPanel("CO2 Emissions: Per Capita",
                          mainPanel(plotOutput(outputId = "percapita_choro"), width = 12 
                          )
                 )
            ),

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
    
    
    ##This is for the choropleth maps
    
    #Petroleum
    output$petroleum_choro <- renderPlot({
        ggplot(petroleum_map, aes(x = long, y = lat, group = group
                                  , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Petroleum Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Petroleum Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Geothermal
    output$geo_choro <- renderPlot({
        ggplot(geo_map, aes(x = long, y = lat, group = group
                            , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Geothermal Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Geothermal Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Hydroelectric
    output$hydro_choro <- renderPlot({
        ggplot(hydro_map, aes(x = long, y = lat, group = group
                              , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Hydroelectric Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Hydroelectric Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Natural Gas
    output$ng_choro <- renderPlot({
        ggplot(ng_map, aes(x = long, y = lat, group = group
                           , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Natural Gas Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Natural Gas Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
        
    })
    #Coal
    output$coal_choro <- renderPlot({
        ggplot(coal_map, aes(x = long, y = lat, group = group
                             , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Coal Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Coal Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Nuclear
    output$nuclear_choro <- renderPlot({
        ggplot(nuclear_map, aes(x = long, y = lat, group = group
                                , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Nuclear Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Nuclear Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Solar
    output$solar_choro <- renderPlot({
        ggplot(solar_map, aes(x = long, y = lat, group = group
                              , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Solar Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Solar Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Wind
    output$wind_choro <- renderPlot({
        ggplot(wind_map, aes(x = long, y = lat, group = group
                             , fill = Energy_Produced)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Wind Energy Produced by State", 
                 subtitle = "as of 2018",
                 fill = "Wind Energy Produced (Megawatts)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #Per Capita CO2
    output$percapita_choro <- renderPlot({
        ggplot(percapita_co2_map, aes(x = long, y = lat, group = group
                                      , fill = Y2018)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Per Capita Carbon Dioxide Emissions by State", 
                 subtitle = "as of 2018",
                 fill = "Per Capita Carbon Dioxide Emissions (Metric Tons)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
    })
    #CO2 Emissions
    output$state_co2_choro <- renderPlot({
        ggplot(state_co2_map, aes(x = long, y = lat, group = group
                                  , fill = Y2018)) +
            geom_polygon(color = "white") +
            theme_void() +
            coord_fixed(ratio = 1.3) +
            labs(title = "Carbon Dioxide Emissions by State", 
                 subtitle = "as of 2018",
                 fill = "Carbon Dioxide Emissions (Million Metric Tons)") +
            scale_fill_distiller(palette = "PuBu", direction = "horizantle")
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
