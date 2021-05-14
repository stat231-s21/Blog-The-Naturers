library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(shinybusy)
library(ggrepel)


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




ui <- navbarPage(theme = shinytheme("sandstone"),
                 setBackgroundColor(
                     color = c("#218766", "#adccb0 "),
                     gradient = "linear",
                     direction = "bottom"
                 ),
                 
                 title="Energy-Related CO2 Emission in the US",
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
                             plotOutput("distPlot")
                         )
                     )
                     
                 ),
                 navbarMenu("CO2 Emissions",
                            tabPanel(
                                title = "State",
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
                            
                            tabPanel("Per Capita",
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
                 navbarMenu("Energy Production",
                            tabPanel(
                                title= "State Energy Production",
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel(
                                                    title="Line Plot",
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
                                                    title = "Table",
                                                    titlePanel("State Energy Production Estimates (1960-2018)"),
                                                    hr(),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            selectizeInput(inputId = "state_renewable_col"
                                                                           , label = "Choose Columns"
                                                                           , choices = state_renewables_col_names
                                                                           , selected = state_renewables_col_names
                                                                           , multiple = TRUE),
                                                            
                                                            selectizeInput(inputId = "state_renewables_states"
                                                                           , label = "Choose one or more States:"
                                                                           , choices = unique(state_renewables$state)
                                                                           , selected = c("New Hampshire")
                                                                           , multiple = TRUE),
                                                            selectizeInput(inputId = "state_renewables_table_regions"
                                                                           , label = "Choose one or more regions:"
                                                                           , choices = unique(state_renewables$Region)
                                                                           , selected = c("New England"),
                                                                           , multiple = TRUE),
                                                            selectizeInput(inputId = "state_renewables_year"
                                                                           , label = "Choose one or more Years:"
                                                                           , choices = unique(state_renewables$year)
                                                                           , selected = c(1960,2018)
                                                                           , multiple = TRUE),
                                                            width=5
                                                        ),
                                                        mainPanel(
                                                            DT::dataTableOutput(outputId = "state_renewables_table"), width=5
                                                        )
                                                    )
                                                ),
                                                tabPanel(
                                                    title="Scatter Plot",
                                                    titlePanel("State Energy Production Estimates (1960-2018)"),
                                                    hr(),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            selectInput('state_renewables_scatter_x', 'X Variable', choices=line_var_values, selected ="coal"),
                                                            selectInput('state_renewables_scatter_y', 'Y Variable', choices=line_var_values, selected = "nuclear"),
                                                            selectizeInput(inputId = "state_renewables_scatter_years"
                                                                           , label = "Choose one or more Years:"
                                                                           , choices = unique(state_renewables$year)
                                                                           , selected = c(2018)
                                                                           , multiple = TRUE),
                                                            selectizeInput(inputId = "state_renewables_scatter_state"
                                                                           , label = "Identify State(s) in the scatterplot:"
                                                                           , choices = unique(state_renewables$state)
                                                                           , selected = c('Wyoming', "Pennslyvania")
                                                                           , multiple = TRUE),
                                                        ), 
                                                        mainPanel(
                                                            plotOutput(outputId = "state_renewables_scatter")
                                                        )
                                                    )
                                                    
                                                )
                                                
                                                
                                    )
                                )
                            ),
                            tabPanel(
                                title= "US Electricity Generation",
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel(
                                                    titlePanel("US Electricity Generation By Energy Source and Sector (2001-2020)"),
                                                    hr(),
                                                    title="Line Plot",
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
                                                    title = "Table",
                                                    titlePanel("US Electricity Generation By Energy Source and Sector (2001-2020)"),
                                                    hr(),
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            selectizeInput(inputId = "US_industry_col"
                                                                           , label = "Choose Columns"
                                                                           , choices = US_renewables_col_names
                                                                           , selected = US_renewables_col_names
                                                                           , multiple = TRUE),
                                                            
                                                            selectizeInput(inputId = "US_industry_type"
                                                                           , label = "Choose one or more Sector Types:"
                                                                           , choices = US_renewables$Type
                                                                           , selected = c("All", "Industrial", "Commercial", "Electric")
                                                                           , multiple = TRUE), 
                                                            selectizeInput(inputId = "US_industry_year"
                                                                           , label = "Choose one or more Years:"
                                                                           , choices = unique(US_renewables$Year)
                                                                           , selected = c(2001,2020)
                                                                           , multiple = TRUE),
                                                            width=5
                                                        ),
                                                        mainPanel(
                                                            DT::dataTableOutput(outputId = "US_renewables_table"), width=5
                                                        )
                                                    )
                                                )
                                            
                                                # tabPanel(
                                                #   title="Scatter Plot",
                                                #   sidebarLayout(
                                                #     sidebarPanel(
                                                #       selectInput('US_renewables_scatter_x', 'X Variable', choices=US_line_var_values, selected ="Coal"),
                                                #       selectInput('US_renewables_scatter_y', 'Y Variable', choices=US_line_var_values, selected = "Other"),
                                                #       sliderInput(inputId = "US_renewables_scatter_years",
                                                #                   label = "Choose Years",
                                                #                   value =c(2001,2020) , min = 2001, max = 2020,sep=""),
                                                # 
                                                #     ),
                                                #     mainPanel(
                                                #       plotOutput(outputId = "US_renewables_scatter")
                                                #     )
                                                #   )
                                                # 
                                                # )
                                                
                                                
                                                
                                    )
                                )
                                
                            )
                            
                            
                 )
)



server <- function(input,output){
    
    
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
    
    
    
    ##For US_state renewable energy table
    data_for_state_renewables_table <- reactive({
        data <- state_renewables %>%
            filter(state %in% input$state_renewables_states | Region %in% input$state_renewables_table_regions) %>%
            filter(year %in% input$state_renewables_year) %>%
            select(input$state_renewable_col)
    })
    
    output$state_renewables_table <- DT::renderDataTable({ 
        data_for_state_renewables_table()
    })
    
    ##For US_country renewable energy table
    data_for_US_renewables_table <- reactive({
        data <- US_renewables %>%
            filter(Type %in% input$US_industry_type) %>%
            filter(Year %in% input$US_industry_year) %>%
            select(input$US_industry_col)
    })
    
    
    output$US_renewables_table <- DT::renderDataTable({ 
        data_for_US_renewables_table()
    })
    
    ##For state renewable scatterplot
    data_for_state_renewables_scatter<- reactive({
        data <- state_renewables %>%
            filter(year %in% input$state_renewables_scatter_years)
        
    })
    
    output$state_renewables_scatter<- renderPlot({ 
        ggplot(data=data_for_state_renewables_scatter(), aes_string(x=input$state_renewables_scatter_x, y=input$state_renewables_scatter_y, color="Division")) +
            geom_point() +
            labs(x = paste(line_var_names[line_var_values == input$state_renewables_scatter_x], sep=" ", "Production (Trillion Btu)"),
                 y = paste(line_var_names[line_var_values == input$state_renewables_scatter_y],sep=" ", "Production (Trillion Btu)"),
                 color = "Region",
                 title = paste("Scatter Plot of Primary Energy Production Estimates of",line_var_names[line_var_values == input$state_renewables_scatter_x], "vs", line_var_names[line_var_values == input$state_renewables_scatter_y])
            ) +
            geom_label_repel(data = filter(data_for_state_renewables_scatter(), state %in% input$state_renewables_scatter_state)
                             , aes(label = state), show.legend = FALSE)
    })
    
    ##For US renewable scatterplot
    data_for_US_renewables_scatter<- reactive({
        data <- US_renewables %>%
            filter(Year >= input$US_renewables_scatter_years[1] & Year <= input$US_renewables_scatter_years[2])
    })
    
    output$US_renewables_scatter<- renderPlot({ 
        ggplot(data=data_for_US_renewables_scatter(), aes_string(x=input$US_renewables_scatter_x, y=input$US_renewables_scatter_y, color="Type")) +
            geom_point() +
            labs(x = paste(US_line_var_names[US_line_var_values == input$US_renewables_scatter_x], sep=" ", "Production (Thousand Megawatthours)"),
                 y = paste(US_line_var_names[US_line_var_values == input$US_renewables_scatter_y],sep=" ", "Production (Thousand Megawatthours)"),
                 color = "Sector",
                 title = paste("Scatter Plot of Electricity Generation of",line_var_names[line_var_values == input$state_renewables_scatter_x], "vs", line_var_names[line_var_values == input$state_renewables_scatter_y])
            )
        
    })
    
    output$distPlot <- renderPlot({
        
        dat <- energy %>%
            filter(state %in% input$st) %>%
            filter(year >= input$energy_years[1] & year <= input$energy_years[2] )
    
        
        ggplot(data = dat, aes(x = year, y = energy_intensity, color=state)) +
            geom_line() + 
            labs(x = "Year", y = "Total energy consumption (Thousand BTU per 2012$GDP)"
                 , title = paste("Total Energy Consumption by State"))
        
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
    
}

shinyApp(ui = ui, server = server)
