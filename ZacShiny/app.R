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




#For Choropleths
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
perCapita_co2_map <- stateCO2_perCapita %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("State" = "region")) %>%
  rename(Y1990 = "1990", Y2018 = "2018") 







ui <- navbarPage(theme = shinytheme("sandstone"),
                       setBackgroundColor(
                         color = c("#218766", "#adccb0 "),
                         gradient = "linear",
                         direction = "bottom"
                       ),
                       title="Energy and CO2 Production in the US",
navbarMenu("Choropleths",
           tabPanel(
             title="Petroleum",
               mainPanel(plotOutput(outputId = "state_renewable_lineplot"), width=8
               )
             ),
           
           tabPanel(
             title="Natural Gas",
             mainPanel(plotOutput(outputId = "US_renewable_lineplot")
                 )
             ),
               
           tabPanel(
             title= "Coal",
               mainPanel(
                 plotOutput("distPlot")
                 )
             ),
  
           tabPanel(
             title = "Geothermal",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel(
             title = "Hydroelectric",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel(
             title = "Nuclear",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel(
             title = "Wind",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel(
             title = "Solar",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel(
             title = " CO2 Emissions: State",
             mainPanel(plotOutput(outputId = "state_co2_plot"), width =8
             )
           ),
           
           tabPanel("CO2 Emissions: Per Capita",
              mainPanel(plotOutput(outputId = "percapita_plot"), width = 8
                      )
                    )
)
)
           


server <- function(input, output) {
  
  
  
  
  
  
  
  
  
  
}




shinyApp(ui = ui, server = server)